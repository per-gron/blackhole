;==============================================================================

; File: "http.scm", Time-stamp: <2007-04-04 14:42:59 feeley>

; Copyright (c) 2005-2007 by Marc Feeley, All Rights Reserved.

;==============================================================================

;; TODO The server doesn't give a 400 error when given a HTTP/1.1
;;      request without a Host: header.

(import ../misc/token-table
        ../misc/al
        ../string/sxml-to-xml
        ../string/util
        ../srfi/13
        ../srfi/19
        ../srfi/1
        uri
        http-common
        x-www-form-urlencoded)

(export make-server
        server?
        server-port-number
        server-port-number-set!
        server-timeout
        server-timeout-set!
        server-threaded?
        server-threaded?-set!
        server-handler-function
        server-handler-function-set!
        
        make-request
        request?
        request-server
        request-server-set!
        request-connection
        request-connection-set!
        request-method
        request-method-set!
        request-uri
        request-uri-set!
        request-version
        request-version-set!
        request-attributes
        request-attributes-set!
        request-query
        request-query-set!
        request-cookies
        request-cookies-set!
        request-cookie-changes
        request-cookie-changes-set!
        current-request
        request-get-query
        request-post-query
        make-http-server
        http-server-start!
        
        cookie-set!
        cookie-ref
        cookie-del!
        cookie-get
        
        method-not-implemented-error
        internal-server-error
        unimplemented-method
        bad-request-error
        
        display-headers-reply
        reply-chunk
        reply
        reply-xml
        http-redirect)

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  ;;(not safe)
  )

;==============================================================================

; HTTP server.

(define-type server
  id: c69165bd-c13f-11d9-830f-00039301ba52

  port-number
  timeout
  threaded?
  handler-function)

(define-type request
  id: 8e66862f-c143-11d9-9f4e-00039301ba52

  (server unprintable:)
  connection
  method
  uri
  version
  attributes
  query
  cookies
  cookie-changes)

(define (current-request)
  (thread-specific (current-thread))) ; request is stored in thread

(define (request-get-query request)
  (uri-query (request-uri request)))

(define (request-post-query request)
  (let ((q (request-query request)))
    (if (eq? q (request-get-query request))
        '()
        q)))

(define make-http-server
  (lambda (handler-function
           #!optional
           (port-number 80)
           #!key
           (timeout     300)
           (threaded?   #t))
    (make-server port-number
                 timeout
                 threaded?
                 handler-function)))

(define (http-server-start! hs)
  (let ((server-port
         (open-tcp-server
          (list port-number: (server-port-number hs)
                backlog: 128
                reuse-address: #t
                server-address: "*"
                ;; server-address: '#u8(127 0 0 1) ; on localhost interface only
                char-encoding: 'UTF-8))))
    (accept-connections hs server-port)))

(define (accept-connections hs server-port)
  (let loop ()
    (let ((connection (read server-port)))
      (if (server-threaded? hs)
          ;; Multithreaded mode.
          (let ((dummy-port (open-dummy)))
            (parameterize
             ((current-input-port dummy-port)
              (current-output-port dummy-port))
             (thread-start!
              (make-thread
               (lambda ()
                 (serve-connection hs connection))))))
          
          ;; Single-threaded mode.
          (serve-connection hs connection)))
    (loop)))

;==============================================================================

; Server specific cookie functions.

;; Lazily get the cookie changes table
(define (cookie-get-changes)
  (let ((req (current-request)))
    (or (request-cookie-changes req)
        (let ((tbl (make-table)))
          (request-cookie-changes-set! req tbl)
          tbl))))

;; Lazily get the cookie table
(define (cookie-get-cookies)
  (let ((req (current-request)))
    (or (request-cookies req)
        (let ((tbl (make-table)))
          (request-cookies-set! req tbl)
          tbl))))

(define (cookie-set! name value #!key expires domain path port secure http-only)
  (let ((chs (cookie-get-changes)))
    (table-set! chs name (make-cookie name
                                      value
                                      expires: expires
                                      domain: domain
                                      path: path
                                      port: port
                                      secure: secure
                                      http-only: http-only))))

(define (cookie-ref name)
  (let* ((chs (cookie-get-changes))
         (cs (cookie-get-cookies))
         (ch (table-ref chs name #f)))
    (or ch
        (table-ref cs name #f))))

(define (cookie-del! name)
  (cookie-set! name #f expires: date-in-the-past))

(define (cookie-get name)
  (let ((c (cookie-ref name)))
    (if c (cookie-value c) #f)))

;==============================================================================

; Error functions.

(define (send-error connection xml)
  (sxml>>xhtml-fast xml connection)
  (close-port connection))

(define (method-not-implemented-error connection)
  (send-error
   connection
   '(html (head (title "501 Method Not Implemented"))
          (body
           (h1 "Method Not Implemented")))))

(define (internal-server-error connection)
  (send-error
   connection
   '(html (head (title "500 Internal Server Error"))
          (body
           (h1 "Internal Server Error")))))

(define (unimplemented-method connection)
  (let* ((request (current-request))
         (connection (request-connection request)))
    (method-not-implemented-error connection)))

(define (bad-request-error connection)
  (send-error
   connection
   '(html (head (title "400 Bad Request"))
          (body
           (h1 "Bad Request")
           (p "Your browser sent a request that this server could "
              "not understand."
              (br))))))

;==============================================================================

; Reply functions.

(define (display-headers-reply port
                               #!optional
                               (hs '())
                               (protocol 'HTTP/1.1)
                               (status 200)
                               (charset "UTF-8")
                               cookie-table)
  (display-crlf port protocol " " status " " (http-status-code status))
  (display-headers port hs charset)
  (if cookie-table
      (cookie-headers cookie-table port))
  (display-crlf port))

(define hex "0123456789abcdef")

(define (hex-formatter num)
  (let ((gssymlen (string-length hex)))
    (reverse-list->string
     (let loop ((num num))
       (let ((idx (modulo num gssymlen)))
         (cons (string-ref hex idx)
               (if (eq? idx num)
                   '()
                   (loop (/ (- num idx) gssymlen)))))))))

(define (reply-chunk thunk #!key (code 200) (headers '()) (type "text/html"))
  (let ((conn (request-connection (current-request)))
        (version (request-version (current-request))))
    (if (or (eq? version 'HTTP/1.0)
            (eq? version 'HTTP/1.1))
        (let ((trailing-headers '()))
          (display-headers-reply
           conn
           (append headers
                   (al date (current-date 0)
                       transfer-encoding 'chunked
                       cache-control "no-cache=\"set-cookie\""
                       expires date-in-the-past
                       connection 'close))
           version
           code
           "UTF-8"
           (request-cookie-changes (current-request)))

          (thunk
           (lambda (inner-thunk #!optional (headers '()))
             (set! trailing-headers
                   (append trailing-headers
                           headers))
             (let* ((message
                     (with-output-to-u8vector
                      '(char-encoding: UTF-8
                        eol-encoding: cr-lf)
                      inner-thunk))
                    (len (u8vector-length message)))
               ;; We must not send a chunk of zero length, since that
               ;; means the end of the response.
               (if (not (zero? len))
                   (begin
                     (display-crlf conn (hex-formatter len))
                     (write-subu8vector
                      message
                      0
                      (u8vector-length message)
                      conn)
                     (display-crlf conn)
                     (force-output conn))))))
          
          ;; Send last chunk
          (display-crlf conn "0")
          
          ;; Send the trailing headers
          (for-each (lambda (x)
                      (display-header conn x))
                    trailing-headers)
          
          (display-crlf conn))
        (thunk
         (lambda (inner-thunk #!optional headers)
           (with-output-to-port conn inner-thunk))))
    (with-exception-catcher
     (lambda (e)
       #f ;; Silently ignore; chunked data is often sent over extended
          ;; periods of time, when the client is likely to close the
          ;; connection.
       )
     (lambda ()
       (close-port conn)))))

(define (reply thunk #!key (code 200) (headers '()) (type "text/html"))
  (let ((conn (lambda ()
                (request-connection (current-request))))
        (version (request-version (current-request))))
    (if (or (eq? version 'HTTP/1.0)
            (eq? version 'HTTP/1.1))
        (let ((message
               (with-output-to-u8vector
                '(char-encoding: UTF-8
                  eol-encoding: cr-lf)
                thunk)))
          (display-headers-reply
           (conn)
           (append (al date (current-date 0)
                       content-length (u8vector-length message)
                       content-type type
                       cache-control "no-cache=\"set-cookie\""
                       expires date-in-the-past
                       connection 'close)
                   headers)
           version
           code
           "UTF-8"
           (request-cookie-changes (current-request)))
          (write-subu8vector
           message
           0
           (u8vector-length message)
           (conn)))
        (with-output-to-port (conn) thunk))
    (close-port (conn))))

(define (reply-xml xml . rest)
  (apply reply
         (cons (lambda ()
                 (sxml>>xml-fast xml))
               rest)))

(define (http-redirect code location)
  (reply-xml
   `(html
     (head (title ,(http-status-code code)))
     (body
      (h1 ,(http-status-code code))
      (p ,(cond
           ((= code 301) "The page you seek has been permanenrly moved to")
           ((= code 302) "The page you seek is temporarily at")
           ((= code 303) "The response of the request can be found at")
           ((= code 307) "The page you seek is temporarily at")
           (else "[Unknown code]"))
         " "
         (a (@ (href ,location)) ,location))))
   code: code
   headers: (al location location)))

;------------------------------------------------------------------------------

; Low-level serving functions.

(define version-table
  (make-token-table
   ("HTTP/1.0" 'HTTP/1.0)
   ("HTTP/1.1" 'HTTP/1.1)))

(define (serve-connection hs connection)
  ;; Configure the connection with the client so that if we can't
  ;; read the request after 300 seconds, the read operation will fail
  ;; (and the thread will terminate).
  (input-port-timeout-set! connection (server-timeout hs))
  
  ;; Configure the connection with the client so that if we can't
  ;; write the response after 300 seconds, the write operation will
  ;; fail (and the thread will terminate).
  (output-port-timeout-set! connection (server-timeout hs))
  
  (let ((req (permissive-read-line connection)))
    (if (not (string? req))
        (bad-request-error connection)
        (begin
          (let* ((end
                  (let loop ((i 0))
                    (cond ((= i (string-length req))
                           #f)
                          ((char=? (string-ref req i) #\space)
                           i)
                          (else
                           (loop (+ i 1))))))
                 (method-name
                  (and end (substring req 0 end))))
            (if method-name
                (parse-uri
                 req
                 (+ end 1)
                 (string-length req)
                 #t
                 (lambda (uri i)
                   
                   (define (handle-version version)
                     (case version
                       ((HTTP/1.0 HTTP/1.1)
                        (let ((attributes (read-header connection)))
                          (if attributes
                              (handle-request version attributes)
                              (bad-request-error connection))))
                       ((#f)
                        ;; this is an HTTP/0.9 request
                        (handle-request 'HTTP/0.9 '()))
                       (else
                        (bad-request-error connection))))
                   
                   (define (handle-request version attributes)
                     (let* ((content (read-content-chars connection attributes))
                            (query
                             (let ((x (assoc "content-type" attributes)))
                               (if (and x
                                        ;; This is a hack for when the
                                        ;; content-type string
                                        ;; contains "; charset=blabla"
                                        (string-prefix?
                                         "application/x-www-form-urlencoded"
                                         (cdr x)))
                                   (decode-x-www-form-urlencoded content)
                                   (uri-query uri))))
                            (cookie-attr (assoc "cookie" attributes))
                            (cookies (if cookie-attr
                                         (cookie-parse (cdr cookie-attr))
                                         #f)))
                       (let ((request (make-request hs
                                                    connection
                                                    method-name
                                                    uri
                                                    version
                                                    attributes
                                                    query
                                                    cookies
                                                    #f)))
                         (thread-specific-set! (current-thread) request))
                       ;; Invoke handler (i.e. page generator)
                       (with-exception-catcher
                        (lambda (e)
                          (internal-server-error connection))
                        (lambda ()
                          ((server-handler-function hs))))))
                   
                   (cond ((not uri)
                          (bad-request-error connection))
                         ((not (< i (string-length req)))
                          (handle-version #f))
                         ((not (char=? (string-ref req i) #\space))
                          (bad-request-error connection))
                         (else
                          (let ((version-index
                                 (token-table-lookup-substring
                                  version-table
                                  req
                                  (+ i 1)
                                  (string-length req))))
                            (if version-index
                                (handle-version
                                 (vector-ref version-table
                                             (+ version-index 1)))
                                (bad-request-error connection)))))))
                
                (method-not-implemented-error connection)))))))

;==============================================================================
