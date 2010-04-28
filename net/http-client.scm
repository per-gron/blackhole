;; ===================================================================
;;
;;   Another http (1.1) client library, by Per Eckerdal
;;
;; Things it supports:
;; * Reading chunked encoding
;; * Optionally following redirects
;; * Basic auth
;; * Post requests with application/x-www-form-urlencoded
;;
;; Things it cannot do right now:
;; * "multipart/form-data"
;; * .. and probably move out some of the code to lib/http-common
;;
;; ===================================================================
;;
;; History:
;;
;; 2008-09-23: Removed close-output-port call between sending headers
;;             and reading response, showed up not to work for certain
;;             remote hosts (!).
;; 

(import http-common
        uri
        x-www-form-urlencoded
        ../ds/queue
        ../ds/wt-tree
        ../string/base64
        ../string/util
        ../srfi/1
        ../srfi/13
        ../srfi/19
        ../misc/u8v)

(export http-max-pipelined-requests-per-connection
        http-max-connection-idle-time
        http-max-connections-per-server
        http-preferred-pipelined-connections
        
        make-http-request
        http-request?
        http-request-method
        http-request-uri
        http-request-headers
        http-request-query
        http-request-port-callback
        http-request-done-callback
        http-request-method-set
        http-request-uri-set
        http-request-headers-set
        http-request-query-set
        http-request-port-callback-set
        http-request-done-callback-set
        http-queue-request
        http-invoke-request
        
        http-follow-request
        http-request-to-string
        http-post
        http-get
        
        make-http-client-error-from-remote
        http-client-error-from-remote?
        http-client-error-from-remote-code
        http-client-error-from-remote-headers
        http-client-error-from-remote-request-headers
        http-client-error-from-remote-uri
        http-client-error-from-remote-content
        http-access-url
        http-get-url
        http-post-url
        http-client-404-exception?
        http-response-get-status
        http-response-get-headers
        http-response-get-content
        http-response-get-content-string)

(declare (block)
         (mostly-fixnum)
         (standard-bindings)
         (extended-bindings))

;; ===================================================================
;;
;; Tweakable parameters of the library
;;

;; Define them first, then set them. This is to be able to do (declare
;; (block)) and still be able to |set!| these things later.
(define http-max-pipelined-requests-per-connection #f)
(define http-max-connection-idle-time #f) ;; In seconds
(define http-max-connections-per-server #f)
(define http-preferred-pipelined-connections #f) ;; Not implemented

(set! http-max-pipelined-requests-per-connection 2)
(set! http-max-connection-idle-time 30)
(set! http-max-connections-per-server 200)
(set! http-preferred-pipelined-connections 2) ;; Not implemented

;; ===================================================================
;;
;; Utility functions
;;


(define (al-get lst key #!optional (dfl #f))
  (let ((pair (assoc key lst)))
    (if pair
        (cdr pair)
        dfl)))

(define (mutex-thunk)
  (let ((mtx (make-mutex)))
    (lambda (thunk)
      (dynamic-wind
       (lambda ()
         (mutex-lock! mtx))
       thunk
       (lambda ()
         (mutex-unlock! mtx))))))

;; TODO This functionality overlaps with lib/uri. lib/uri should
;; really be used instead.
(define (uri-query-string uri)
  (with-output-to-string
   (string)
   (lambda ()
     (display (let ((p (uri-path uri)))
                (if (zero? (string-length p))
                    "/"
                    p)))

     (let ((q (uri-query uri)))
       (cond
        ((string? q)
         (display "?")
         (display q))

        ((pair? q)
         (let ((first #t))
           (for-each
            (lambda (pair)
              (display (if first
                           "?"
                           "&"))
              (set! first #f)
              (if (cdr pair)
                  (display (list
                            (urlencode (car pair))
                            "="
                            (urlencode (cdr pair))))
                  (display (urlencode (car pair)))))
            q))))))))

(define (pipe/buffer in-port out-port #!optional chars-left buf)
  (let* ((buf (or buf (make-u8vector (* 5 1024))))
         (buf-len (u8vector-length buf))
         (bytes-to-read (if chars-left
                            (min chars-left buf-len)
                            buf-len)))
    (if (zero? bytes-to-read)
        #t
        (let ((read-bytes
               (read-subu8vector buf 0 bytes-to-read in-port)))
          (if (and chars-left
                   (not (= bytes-to-read read-bytes)))
              (error "Failed to read correct number of bytes"
                     read-bytes
                     bytes-to-read))
          (write-subu8vector buf 0 read-bytes out-port)
          (pipe/buffer in-port
                       out-port
                       (- chars-left bytes-to-read)
                       buf)))))

(define (http-auth-headers un pw)
  (cons 'authorization
        (string-append "Basic "
                       (u8vector->base64-string
                        (string->utf8-u8vector
                         (string-append un ":" pw))))))

;; Joins two alists of headers, the first one having precedence over
;; the other. TODO Right now the implementation rather quick&dirty.
(define (header-join one two)
  (table->list
   (table-merge!
    (list->table one)
    (list->table two))))

(define (with-port-for-request uri thunk #!key (close-on-exit #t))
  (let* ((host (let ((host (uri-host uri)))
                 (or host
                     (error "Cannot connect to server without address"
                            uri))))
         (scheme (or (uri-scheme uri) "http"))
         (port-number
          (or (uri-port uri)
              (cond ((equal? scheme "http")
                     80)
                    ((equal? scheme "https")
                     443)
                    (else
                     (error "Don't know what port number to use for protocol"
                            scheme)))))
         ;; The following line is where one would hook up a green
         ;; thread friendly dns lookup. I removed it because of
         ;; licensing issues with the dns library used. Hopefully
         ;; it'll be possible to use a better dns client in the
         ;; future.
         (server-address host)
         (port
          ((cond ((equal? scheme "http")
                  open-tcp-client)
                 ;; I have removed openssl support because of license
                 ;; issues with the openssl module. Hopefully it can
                 ;; be included in the future. To add the support
                 ;; again, just use the ssl module and uncomment these
                 ;; lines:
                 ;; 
                 ;;((equal? scheme "https")
                 ;; open-ssl-tcp-client)
                 (else
                  (error "Unknown protocol" scheme)))
           (list server-address: server-address
                 port-number: port-number))))
    
    (let ((res (thunk port)))
      (if close-on-exit
          (close-port port))
      res)))

(define (chunked-coding-read-hex str)
  (let* ((str-len (string-length str))
         (chr-lst
          (let loop ((lst '()) (idx 0))
            (cond
             ((or (>= idx str-len)
                  (let ((chr (string-ref str idx)))
                    (or (char=? #\; chr)
                        (char=? #\space chr))))
              lst)

             (else
              (loop (cons (char->integer
                           (string-ref str idx))
                          lst)
                    (+ 1 idx))))))
         (zero (char->integer #\0))
         (nine (char->integer #\9))
         (a (char->integer #\a))
         (A (char->integer #\A))
         (f (char->integer #\f))
         (F (char->integer #\F)))
    (let loop ((lst chr-lst) (multiple 1))
      (if (null? lst)
          0
          (let ((chr (car lst)))
            (+ (loop (cdr lst) (* multiple 16))
               (* multiple
                  (cond
                   ((and (>= chr zero)
                         (<= chr nine))
                    (- chr zero))

                   ((and (>= chr a)
                         (<= chr f))
                    (+ 10 (- chr a)))

                   ((and (>= chr A)
                         (<= chr F))
                    (+ 10 (- chr A)))

                   (else
                    (error "Invalid character in hex string" str))))))))))


;; ===================================================================
;;
;; Request sending functions
;;

;; Bah.. This function is quite ugly.
(define (display-request port method uri headers query)
  (display (string-upcase (symbol->string method)) port)
  (display " " port)
  (display (uri-query-string uri) port)
  (display-crlf port " HTTP/1.1")

  (let* ((ct (al-get headers 'content-type))

         (header-content-pair
          (cond
           ((not query)
            (cons '() #f))

           ((port? query)
            (cons '((transfer-encoding . chunked))
                  query))

           ((string? query)
            (cons `((content-length . ,(string-length query)))
                  (string->utf8-u8vector query)))

           ((u8vector? query)
            (cons `((content-length . ,(u8vector-length query)))
                  query))

           ((pair? query)
            (cond
             ((or (not ct)
                  (equal? ct "application/x-www-form-urlencoded"))
              (let ((content (with-output-to-u8vector
                              (u8vector)
                              (lambda ()
                                (write-x-www-form-urlencoded query)))))
                (cons
                 `((content-type . "application/x-www-form-urlencoded")
                   (content-length . ,(u8vector-length content)))
                 content)))

             ((string-prefix? "multipart/form-data" ct)
              (error "multipart/form-data is not implemented"))

             (else
              (error "Invalid Content-Type header for given query"
                     ct
                     query))))

           (else
            (error "Invalid query parameter type" query))))

         (actual-headers
          (header-join
           headers
           `((host . ,(uri-host uri))
             (user-agent . "curl/7.18.2 (i486-pc-linux-gnu) libcurl/7.18.2")
             (cache-control . "max-age=0")
             (accept-charset . "utf-8;q=0.7,ISO-8859-1;q=0.6,*;q=0.5")
             (accept . "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5")
             (accept-encoding . "")
             (accept-language . "en")
             (pragma . "no-cache")
             (cache-control . "no-cache")
             ,@(car header-content-pair)))))
    (display-headers port actual-headers)

    ;; If method is post, and no content-length header has been sent, send one.
    (if (and (eq? method 'post)
             (not (assq 'content-length actual-headers)))
        (display-header port (cons 'content-length 0)))

    (display-crlf port)
    (let ((content (cdr header-content-pair)))
      (if content
          (begin
            (if (u8vector? content)
                (write-u8vector content port)
                (pipe/buffer content port))
            (display-crlf port))))

    (force-output port)))


;; ===================================================================
;;
;; Response reading functions
;;

(define (read-content port output-port/vector headers)
  (let ((output-port (if (u8vector? output-port/vector)
                         (open-u8vector output-port/vector)
                         output-port/vector))

        (cl (let ((val (al-get headers "content-length")))
              (and val (string->number val))))

        (chunked?
         (find (lambda (x)
                 (and (equal? "transfer-encoding" (car x))
                      (string-prefix? "chunked"
                                      (string-downcase (cdr x)))))
               headers)))
    (cond
     (chunked?
      (let ((buf (make-u8vector (* 5 1024))))
        (let loop ()
          (let* ((len-str (permissive-read-line port))
                 (len (chunked-coding-read-hex len-str)))
            (if (not (zero? len))
                (begin
                  (pipe/buffer port
                               output-port
                               len
                               buf)
                  (permissive-read-line port)
                  (loop)))))))

     (cl
      (if (u8vector? output-port/vector)
          ;; Do the case when the output port is a vector with an optimized
          ;; (non-copying) algorithm.
          (let ((read-bytes (read-subu8vector output-port/vector 0 cl port)))
            (if (not (= read-bytes cl))
                (error "Failed to read correct number of bytes" read-bytes cl)))


          (let ((buf (make-u8vector (min cl (* 5 1024)))))
            (pipe/buffer port output-port cl buf))))

     (else
      (dump-u8vector-port-to-other-u8vector-port
       port
       output-port)))))

(define (read-status-line-code str)
  (let* ((len (string-length str))
		 (hit-space #f)
         (start
          (let loop ((start 0))
			(if (char=? (string-ref str start) #\space)
				(set! hit-space #t)
				#f)
            (if (and (< start len)
					 hit-space
                     (not (char-numeric? (string-ref str start))))
                (loop (+ 1 start))
                start))))
    (string->number
     (substring str start (min (+ start 3) len)))))

(define (read-response port req)
  (let ((cont (http-request-port-callback req))
        (method (http-request-method req)))
    (call/cc
     (lambda (ret)
       (let* ((status-line (permissive-read-lines port))

              (code
               (or (read-status-line-code status-line)
                   (error "Couldn't parse status line" status-line)))

              (has-message-body
               (if (= 100 code)
                   (ret (read-response port req))

                   ;; 204 No Content does not include a message-body
                   ;; 304 Not Modified does not include a message-body
                   ;; 1xx responses don't include a message-body
                   ;;
                   ;; If the request was head, then the response
                   ;; doesn't include a message-body
                   (not
                    (or (= 204 code)
                        (= 304 code)
                        (= 1 (floor (/ code 100)))
                        (eq? method 'head)))))

              (headers (read-header port))

              (output-port (cont code headers has-message-body)))

         (if has-message-body
             (read-content port output-port headers))

         ;; Return true if the server sent Connection: close
         (member '("connection" . "close") headers))))))

;; ===================================================================
;;
;; Persistent connection and pipelining layer
;;

;;; Functions that a user of the library might invoke

;; This api demands some explanation. The parameters it takes are the
;; obvious method ('get, 'post, 'method and so on), uri, headers (as
;; an alist where the keys are lowercase symbols), query,
;; port-callback, a thunk described later and done-callback, a thunk
;; that takes no parameters and is invoked when the request is done.
;;
;; Both port-callback and done-callback will be invoked from another
;; thread than the invocation of the http request was from, but they
;; are guaranteed to be called from the same thread, done-callback
;; after part-callback.
;;
;; part-callback should be a procedure taking three arguments:
;; * code, the numerical response code of the http request,
;; * headers, an alist where the car is a lowercase string of
;;   the header and cdr is a string of the value (not case-modified)
;; * has-content?, a boolean indicating whether this response will
;;   have a content body.
;;
;; This function should return an output port or an u8vector. The
;; response of the request will be written to this output
;; port/u8vector.
(define-type http-request
             (method read-only:)
             (uri read-only:)
             (headers read-only:)
             (query read-only:)
             (port-callback read-only:)
             (done-callback read-only:))

(define (http-request-method-set req m)
  (make-http-request
   m
   (http-request-uri req)
   (http-request-headers req)
   (http-request-query req)
   (http-request-port-callback req)
   (http-request-done-callback req)))

(define (http-request-uri-set req u)
  (make-http-request
   (http-request-method req)
   u
   (http-request-headers req)
   (http-request-query req)
   (http-request-port-callback req)
   (http-request-done-callback req)))

(define (http-request-headers-set req h)
  (make-http-request
   (http-request-method req)
   (http-request-uri req)
   h
   (http-request-query req)
   (http-request-port-callback req)
   (http-request-done-callback req)))

(define (http-request-query-set req q)
  (make-http-request
   (http-request-method req)
   (http-request-uri req)
   (http-request-headers req)
   q
   (http-request-port-callback req)
   (http-request-done-callback req)))

(define (http-request-port-callback-set req pc)
  (make-http-request
   (http-request-method req)
   (http-request-uri req)
   (http-request-headers req)
   (http-request-query req)
   pc
   (http-request-done-callback req)))

(define (http-request-done-callback-set req dc)
  (make-http-request
   (http-request-method req)
   (http-request-uri req)
   (http-request-headers req)
   (http-request-query req)
   (http-request-port-callback req)
   dc))

;; The main entry point of this part of the machinery
(define (http-queue-request req)
  (let ((conns (request-queue-get-connections (http-request-uri req)
                                              #t)))
    (http-connections-push-request conns req)))

;; Don't use persistent connections.
(define (http-invoke-request req)
  (thread-start!
   (make-thread
    (lambda ()
      (with-port-for-request
       (http-request-uri req)
       (lambda (port)
         (display-request port
                          (http-request-method req)
                          (http-request-uri req)
                          (header-join (http-request-headers req)
                                       '((connection . close)))
                          (http-request-query req))
         (read-response port req)
         ((http-request-done-callback req))))))))


;;; Functions regarding the request queue:

(define request-queue-table (make-table))
(define request-queue-with-mutex! (mutex-thunk))

(define (request-queue-get-connections uri #!optional create-on-fail?)
  (request-queue-with-mutex!
   (lambda ()
     (let* ((stripped-uri (make-uri (uri-scheme uri)
                                    ;; Skip the userinfo part of the
                                    ;; authority
                                    (cons #f
                                          (cdr (uri-authority uri)))
                                    #f
                                    #f
                                    #f))
            (res (table-ref request-queue-table
                            stripped-uri
                            #f)))
       (if (and (not res) create-on-fail?)
           (let ((new-conns (make-http-connections stripped-uri)))
             (table-set! request-queue-table stripped-uri new-conns)
             new-conns)
           res)))))

;; Used by http-connections-pop-connection when there is no longer an
;; open connection to that server.
(define (request-queue-pop-connections server)
  (request-queue-with-mutex!
   (lambda ()
     (table-set! request-queue-table server))))



;;; Functions regarding the http-connections structure:

;; A http-connections object keeps the connections we have to one
;; particular server (that is, protocol (http/https), host and port).
(define-type http-connections
             constructor: make-http-connections-internal

             ;; This variable is never mutated so it can be used without mutex.
             (server read-only:)

             ;; (The following are only to be used internally by the
             ;; http-connections functions.)

             ;; conns is a wt-tree where the value is a thread with a connection
             ;; and the key is the number of pipelined requests on that
             ;; connection.
             threads

             ;; Because wt-tree cannot have more than one value with the same
             ;; key, this variable is used to generate unique keys for the tree.
             counter

             ;; Used to maintain thread safety. All operations on the threads
             ;; tree and the counter must go through this mutex.
             (mutex read-only:))

(define http-connections-wt-tree-type
  (make-wt-tree-type
   ;; Comparison function.
   (lambda (a b)
     (let ((a-val (car a))
           (b-val (car b))
           (a-uniq (cdr a))
           (b-uniq (cdr b)))
       (if (= a-val b-val)
           (< a-uniq b-uniq)
           (< a-val b-val))))))


(define (make-http-connections server)
  (make-http-connections-internal
   server
   (make-wt-tree http-connections-wt-tree-type)
   0
   (make-mutex server)))

;; Used internally by the http-connections functionse
(define (http-connections-with-mutex! conns thunk)
  (let ((mtx (http-connections-mutex conns)))
    (dynamic-wind
     (lambda ()
       (mutex-lock! mtx))
     thunk
     (lambda ()
       (mutex-unlock! mtx)))))

;; Invoked by http-connections-push-request when it decides that one
;; more connection is needed.
;;
;; Creates a new connection thread belonging to a http-connections
;; structure.
;;
;; Note that this function does not lockt the http-connections
;; mutex. The user of the function must do that.
(define (http-connections-push-connection conns)
  (let* ((uid (http-connections-counter conns))
         (conn (http-connection-open conns uid))
         (key (cons 0 uid)))
    (http-connections-counter-set! conns (+ 1 uid))
    (wt-tree/add! (http-connections-threads conns)
                  key
                  conn)
    (cons key conn)))

;; Invoked by the connection upon its death. If the connection queue
;; gets empty, the http-connections object removes itself from the
;; request queue.
;;
;; n-reqs is the number of requests that the connection has left to
;; do, uid is the connection's id.
(define (http-connections-pop-connection conns n-reqs uid)
  (http-connections-with-mutex!
   conns
   (lambda ()
     (let ((tree (http-connections-threads conns)))
       (wt-tree/delete! tree (cons n-reqs uid))
       (if (and (zero? n-reqs)
                (wt-tree/empty? tree))
           ;; Only pop this connection if the connection had zero
           ;; pending requests. Otherwise we aren't done yet.
           (request-queue-pop-connections conns))))))

;; The "entry point" to the http-connections part of the
;; machinery. Used by http-queue-request.
;;
;; This function takes a request and makes sure the request gets done.
;;
;; TODO The http-preferred-pipelined-connections stuff isn't
;; implemented.
(define (http-connections-push-request conns request)
  (http-connections-with-mutex!
   conns
   (lambda ()
     (let ((threads (http-connections-threads conns)))
       (if (wt-tree/empty? threads)
           (http-connection-push-request
            (cdr (http-connections-push-connection conns))
            request)
           (let* ((min-pair (wt-tree/min-pair threads))
                  (pair
                   (if (and (> (caar min-pair)
                               http-max-pipelined-requests-per-connection)
                            (< (wt-tree/size threads)
                               http-max-connections-per-server))
                       (http-connections-push-connection conns)
                       min-pair)))
             (wt-tree/delete! threads (car pair))
             (wt-tree/add! threads
                           (cons (+ 1 (caar pair))
                                 (cdar pair))
                           (cdr pair))
             (http-connection-push-request
              (cdr pair)
              request)))))))

;; Invoked by the connection when a request is completed. It decreases
;; the number of requests that is kept for that connection in the
;; threads tree.
;;
;; n-reqs is the requests the connection had earlier, uid is the
;; connection's id, conn is the connection object.
(define (http-connections-pop-request conns n-reqs uid conn)
  (http-connections-with-mutex!
   conns
   (lambda ()
     (let* ((tree (http-connections-threads conns)))
       (wt-tree/delete! tree (cons n-reqs uid))
       (wt-tree/add! tree
                     (cons (- n-reqs 1) uid)
                     conn)))))

;;; Functions regarding the connection threads.

(define (http-connection-open connections uid)
  ;; The with-port-for-request should be outside of the thread because
  ;; we want to throw an exception to the caller of this function if
  ;; we fail to connect to the server.
  (with-port-for-request
   (http-connections-server connections)
   (lambda (port)
     (let* (;; A queue that contains the requests that the thread is
            ;; scheduled to do. Is must be synced with the map that is
            ;; kept in the connections object (the request count must be
            ;; the same)
            ;;
            ;; A #f in the queue is a signal to the receiver thread that
            ;; it should stop working.
            (rq (make-queue))
            (with-rq! (mutex-thunk)))
       (thread-start!
        (make-thread
         (lambda ()
           (let* ((sender (current-thread))
                  ;; Used to wake up the receiver thread when something
                  ;; should be read. It should be locked in the
                  ;; beginning to make the receiver wait correctly.
                  (receive-mutex (let ((m (make-mutex 'receive-mutex)))
                                   (mutex-lock! m)
                                   m))
                  (receiver
                   (thread-start!
                    (make-thread
                     (lambda ()
                       (let loop ()
                         (let ((req
                                (let loop ()
                                  (or (with-rq!
                                       (lambda ()
                                         (queue-front rq #f)))
                                      (begin
                                        ;; Wait for a signal from the
                                        ;; sender thread
                                        (mutex-lock!
                                         receive-mutex
                                         http-max-connection-idle-time)
                                        (loop))))))
                           (if req
                               (with-exception-catcher
                                (lambda (e)
                                  ;; Silently ignore the error. After
                                  ;; this, we send a message to the
                                  ;; sender thread to kill itself.
                                  #f)
                                (lambda ()
                                  (let ((close?
                                         (read-response port req)))
                                    (with-rq!
                                     (lambda ()
                                       (http-connections-pop-request
                                        connections
                                        (queue-size rq)
                                        uid
                                        sender)
                                       (queue-pop! rq)))
                                    ((http-request-done-callback req))
                                    (if (not close?) (loop))))))))
                       (thread-send sender #f))))))
             ;; The sender loop
             (with-exception-catcher
              (lambda (e)
                ;; This will be reached both when something goes wrong
                ;; and when the timeout is reached.
                #f)
              (lambda ()
                (let loop ()
                  (let ((req (thread-receive
                              http-max-connection-idle-time)))
                    (if (eq? 'alive
                             (thread-join! receiver 0 'alive))
                        (begin
                          (display-request port
                                           (http-request-method req)
                                           (http-request-uri req)
                                           (http-request-headers req)
                                           (http-request-query req))
                          (with-rq!
                           (lambda ()
                             (queue-push! rq req)))
                          (mutex-unlock! receive-mutex)
                          (if (not (member '(connection . close)
                                           (http-request-headers req)))
                              ;; Only continue the loop if we didn't send a
                              ;; Connection: close header
                              (loop))))))))
             ;; The connection is dead. Now shut everything down gracefully.
             (with-rq!
              (lambda ()
                ;; Remove this connection from the connections structure
                (http-connections-pop-connection
                 connections
                 (queue-size rq)
                 uid)
                ;; Re-issue the requests that aren't done yet
                (let loop ()
                  (if (not (queue-empty? rq))
                      (begin
                        (http-connections-push-request
                         connections
                         (queue-pop! rq)))))
                ;; Tell the receiver thread to stop
                (queue-push! rq #f)
                (mutex-unlock! receive-mutex)))
             ;; Wait for the receiver thread to exit before we close
             ;; the connection.
             (thread-join! receiver)
             (close-port port)))))))
      close-on-exit: #f))

(define (http-connection-push-request conn req)
  ;; I want guaranteed message delivery and last-in-first-out order of
  ;; the messages, so I don't use termite.
  (thread-send conn req))

;; ===================================================================
;;
;; Higher-level functionality
;;

;; The follow parameter can be an integer or #f for one redirection
(define (http-follow-request req
                             #!optional
                             (follow 10)
                             (req-fun http-queue-request))
  (let ((method (http-request-method req))
        (uri (http-request-uri req))
        (headers (http-request-headers req))
        (query (http-request-query req)))
    (cond
     ((not follow)
      (req-fun req))

     ((negative? follow)
      (error "Too many redirects"))

     (else
      (let ((redir-loc #f))
        ;; redir-loc is a variable that will be set later if this
        ;; request gives a redirect as result. It is needed because we
        ;; can't call send-http-request from within the port callback we
        ;; send to it (or shouldn't, at least)
        (req-fun
         (make-http-request
          method
          uri
          headers
          query
          (lambda (code resp-headers has-content?)
            (if (or (and (or (= 301 code)
                             (= 302 code)
                             (= 307 code))
                         ;; Unless the method is get or head, don't redirect.
                         (or (eq? 'get method)
                             (eq? 'head method)))
                    (= 303 code))

                (let ((loc (al-get resp-headers "location")))
                  (if loc
                      (begin
                        (set! redir-loc loc)
                        ;; Return a u8vector port. The content of the
                        ;; redirect response will be filled to that
                        ;; vector, which is then thrown away.
                        (if has-content? (open-u8vector (u8vector))))
                      (error "No location header of response" uri)))

                ((http-request-port-callback req)
                 code
                 resp-headers
                 has-content?)))
          (lambda ()
            (if redir-loc

                (http-follow-request
                 (http-request-uri-set
                  req
                  (uri-join uri
                            (string->uri redir-loc)))
                 (- follow 1)
                 req-fun)

                ((http-request-done-callback req)))))))))))

;;; Accessory functions

(define (http-request-to-string method
                                url
                                #!optional
                                query
                                (headers '()))
  (let ((up (open-output-u8vector (u8vector)))
        (mtx (make-mutex)))
    (mutex-lock! mtx)
    (http-follow-request
     (make-http-request
      method
      (if (uri? url) url (string->uri url))
      headers
      query
      (lambda (code headers has-content?) up)
      (lambda ()
        (mutex-unlock! mtx))))
    (mutex-lock! mtx)
    (u8vector->string (get-output-u8vector up)
                      ignore-errors-in-encoding: #t)))

(define (http-post url #!optional query (headers '()))
  (http-request-to-string 'post url query headers))

(define (http-get url #!optional (headers '()))
  (http-request-to-string 'get url #f headers))

(define-type http-client-error-from-remote
             id: DB7F49A3-BBF6-40F3-95A1-038CCACE1D88
             (code read-only:)
             (headers read-only:)
             (request-headers read-only:)
             (uri read-only:)
             (content read-only:))

(define (if-applicable-raise-remote-exception code
                                              headers
                                              request-headers
                                              uri
                                              content)
  (if (not (or (= code 100)
               (= code 200)
               (and (>= code 300) (< code 400))))
      (raise
       (make-http-client-error-from-remote
        code
        headers
        request-headers
        uri
        (if (u8vector? content)
            (u8vector->ISO-8859-1-string content)
            content)))))

(define (http-access-url method
                         uri
                         #!optional
                         http-username
                         http-password
                         (headers '())
                         (follow #t)
                         query)
  (let* ((up (open-output-u8vector (u8vector)))
         (mtx (make-mutex))
         (response-code #f)
         (response-headers #f)
         (headers (if (and http-username http-password)
                      (cons (http-auth-headers http-username
                                               http-password)
                            headers)
                      headers))
         (response-data (call-with-output-u8vector
                         '()
                         (lambda (port)
                           (mutex-lock! mtx)
                           (http-follow-request
                            (make-http-request
                             method
                             (if (uri? uri) uri (string->uri uri))
                             headers
                             query
                             (lambda (code headers has-content?)
                               (set! response-code code)
                               (set! response-headers headers)
                               port)
                             (lambda ()
                               (mutex-unlock! mtx))))
                           (mutex-lock! mtx)))))
    
    (if-applicable-raise-remote-exception response-code
                                          response-headers
                                          headers
                                          uri
                                          response-data)

    (list response-code
          response-headers
          response-data)))

(define (http-get-url . p)
  (apply http-access-url (cons 'get p)))

(define (http-post-url . p)
  (apply http-access-url (cons 'post p)))

(define (http-client-404-exception? e)
  (or (http-client-error-from-remote? e)
      (= 404 (http-client-error-from-remote-code e))))

(define (http-response-get-status c)
  (car c))

(define (http-response-get-headers c)
  (cadr c))

(define (http-response-get-content c)
  (caddr c))

(define (http-response-get-content-string c
                                          #!optional
                                          ignore-errors-in-encoding)
  (u8vector->string
   (http-response-get-content c)
   ignore-errors-in-encoding: ignore-errors-in-encoding))


(define http-client-examples
  '(
    (use /lib/http-common
         /lib/http-client)
    (http-get-url "http://ws.audioscrobbler.com/2.0/?method=user%2Egetplaylists&api%5Fkey=35c639c94148a2a26ef8faa9f94d5b7c&user=bwsb")

    ; Example 2, manual HTTP access
    (define p (open-tcp-client (list server-address: "ws.audioscrobbler.com" port-number: 80)))
    (display "GET /2.0/?method=user%2Egetplaylists&api%5Fkey=35c639c94148a2a26ef8faa9f94d5b7c&user=bwsb HTTP/1.01\r\nPragma: no-cache\r\nCache-Control: max-age=0\r\nConnection: close\r\nHost: ws.audioscrobbler.com\r\nAccept-Charset: utf-8;q=0.7,ISO-8859-1;q=0.6,*;q=0.5\r\nAccept-Language: en\r\nAccept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\r\nUser-Agent: Beta software 0.1 (Linux 2.6, Debian Etch)\r\n\r\n" p)
    (force-output p)
    ; (read-line p) ; The easy way is to use the character system, though doing that disables the ability to read bytes subsequently during the connection.
    (permissive-read-line p) ; Byte-based readline
    ))
