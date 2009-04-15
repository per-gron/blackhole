;;; Routines common to both a HTTP client and server.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal, 2005-2007 Marc Feeley, All
;;; Rights Reserved.

(import ../string/util
        ../srfi/1
        ../srfi/13
        ../srfi/19
        x-www-form-urlencoded)

(export http-status-code
        make-cookie
        cookie?
        cookie-name
        cookie-name-set!
        cookie-value
        cookie-value-set!
        cookie-expires
        cookie-expires-set!
        cookie-domain
        cookie-domain-set!
        cookie-path
        cookie-path-set!
        cookie-port
        cookie-port-set!
        cookie-secure
        cookie-secure-set!
        cookie-http-only
        cookie-http-only-set!
        cookie-to-http
        cookie-parse
        cookie-headers
        headerify
        date-in-the-past
        date->rfc1123
        display-crlf
        display-header
        display-headers
        split-attribute-line
        permissive-read-line-reached-eof
        permissive-read-line
        permissive-read-lines
        read-header
        read-content-chars)

(declare (block)
         (mostly-fixnum)
         (standard-bindings)
         (extended-bindings))

;==============================================================================

; Status codes.

(define http-status-code #f)
(let ((http-status-codes (make-table)))
  (for-each (lambda (x)
              (table-set! http-status-codes
                          (car x)
                          (cdr x)))
            '((100 . "Continue")
              (101 . "Switching Protocols")
              (200 . "OK")
              (201 . "Created")
              (202 . "Accepted")
              (203 . "Non-Authoritative Information")
              (204 . "No Content")
              (205 . "Reset Content")
              (206 . "Partial Content")
              (300 . "Multiple Choices")
              (301 . "Moved Permanently")
              (302 . "Found")
              (303 . "See Other")
              (304 . "Not Modified")
              (305 . "Use Proxy")
              (307 . "Temporary Redirect")
              (400 . "Bad Request")
              (401 . "Unauthorized")
              (402 . "Payment Required")
              (403 . "Forbidden")
              (404 . "Not Found")
              (405 . "Method Not Allowed")
              (406 . "Not Acceptable")
              (407 . "Proxy Authentication Required")
              (408 . "Request Timeout")
              (409 . "Conflict")
              (410 . "Gone")
              (411 . "Length Required")
              (412 . "Precondition Failed")
              (413 . "Request Entity Too Large")
              (414 . "Request-URI Too Long")
              (415 . "Unsupported Media Type")
              (416 . "Requested Range Not Satisfiable")
              (417 . "Expectation Failed")
              (500 . "Internal Server Error")
              (501 . "Not Implemented")
              (502 . "Bad Gateway")
              (503 . "Service Unavailable")
              (504 . "Gateway Timeout")
              (505 . "HTTP Version Not Supported")))
  (set! http-status-code (lambda (num)
                           (table-ref http-status-codes num))))



;==============================================================================

; Cookies.

(define-type cookie
  id: D00E6EE6-5E55-47F2-B901-4DECBB3AA011
  constructor: make-cookie/no-check
  
  name
  value
  expires
  domain
  path
  port
  secure
  http-only)

(define http-separators
  (let ((lst '()))
    (string-for-each (lambda (x)
                       (set! lst (cons x lst)))
                     "()<>@,;:\\\"/[]?={} \t")
    lst))

;; One optimization might be to simply not call this function
(define (valid-cookie-name? name)
  (call/cc
   (lambda (ret)
     (string-for-each
      (lambda (chr)
        (let ((int (char->integer chr)))
          (if (or (<= int 31)
                  (>= int 127)
                  (find (lambda (x) (eq? x chr))
                        http-separators))
              (ret #f))))
      name)
     (ret #t))))

(define (make-cookie name value #!key expires domain path port secure http-only)
  (if (not (valid-cookie-name? name))
      (error "Invalid cookie name:" name))
  (make-cookie/no-check name value expires domain path port secure http-only))

(set! cookie-name-set!
      (lambda (c v)
        (error "Cookie names are read-only")))

(define (cookie-to-http c)
  (let ((name (cookie-name c))
        (value (cookie-value c))
        (expires (cookie-expires c))
        (domain (cookie-domain c))
        (path (cookie-path c))
        (port (cookie-port c))
        (secure (cookie-secure c))
        (http-only (cookie-http-only c)))
    (apply string-append
           `(,name
              "="
              ,(or value "")

              ,@(if expires
                    `("; expires=" ,(date->rfc1123 expires))
                    '())
              ,@(if domain
                    `("; domain=" ,domain)
                    '())
              ,@(if path
                    `("; path=" ,path)
                    '())
              ,@(if port
                    `("; port="
                      ,@(cond
                         ((number? port)
                          `("\"" ,(number->string port) "\""))
                         ((pair? port)
                          `("\""
                            ,@(join "," (map number->string port))
                            "\""))
                         (else `(,port)))))
              ,(if secure "; secure" "")
              ,(if http-only "; HttpOnly" "")
              "; Version=1"))))

;; Takes the raw Cookie: field data and splits it into a list
;; of key/value pairs.
(define (cookie-parse-split data)
  (let ((cookies (make-table)))
    (map (lambda (s)
           (let ((sp (string-split #\= s)))
             (if (or (null? sp)
                     (null? (cdr sp)))
                 (cons "" "")
                 (cons (urldecode (car sp))
                       (urldecode (cadr sp))))))
         (map string-strip (string-split #\; data)))))

(define (cookie-parse-to-list data)
  (let ((ps (cookie-parse-split data))
        (current-cookie #f)
        (default-prefs '()) ;; The special attributes specified before any other cookie
        (cookies '())) ;; List of processed cookies
    (define (set-pref name val)
      (let ((name (string-downcase name)))
        (if current-cookie
            (cond
             ((equal? name "$path")
              (cookie-path-set! current-cookie val))
             ((equal? name "$domain")
              (cookie-domain-set! current-cookie val))
             ((equal? name "$port")
              (cookie-port-set! current-cookie val)))
            (set! default-prefs (cons (cons name val)
                                      default-prefs)))))
    (define (new-cookie name val)
      (if current-cookie
          (set! cookies (cons current-cookie cookies)))
      (set! current-cookie (make-cookie name val))
      (for-each (lambda (x) (set-pref (car x) (cdr x)))
                default-prefs))
    (for-each
     (lambda (pair)
       (let ((name (car pair))
             (val (cdr pair)))
         (if (not (= 0 (string-length name)))
             (if (eq? #\$ (string-ref name 0))
                 (set-pref name val)
                 (new-cookie name val)))))
     ps)
    (if current-cookie
        (cons current-cookie cookies)
        '())))

;; Parses the value of a Cookie: header and returns it as a
;; table with the cookies, where the keys are the cookie names.
(define (cookie-parse data)
  (let ((tbl (make-table)))
    (for-each (lambda (c)
                (table-set! tbl (cookie-name c) c))
              (cookie-parse-to-list data))
    tbl))

(define (cookie-headers tbl port)
  (table-for-each (lambda (key val)
                    (display-crlf port "Set-Cookie: " (cookie-to-http val)))
                  tbl))

;==============================================================================

; Header writing functions.

(define (write-u8vector vec #!optional port)
  (write-subu8vector vec
                     0
                     (u8vector-length vec)
                     (or port (current-output-port))))

(define (headerify sym)
  (apply string-append
         (join "-"
               (map string-capitalize!
                    (string-name-split
                     (symbol->string sym))))))

(define date-in-the-past (make-date 0 0 0 0 1 1 1990 0))

(define (date->rfc1123 d)
  (date->string d "~a, ~d ~b ~Y ~T GMT"))

(define (display-crlf port . msgs)
  (if (not (null? msgs))
      (for-each (lambda (msg)
                  (display msg port))
                msgs))
  (display "\r\n" port))

(define (display-header port pair #!optional (charset "UTF-8"))
  (let ((name (car pair))
        (val (cdr pair)))
    (if (or (eq? name 'date)
            (eq? name 'expires))
        (set! val (date->rfc1123 val)))
    (if (not (eq? name 'charset))
        (if (eq? name 'content-type)
            (display-crlf
             port
             (headerify name)
             ": "
             val
             "; charset="
             charset)
            (display-crlf
             port (headerify name) ": " val)))))

(define (display-headers port hs #!optional (charset "UTF-8"))
  (for-each (lambda (x)
              (display-header port x charset))
            hs))

;==============================================================================

; Http reading functions.

(define (find-char-pos str char)
  (let loop ((i 0))
    (if (< i (string-length str))
        (if (char=? char (string-ref str i))
            i
            (loop (+ i 1)))
        #f)))

(define (split-attribute-line line)
  (let ((pos (find-char-pos line #\:)))
    (and pos
         (< (+ pos 1) (string-length line))
         (char=? #\space (string-ref line (+ pos 1)))
         (cons (let ((str (substring line 0 pos)))
                 (string-downcase! str)
                 str)
               (substring line (+ pos 2) (string-length line))))))

(define permissive-read-line-reached-eof
  (list 'permissive-read-line-reached-eof))

;; Reads one line from port, using the byte reading functions and not
;; read-line. Safe to US ASCII only.
(define (permissive-read-line port #!optional exception-on-eof?)
  (let loop ((lst '()))
    (let ((x (read-u8 port)))
      (cond
       ((and (eq? x 10)
             (pair? lst)
             (eq? (car lst) #\return))
        (reverse-list->string (cdr lst)))

       ((eq? #!eof x)
        (if exception-on-eof?
            (raise permissive-read-line-reached-eof)
            (reverse-list->string lst)))

       (else
        (loop (cons (integer->char x) lst)))))))

(define (permissive-read-lines port)
  (let loop ()
    (let ((line (permissive-read-line port #t)))
      (if (zero? (string-length line))
          (loop)
          line))))

(define (read-header port)
  (let loop ((attributes '()))
    (let ((line (permissive-read-line port)))
      (cond ((not line)
             #f)
            ((= (string-length line) 0)
             attributes)
            (else
             (let ((attribute (split-attribute-line line)))
               (if attribute
                   (loop (cons attribute attributes))
                   #f)))))))

(define (read-content-chars port attributes)
  (let ((cl
         (cond ((assoc "content-length" attributes)
                =>
                (lambda (x)
                  (let ((n (string->number (cdr x))))
                    (and n (integer? n) (exact? n) n))))
               (else
                #f))))
    (if cl
        (let ((str (make-string cl)))
          (let ((n (read-substring str 0 cl port)))
            (if (= n cl)
                str
                "")))
        "")))
