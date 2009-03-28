;;; URI parsing.
;;;
;;; Written by Marc Feeley for http-server, made to a
;;; separate module and refactored by Per Eckerdal.

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  ;;(not safe) ?
  )

(export make-uri
        uri?
        uri-scheme
        uri-scheme-set!
        uri-authority
        uri-authority-set!
        uri-path
        uri-path-set!
        uri-query
        uri-query-set!
        uri-fragment
        uri-fragment-set!
        uri-userinfo
        uri-host
        uri-port
        uri-query-string
        parse-uri
        parse-uri-query
        string->uri
        uri->string
        string->uri-query
        encode-for-uri
        remove-dot-segments
        uri-join
        uri-join*)

(define-type uri
  id: 62788556-c247-11d9-9598-00039301ba52

  scheme
  authority
  path
  query
  fragment)

(define (uri-userinfo uri)
  (car (uri-authority uri)))

(define (uri-host uri)
  (cadr (uri-authority uri)))

(define (uri-port uri)
  (caddr (uri-authority uri)))

(define (uri-query-string uri)
  (let ((q (uri-query uri)))
    (if (string? q)
        q
        (with-output-to-string
          ""
          (lambda ()
            (cond
             ((string? q)
              (display "?")
              (display q))
             
             ((pair? q)
              (let ((first #t))
                (for-each
                 (lambda (pair)
                   (if (not first)
                       (display "&"))
                   (set! first #f)
                   (if (cdr pair)
                       (display (list
                                 (urlencode (car pair))
                                 "="
                                 (urlencode (cdr pair))))
                       (display (urlencode (car pair)))))
                 q)))))))))

;;(private)

(define (hex-digit str i)
  (let ((n (char->integer (string-ref str i))))
    (cond ((and (>= n 48) (<= n 57))
           (- n 48))
          ((and (>= n 65) (<= n 70))
           (- n 55))
          ((and (>= n 97) (<= n 102))
           (- n 87))
          (else
           #f))))

(define (hex-octet str i)
  (let ((n1 (hex-digit str i)))
    (and n1
         (let ((n2 (hex-digit str (+ i 1))))
           (and n2
                (+ (* n1 16) n2))))))

(define (plausible-hex-escape? str end j strict)
  (and (< (+ j 2) end)
       (not (control-or-space-char? (string-ref str (+ j 1)) strict))
       (not (control-or-space-char? (string-ref str (+ j 2)) strict))))

(define (control-or-space-char? c strict)
  (or (not ((if strict
                char<?
                char<=?)
            #\space c))
      (not (char<? c #\x7f))))

(define (excluded-char? c)
  (or (not (char<? #\space c))
      (not (char<? c #\x7f))
      (char=? c #\<)
      (char=? c #\>)
      (char=? c #\#)
      (char=? c #\%)
      (char=? c #\")
      (char=? c #\{)
      (char=? c #\})
      (char=? c #\|)
      (char=? c #\\)
      (char=? c #\^)
      (char=? c #\[)
      (char=? c #\])
      (char=? c #\`)))

(define (extract-escaped str start n)
  (let ((result (make-string n)))
    (let loop ((i start) (j 0))
      (if (< j n)
          (let ((c (string-ref str i)))
            (if (char=? c #\%)
                (let ((n (hex-octet str (+ i 1))))
                  (and n
                       (begin
                         (string-set! result j (integer->char n))
                         (loop (+ i 3)
                               (+ j 1)))))
                (begin
                  (string-set! result j (if (char=? c #\+) #\space c))
                  (loop (+ i 1)
                        (+ j 1)))))
          result))))

;;(/private)

(define (parse-uri str start end decode? cont #!optional (strict #t))
  (let ((uri (make-uri #f #f "" '() #f)))
    
    (define (extract-string i j n)
      (if decode?
          (extract-escaped str i n)
          (substring str i j)))
    
    (define (extract-query i j n)
      (if decode?
          (parse-uri-query
           str
           i
           j
           decode?
           (lambda (bindings end)
             bindings))
          (substring str i j)))
    
    ;; possibly inside the "scheme" part
    (define (state0 i j n)
      ;; (dbg "State0 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (if (< j end)
          (let ((c (string-ref str j)))
            ;; (dbg "state0 processes char " c)
            (cond ((char=? c #\:)
                   (if (= n 0)
                       (state2 j (+ j 1) 1) ; the ":" is in the "path" part
                       (let ((scheme (extract-string i j n)))
                         (and scheme
                              (begin
                                (uri-scheme-set! uri scheme)
                                (if (and (< (+ j 2) end)
                                         (char=? (string-ref str (+ j 1))
                                                 #\/)
                                         (char=? (string-ref str (+ j 2))
                                                 #\/))
                                    (state1 (+ j 3) (+ j 3) 0 #f #f)
                                    (state2 (+ j 1) (+ j 1) 0)))))))
                  ((char=? c #\/)
                   (if (and (= n 0)
                            (< (+ j 1) end)
                            (char=? (string-ref str (+ j 1)) #\/))
                       (state1 (+ j 2) (+ j 2) 0 #f #f)
                       (state2 i (+ j 1) (+ n 1))))
                  ((char=? c #\?)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            (uri-path-set! uri path)
                            (state3 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\#)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            (uri-path-set! uri path)
                            (state4 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state0 i (+ j 3) (+ n 1))))
                  ((control-or-space-char? c strict)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            (uri-path-set! uri path)
                            j))))
                  (else
                   (state0 i (+ j 1) (+ n 1)))))
          (let ((path (extract-string i j n)))
            (and path
                 (begin
                   ;;(dbg "state0 concluded uri-path to " path)
                   (uri-path-set! uri path)
                   j)))))

    ;; inside the "authority" part.  last-colon-marker is a cons of (j
    ;; . n) where the last colon was found, userinfo-idx is a cons of
    ;; (j . n) where the last @ was found. This is used in the
    ;; conclude function, which creates the authority part, a list of
    ;; (USERINFO HOST PORT).
    (define (state1 i j n last-colon-marker userinfo-idx)
      ;; (dbg "State1 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (define (conclude fun new-i new-j new-n)
        (let* (;; If userinfo-idx (the index of the @ mark) is
               ;; greater than the last-colon-marker, then the
               ;; last-colon-marker doesn't refer to the port
               ;; and should be discarded.
               (last-colon-marker
                (if (and userinfo-idx
                         (> (car userinfo-idx)
                            (car last-colon-marker)))
                    #f
                    last-colon-marker))
               (userinfo
                (and userinfo-idx
                     (extract-string i
                                     (car userinfo-idx)
                                     (cdr userinfo-idx))))
               (after-userinfo (if userinfo-idx
                                   (+ (car userinfo-idx) 1)
                                   i))
               (host-absolute-end
                (if last-colon-marker
                    (car last-colon-marker)
                    j))
               (host (extract-string after-userinfo
                                     host-absolute-end
                                     (- host-absolute-end after-userinfo)))
               (port (and last-colon-marker
                          (let ((port-absolute-begin
                                 (+ 1 (car last-colon-marker))))
                            (string->number
                             (extract-string port-absolute-begin
                                             j
                                             (- j port-absolute-begin))))))
               (authority (list userinfo host port)))
          (and authority
               (begin
                 (uri-authority-set! uri authority)
                 (fun new-i new-j new-n)))))
      
      (if (< j end)
          (let ((c (string-ref str j)))
            (cond ((char=? c #\/)
                   (conclude state2 j (+ j 1) 1))
                  ((char=? c #\?)
                   (conclude state3 (+ j 1) (+ j 1) 0))
                  ((char=? c #\#)
                   (conclude state4 (+ j 1) (+ j 1) 0))
                  ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state1 i (+ j 3) (+ n 1) last-colon-marker userinfo-idx)))
                  ((control-or-space-char? c strict)
                   (let ((authority (extract-string i j n)))
                     (and authority
                          (begin
                            (uri-authority-set! uri authority)
                            j))))
                  ((char=? c #\@)
                   ;; All up until this was userinfo
                   (state1 i (+ j 1) (+ n 1) last-colon-marker (cons j n)))
                  ((char=? c #\:)
                   ;; Set last colon marker
                   (state1 i (+ j 1) (+ n 1) (cons j n) userinfo-idx))
                  (else
                   (state1 i (+ j 1) (+ n 1) last-colon-marker userinfo-idx))))
          (conclude (lambda (new-i new-j new-n) j)
                    0 0 0)))
    
    ;; inside the "path" part
    (define (state2 i j n)
      ;; (dbg "State2 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (if (< j end)
          (let ((c (string-ref str j)))
            ;; (dbg "state2 processes char " c)
            (cond ((char=? c #\?)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            ;; (dbg "state2 concluded path to " path)
                            (uri-path-set! uri path)
                            (state3 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\#)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            ;; (dbg "state2 concluded path to " path)
                            (uri-path-set! uri path)
                            (state4 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state2 i (+ j 3) (+ n 1))))
                  ((control-or-space-char? c strict)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            ;; (dbg "state2 concluded path to " path)
                            (uri-path-set! uri path)
                            j))))
                  (else
                   (state2 i (+ j 1) (+ n 1)))))
          (let ((path (extract-string i j n)))
            (and path
                 (begin
                   ;; (dbg "state2 concluded path to " path)
                   (uri-path-set! uri path)
                   j)))))
    
    ;; inside the "query" part
    (define (state3 i j n)
      ;;(dbg "State3 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (if (< j end)
          (let ((c (string-ref str j)))
            ;; (dbg "state3 processes char " c)
            (cond ((char=? c #\#)
                   (let ((query (extract-query i j n)))
                     (and query
                          (begin
                            (uri-query-set! uri query)
                            (state4 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state3 i (+ j 3) (+ n 1))))
                  ((control-or-space-char? c strict)
                   (let ((query (extract-query i j n)))
                     (and query
                          (begin
                            (uri-query-set! uri query)
                            j))))
                  (else
                   (state3 i (+ j 1) (+ n 1)))))
          (let ((query (extract-query i j n)))
            (and query
                 (begin
                   ;;(dbg "state3 concluded query to " query)
                   (uri-query-set! uri query)
                   j)))))

    ;; inside the "fragment" part
    (define (state4 i j n)
      ;; (dbg "State4 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (if (< j end)
          (let ((c (string-ref str j)))
            ;; (dbg "state4 processes char " c)
            (cond ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state4 i (+ j 3) (+ n 1))))
                  ((control-or-space-char? c strict)
                   (let ((fragment (extract-string i j n)))
                     (and fragment
                          (begin
                            (uri-fragment-set! uri fragment)
                            j))))
                  (else
                   (state4 i (+ j 1) (+ n 1)))))
          (let ((fragment (extract-string i j n)))
            (and fragment
                 (begin
                   ;;(dbg "state4 concluded fragment to " fragment)
                   (uri-fragment-set! uri fragment)
                   j)))))
  
    (let ((i (state0 start start 0)))
      ;;(dbg "state0 returned " i)
      (cont (and i uri)
            (or i start)))))

(define (parse-uri-query str start end decode? cont #!optional (strict #t))
  (let ((rev-bindings '()))
    
    (define (extract-string i j n)
      (let ((returnvalue
             (if decode?
                 (extract-escaped str i n)
                 (substring str i j))))
        ;;(dbg "extract-string returns \"" returnvalue "\".")
        returnvalue))
    
    (define (state0 i j n)
      (if (< j end)
          (let ((c (string-ref str j)))
            (cond ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state0 i
                                (+ j 3)
                                (+ n 1))))
                  ((char=? c #\=)
                   (let ((name (extract-string i j n)))
                     (and name
                          (let ((j (+ j 1)))
                            (state1 j
                                    j
                                    0
                                    name)))))
                  ((char=? c #\&)
                   (set! rev-bindings
                         (cons (cons (extract-string i j n) #f) rev-bindings))
                   (let ((nj (+ j 1)))
                     (state0 nj nj 0)))
                  ((excluded-char? c)
                   (if (= n 0)
                       j
                       #f))
                  (else
                   (state0 i
                           (+ j 1)
                           (+ n 1)))))
          (begin
            (set! rev-bindings
                  (cons (cons (extract-string i j n) #f) rev-bindings))
            j)))
    
    (define (state1 i j n name)
      (if (< j end)
          (let ((c (string-ref str j)))
            (cond ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state1 i
                                (+ j 3)
                                (+ n 1)
                                name)))
                  ((char=? c #\&)
                   (let ((val (extract-string i j n)))
                     (and val
                          (let ((j (+ j 1)))
                            (set! rev-bindings
                                  (cons (cons name val) rev-bindings))
                            (and (< j end)
                                 (state0 j
                                         j
                                         0))))))
                  ((and strict (char=? c #\=) #f))
                  ((excluded-char? c)
                   (let ((val (extract-string i j n)))
                     (and val
                          (begin
                            (set! rev-bindings
                                  (cons (cons name val) rev-bindings))
                            j))))
                  (else
                   (state1 i
                           (+ j 1)
                           (+ n 1)
                           name))))
          (let ((val (extract-string i j n)))
            (and val
                 (begin
                   (set! rev-bindings
                         (cons (cons name val) rev-bindings))
                   j)))))
    
    (let* ((i (state0 start start 0))
           (returnvalue
            (cont (and i (reverse rev-bindings))
                  (or i start))))
      ;; (dbg "parse-uri-query returns " returnvalue)
      returnvalue)))

(define (string->uri str #!optional (decode? #t) (strict #t))
  (parse-uri str
             0
             (string-length str)
             decode?
             (lambda (uri end)
               (and (= end (string-length str))
                    uri))
             strict))

(define (uri->string uri)
  (with-output-to-string
    ""
    (lambda ()
      (let ((scheme (uri-scheme uri))
            (authority (uri-authority uri))
            (path (uri-path uri))
            (query (uri-query uri))
            (fragment (uri-fragment uri)))
        (if scheme
            (begin
              (display scheme)
              (display ":")))
        (if authority
            (let ((ui (uri-userinfo uri))
                  (p (uri-port uri)))
              (display "//")
              (if ui
                  (begin
                    (display ui)
                    (display "@")))
              (display (uri-host uri))
              (if p
                  (begin
                    (display ":")
                    (display p)))))
        (if path
            (display path))

        (if (or (string? query)
                (pair? query))
            (begin
              (display "?")
              (display (uri-query-string uri))))

        (if fragment
            (begin
              (display "#")
              (display fragment)))))))

(define (string->uri-query str #!optional (decode? #t))
  (parse-uri-query str
                   0
                   (string-length str)
                   decode?
                   (lambda (query end)
                     (and (= end (string-length str))
                          query))))

(define (encode-for-uri str)
  (let ((end (string-length str)))
    
    (define (copy result i j n)
      (if (< i j)
          (let ((new-j (- j 1))
                (new-n (- n 1)))
            (string-set! result new-n (string-ref str new-j))
            (copy result i new-j new-n))
          result))
    
    (define (hex x)
      (string-ref "0123456789ABCDEF" (bitwise-and x 15)))
    
    (define (encode i j n)
      (if (< j end)
          (let ((c (string-ref str j)))
            (cond ((char=? c #\space)
                   (let ((result (encode (+ j 1) (+ j 1) (+ n 1))))
                     (string-set! result n #\+)
                     (copy result i j n)))
                  ((or (char=? c #\+)
                       (excluded-char? c))
                   (let ((result (encode (+ j 1) (+ j 1) (+ n 3))))
                     (let* ((x (char->integer c))
                            (hi (hex (arithmetic-shift x -4)))
                            (lo (hex x)))
                       (string-set! result n #\%)
                       (string-set! result (+ n 1) hi)
                       (string-set! result (+ n 2) lo))
                     (copy result i j n)))
                  (else
                   (encode i (+ j 1) (+ n 1)))))
          (let ((result (make-string n)))
            (copy result i j n))))
    
    (encode 0 0 0)))

;; Removes extraneous "./" and "../" in a URI path. See section 5.2.4
;; of RFC 3986.
(define (remove-dot-segments str)
  (let* ((in-len (string-length str))
         (res (make-string in-len)))
    ;; i is where we are in the source string,
    ;; j is where we are in the result string,
    ;; segs is a list, used as a stack, of the indices of the
    ;; previously encountered path segments in the result string.
    (letrec
        ((new-segment
          (lambda (i j segs)
            (let* ((segment-start (car segs))
                   (segment-length (- j segment-start 1)))
              (cond
               ;; Check for .
               ((and (= 1 segment-length)
                     (char=? #\. (string-ref res segment-start)))
                (loop (+ 1 i) segment-start segs))

               ;; Check for ..
               ((and (= 2 segment-length)
                     (char=? #\. (string-ref res segment-start))
                     (char=? #\. (string-ref res (+ 1 segment-start))))
                (cond
                 ;; Take care of the "/../something" special case; it
                 ;; should return "/something" and not "something".
                 ((and (= 1 segment-start)
                       (char=? #\/ (string-ref res 0)))
                  (loop (+ 1 i) 1 '(1)))
                 
                 ;; This is needed because the code in the else clause
                 ;; assumes that segs is a list of length >= 2
                 ((zero? segment-start)
                  (loop (+ 1 i) 0 segs))

                 (else
                  (loop (+ 1 i) (cadr segs) (cdr segs)))))
               
               ;; Check for the end of the string
               ((>= (+ 1 i) in-len)
                j)

               (else
                (loop (+ 1 i) j (cons j segs)))))))
         (loop
          (lambda (i j segs)
            (if (>= i in-len)
                (new-segment i j segs)
                (let ((chr (string-ref str i)))
                  (string-set! res j chr)
                  (if (char=? chr #\/)
                      (new-segment i (+ 1 j) segs)
                      (loop (+ 1 i) (+ 1 j) segs)))))))
      (let ((idx (loop 0 0 '(0))))
        (substring res 0 idx)))))

;; Makes an absolute uri from ref. See section 5.2 of RFC 3986.
(define (uri-join base ref)
  (cond
   ((uri-scheme ref)
    (make-uri (uri-scheme ref)
              (uri-authority ref)
              (remove-dot-segments (uri-path ref))
              (uri-query ref)
              (uri-fragment ref)))

   ((uri-authority ref)
    (make-uri (uri-scheme base)
              (uri-authority ref)
              (remove-dot-segments (uri-path ref))
              (uri-query ref)
              (uri-fragment ref)))

   ((let ((path (uri-path ref)))
      (and path (positive? (string-length path))))
    (make-uri (uri-scheme base)
              (uri-authority base)
              (let ((path (uri-path ref))
                    (base-path (uri-path base)))
                (cond
                 ;; If path begins with /
                 ((char=? #\/ (string-ref path 0))
                  (remove-dot-segments path))

                 ;; The following two cond clauses are the
                 ;; implementation of section 5.2.3 in RFC 3986.
                 ((and (uri-authority base)
                       (or (not base-path)
                           (= 0 (string-length base-path))))
                  (string-append "/" path))

                 (else
                  (string-append
                   (substring base-path
                              0
                              (+ 1
                                 (or (string-index-right
                                      base-path
                                      #\/)
                                     -1)))
                   path))))
              (uri-query ref)
              (uri-fragment ref)))

   (else
    (make-uri (uri-scheme base)
              (uri-authority base)
              (uri-path base)
              (or (uri-query ref)
                  (uri-query base))
              (uri-fragment ref)))))

(define (uri-join* base ref)
  (uri-join (string->uri base)
            (string->uri ref)))
