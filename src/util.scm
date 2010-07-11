;; Utilities

(##define-syntax get-path
  (lambda (a)
    (vector-ref a 2)))

(define (string-contains haystack chr)
  (call/cc
   (lambda (ret)
     (let ((strlen (string-length haystack)))
       (let loop ((i 0))
         (if (>= i strlen)
             (ret #f)
             (let ((c (string-ref haystack i)))
               (if (eq? c chr)
                   (ret i)
                   (loop (+ i 1))))))))))

(define (string-ends-with haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (and (>= hlen nlen)
         (equal? needle
                 (substring haystack (- hlen nlen) hlen)))))

(define (string-begins-with haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (and (>= hlen nlen)
         (equal? needle
                 (substring haystack 0 nlen)))))

(define (string-remove-suffix haystack needle)
  (if (string-ends-with haystack needle)
      (substring haystack 0 (- (string-length haystack)
                               (string-length needle)))
      haystack))

(define (string-remove-prefix haystack needle)
  (if (string-begins-with haystack needle)
      (substring haystack
                 (string-length needle)
                 (string-length haystack))
      haystack))

(define-macro (push! list obj)
  `(set! ,list (cons ,obj ,list)))

(define-macro (pop! list)
  ;; We don't need to worry about double-evaluating list, because it
  ;; has to be a simple identifier anyways or the set! won't work.
  (let ((tmp (gensym 'tmp)))
    `(let* ((,tmp (car ,list)))
       (set! ,list (cdr ,list))
       ,tmp)))

;; TODO As an optimization, implement this
(define reverse! reverse)

(define (file-last-changed-seconds fn)
  (time->seconds
   (file-info-last-change-time
    (file-info fn))))

(define (file-newer? a b)
  (with-exception-catcher
   (lambda (e)
     #f)
   (lambda ()
     (> (file-last-changed-seconds a)
        (file-last-changed-seconds b)))))

;; I have no idea whether this works on non-Unix environments.
;; I don't care right now.
(define (is-directory? dir)
  (file-exists? (string-append dir "/")))

;; This probably won't work on non-Unix environments.
;; I don't care right now.
(define (path-absolute? path)
  (and (string? path)
       (> (string-length path) 0)
       (or (positive? (string-length (path-volume path)))
           (eq? #\\ (string-ref path 0))
           (eq? #\/ (string-ref path 0)))))

(define (recursively-delete-file dir)
  (if (is-directory? dir)
      (begin
        (for-each (lambda (fn)
                    (recursively-delete-file
                     (path-expand fn dir)))
                  (directory-files
                   (list path: dir
                         ignore-hidden: 'dot-and-dot-dot)))
        (delete-directory dir))
      (delete-file dir)))

;; TODO This is probably very slow
(define (flatten list)
  (cond ((null? list) '())

        ((null? (car list))
         (flatten (cdr list)))
        
        ((pair? (car list))
         (append (flatten (car list))
                 (flatten (cdr list))))
        (else
         (cons (car list)
               (flatten (cdr list))))))

;; TODO This is probably very slow
(define (flatten1 list)
  (cond
   ((null? list)
    '())
   (else
    (append (car list)
            (flatten1 (cdr list))))))

(define (remove! pred list)
  (cond
   ((null? list) '())

   (else
    (if (pred (car list))
        (remove! pred (cdr list))
        (let ((return list))
          (let loop ((list list))
            (cond
             ((null? list)
              return)
             
             ((and (pair? (cdr list))
                   (pred (cadr list)))
              (set-cdr! list
                        (cddr list))
              (loop (cdr list)))
             
             (else
              (loop (cdr list))))))))))
           

;; Recursively search directories after files with a certain extension
(define (find-files-with-ext ext dir #!optional prefix)
  (let ((prefix (or prefix "")))
    (flatten
     (map (lambda (f)
            (let ((full-fn (string-append dir "/" f)))
              (cond
               ((is-directory? full-fn)
                (find-files-with-ext
                 ext full-fn (string-append prefix f "/")))
               ((string-ends-with f ext)
                (string-append prefix f))
               (else '()))))
          (directory-files dir)))))

;; Like find-files-with-ext, but removes the extension
(define (find-files-with-ext-remove-ext ext dir)
  (map (lambda (a)
         (string->symbol
          (string-append "/"
                         (string-remove-suffix a ext))))
       (find-files-with-ext ext dir)))

(define (filter pred list)
  (if (null? list)
      '()
      (if (pred (car list))
          (cons (car list) (filter pred (cdr list)))
          (filter pred (cdr list)))))

(define (find pred lst)
  (let loop ((lst lst))
    (cond
     ((pair? lst)
      (let ((hd (car lst)))
        (if (pred hd)
            hd
            (loop (cdr lst)))))

     (else
      #f))))

(define (vector-for-each fn vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (cond
       ((< i len)
        (fn (vector-ref vec i))
        (loop (+ 1 i)))))
    (void)))

(define (vector-map fn vec)
  (let* ((len (vector-length vec))
         (v (make-vector len)))
    (let loop ((i 0))
      (cond
       ((< i len)
        (vector-set! v
                     i
                     (fn (vector-ref vec i)))
        (loop (+ 1 i)))))
    v))

(define (last lst)
  (cond ((null? lst) #f)
        ((null? (cdr lst)) (car lst))
        (else (last (cdr lst)))))

;; Takes an expression of the form ((mname . arglist) . funbody)
;; and transforms it into (mname (lambda arglist funbody))
;; (it does nothing if the input is already in the latter form)
;;
;; TODO This is obsoleted by expr*:transform-to-lambda.
(define (transform-to-lambda expr #!optional (lmb 'lambda))
  (if (pair? (car expr))
      `(,(caar expr)
        (,lmb ,(cdar expr) ,@(cdr expr)))
      expr))

;; Takes a module name and a symbol. If symbol contains a #, just
;; the symbol is returned. Otherwise mod#sym is returned.
(define (absolutify mod sym)
  (if (not mod)
      sym
      (let ((symstr (symbol->string sym)))
        (if (string-contains symstr #\#)
            sym
            (string->symbol
             (if (symbol? mod)
                 (string-append
                  (symbol->string mod)
                  "#"
                  symstr)
                 (string-append mod symstr)))))))

;; Takes an expression of the form (name (lambda arglist . body))
;; and transforms it into (name (lambda arglist [add...] . body))
(define (add-at-beginning-of-lambda lm . add)
  (let ((lme (cadr lm)))
    (if (pair? lme)
        (let ((args (cadr lme))
              (rest (cddr lme)))
          `(,(car lm) (lambda ,args ,@add (let () ,@rest))))
        lm)))

(define (delete-if-exists fn)
  (if (file-exists? fn)
      (delete-file fn)))

;; (This function's implementation is quite ugly I think)
;; Flattens nested begin expressions to one;
;; (begin (begin #f) #f) => (begin #f #f)
(define (flatten-begin exp)
  (cond
   ((or (null? exp) (not (list? exp))) exp)
   ((eq? (car exp) 'begin)
    (let ((r (map flatten-begin (cdr exp))))
      `(begin
         ,@(apply
            append
            (map (lambda (x)
                   (if (and (list? x)
                            (not (null? x))
                            (eq? (car x) 'begin))
                       (cdr x)
                       (list x)))
                 r)))))
   (else exp)))

;; Helper for the define-type macro
(define (expand . args)
  (let* ((exp (cdr (apply ##define-type-expand args))))
    `(begin
       ,@(map (lambda (x)
                (if (eq? (car x) '##define-macro)
                    (cons 'define-macro
                          (if (eq? (caaddr x)
                                   '##define-type-expand)
                              `(,(cadr x)
                                (bh#expand
                                 ,@(cdaddr x)))
                              (cdr x)))
                    x))
              exp))))

;; Helper for cond-expand. This function is more or less copied from
;; Gambit's _nonstd.scm
(define (cond-expand-build src clauses features)
  (define (satisfied? feature-requirement)
    (cond ((##symbol? feature-requirement)
           (if (##member feature-requirement features)
             #t
             #f))
          ((##pair? feature-requirement)
           (let ((first (##source-strip (##car feature-requirement))))
             (cond ((##eq? first 'not)
                    (##shape src (##sourcify feature-requirement src) 2)
                    (##not (satisfied?
                            (##source-strip (##cadr feature-requirement)))))
                   ((or (##eq? first 'and) (##eq? first 'or))
                    (##shape src (##sourcify feature-requirement src) -1)
                    (let loop ((lst (##cdr feature-requirement)))
                      (if (##pair? lst)
                        (let ((x (##source-strip (##car lst))))
                          (if (##eq? (satisfied? x) (##eq? first 'and))
                            (loop (##cdr lst))
                            (##not (##eq? first 'and))))
                        (##eq? first 'and))))
                   (else
                    (error "Ill-formed cond-expand form"
                           (expr*:strip-locationinfo src))))))
          (else
           (error "Ill-formed cond-expand form"
                  (expr*:strip-locationinfo src)))))

  (define (build clauses)
    (if (##pair? clauses)
      (let ((clause (##source-strip (##car clauses))))
        (##shape src (##sourcify clause src) -1)
        (let ((feature-requirement (##source-strip (##car clause))))
          (if (or (and (##eq? feature-requirement 'else)
                       (##null? (##cdr clauses)))
                  (satisfied? feature-requirement))
            (##cons 'begin (##cdr clause))
            (build (##cdr clauses)))))
      (error "Unfulfilled cond-expand form"
             (expr*:strip-locationinfo src))))

  (build clauses))

(define (eval-no-hook expr)
  (let ((hook ##expand-source)
        (c-hook c#expand-source)

        (id (lambda (x) x)))
    (dynamic-wind
        (lambda ()
          (set! ##expand-source id)
          (set! c#expand-source id))
        (lambda ()
          (eval expr))
        (lambda ()
          (set! ##expand-source hook)
          (set! c#expand-source c-hook)))))

;; Beware of n^2 algorithms
(define (remove-duplicates list #!optional (predicate eq?))
  (cond
   ((null? list) '())
   ((pair? list)
    (let ((e (car list)))
      (cons e
            (remove-duplicates
             (filter (lambda (x)
                       (not (predicate x e)))
                     (cdr list))
             predicate))))
   (else (raise "Argument to remove-duplicates must be a list"))))

(define (create-dir-unless-exists dir)
  (if (not (file-exists? dir))
      (begin
        (create-dir-unless-exists
         (path-directory
          (path-strip-trailing-directory-separator dir)))
        (create-directory dir))))

(define (generate-tmp-dir base-dir thunk)
  (create-dir-unless-exists base-dir)
  (let ((fn (let loop ((i 0))
              (let ((fn (path-expand (number->string i)
                                     compile-tmp-dir)))
                (if (file-exists? fn)
                    (loop (+ i 1))
                    fn)))))
    (dynamic-wind
        (lambda ()
          (if (not fn)
              (error "generate-tmp-dir: Can't re-enter"))
          (create-directory fn))
        (lambda ()
          (thunk fn))
        (lambda ()
          (recursively-delete-file fn)
          (set! fn #f)))))

;; Let with multiple values support
(##define-syntax let
  (lambda (source)
    (define (last lst)
      (cond ((null? lst) #f)
            ((null? (cdr lst)) (car lst))
            (else (last (cdr lst)))))
    
    (define (skip-last lst)
      (cond
       ((null? lst)
        (error "Can't skip last"))
       
       ((null? (cdr lst))
        '())
       
       (else
        (cons (car lst)
              (skip-last (cdr lst))))))
    
    (define (filter pred list)
      (if (null? list)
          '()
          (if (pred (car list))
              (cons (car list) (filter pred (cdr list)))
              (filter pred (cdr list)))))

    (define (source-code source)
      (if (##source? source)
          (##source-code source)
          source))

    (##sourcify-deep
     (let* ((sc (source-code source))
            (defs (source-code (cadr sc)))
            (body (cddr sc)))
       (cond
        ((pair? defs)
         (let* ((defs (map source-code defs))
                (single-defs
                 (filter (lambda (x)
                           (null? (cddr x)))
                         defs))
                (multi-defs
                 (map (lambda (x)
                        (cons (last x)
                              (map (lambda (name)
                                     (cons (gensym (source-code
                                                    name))
                                           name))
                                (skip-last x))))
                   (filter (lambda (x)
                             (pair? (cddr x)))
                           defs))))
           (let loop ((mds multi-defs))
             (cond
              ((null? mds)
               `(##let (,@single-defs
                        ,@(apply
                           append
                           (map (lambda (multi-def)
                                  (map (lambda (def)
                                         (list (cdr def)
                                               (car def)))
                                    (cdr multi-def)))
                             multi-defs)))
                  ,@body))
              
              (else
               (let ((multi-def (car mds)))
                 `(call-with-values
                      (lambda ()
                        ,(car multi-def))
                    (lambda ,(map car (cdr multi-def))
                      ,(loop (cdr mds))))))))))
        
        (else
         (cons '##let
               (cdr (source-code source))))))
     source)))



;; Removes extraneous "./" and "../" in a URI path. Copied from the
;; uri module
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


