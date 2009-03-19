;; Utilities

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

(define (flatten list)
  (cond ((null? list) '())
        ((list? (car list))
         (append (flatten (car list))
                 (flatten (cdr list))))
        (else
         (cons (car list)
               (flatten (cdr list))))))

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

(define (last lst)
  (cond ((null? lst) #f)
        ((null? (cdr lst)) (car lst))
        (else (last (cdr lst)))))

;; Takes an expression of the form ((mname . arglist) . funbody)
;; and transforms it into (mname (lambda arglist funbody))
;; (it does nothing if the input is already in the latter form)
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

;; Helper for the define-type macro in build#.scm
(define (expand . args)
  (let* ((exp (cdr (apply ##define-type-expand args))))
    `(begin
       ,@(map (lambda (x)
                (if (eq? (car x) '##define-macro)
                    (cons 'define-macro
                          (if (eq? (caaddr x)
                                   '##define-type-expand)
                              `(,(cadr x)
                                (build#expand
                                 ,@(cdaddr x)))
                              (cdr x)))
                    x))
              exp))))

(define (eval-no-hook expr)
  (let ((hook ##expand-source))
    (set! ##expand-source (lambda (src) src))
    (let ((ret (eval expr)))
      (set! ##expand-source hook)
      ret)))

;; Taken from Christian Jaeger
(define (defined? a)
  (if (symbol? a)
      (with-exception-catcher
       (lambda (e)
         ;; other exceptions shouldn't happen
         ;; really, but who knows
         (if (unbound-global-exception? e)
             #f
             (error "unexpected exception" e)))
       (lambda ()
         ;; TODO Should eval-no-hook be used?
         (eval a) ;; throws exception if unbound
         #t))
      (error "Expected symbol argument" a)))