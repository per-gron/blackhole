(import (only: (module) identifier?)) ;; TODO It might be good to
                                      ;; remove this dependency

(export match)

(define (pattern-match-helper pattern message)
  (cond
   ((identifier? pattern)
    (list message))
   
   ((and (or (string? pattern)
             (null? pattern)
             (boolean? pattern)
             (number? pattern))
         (equal? message pattern))
    '())
   
   ((and (pair? pattern)
         (eq? 'quote (car pattern))
         (eq? message (cadr pattern)))
    '())

   ((and (pair? pattern)
         (pair? message))
    (let loop ((p pattern) (m message))
      (cond
       ((and (null? p)
             (null? m))
        '())

       ((and (pair? p)
             (pair? m))
        (let ((res (pattern-match-helper
                    (car p)
                    (car m))))
          (and res
               (append
                res
                (loop (cdr p)
                      (cdr m))))))

       ((and (not (pair? p))
             (not (pair? m)))
        (pattern-match-helper p m))

       (else
        #f))))

   (else
    #f)))


(syntax-begin
 
 (define (pattern-match-param-list mac-env env pattern)
   (cond
    ((identifier? pattern)
     (list pattern))
    
    ((and (pair? pattern)
          (identifier=? mac-env 'quote env (car pattern)))
     '())
    
    ((pair? pattern)
     (append (pattern-match-param-list mac-env env (car pattern))
             (pattern-match-param-list mac-env env (cdr pattern))))
    
    (else
     '())))

 (define (pattern-match-make-lambda mac-env env pattern . body)
   `(,(make-syntactic-closure mac-env '() 'lambda)
     ,(pattern-match-param-list mac-env env pattern)
     ,@body)))

(define-syntax match-lambda
  (sc-macro-transformer
   (lambda (form env)
     (capture-syntactic-environment
      (lambda (mac-env)
        (apply pattern-match-make-lambda
               (cons mac-env
                     (cons env
                           (cdr form)))))))))

(define-syntax match-inner
  (syntax-rules ()
    ((match var)
     (error "Failed to match " var))

    ((match var (pattern body ...) rest ...)
     (let ((res (pattern-match-helper 'pattern var)))
       (if res
           (apply (match-lambda pattern body ...)
                  res)
           (match var rest ...))))))

(define-syntax match
  (syntax-rules ()
    ((match var rest ...)
     (let ((x var))
       (match-inner x rest ...)))))

