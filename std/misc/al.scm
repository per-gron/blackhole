;;; A-list utilities
;;;
;;; Copyright (c) 2008 Per Eckerdal, Mikael Möre

(export al-get
        al
        al-set!
        al-set!-dfl)

(define (al-helper . args)
  (if (null? args)
      '()
      (let ((key (car args))
            (val (cadr args))
            (rest (cddr args)))
        (cons (cons key val)
              (apply al-helper rest)))))

(define (al-get lst key #!optional (dfl #f))
  (let ((pair (assoc key lst)))
    (if pair
        (cdr pair)
        dfl)))

(define-syntax al
  (sc-macro-transformer
   (lambda (form env)
     (let* ((args (cdr form))
            (nargs
             (let loop ((a args))
               (cond
                ((null? a) '())
                ((pair? (car a))
                 `(',(caar a)
                   (lambda ,(cdar a) ,(cadr a))
                   ,@(loop (cddr a))))
                (else `(',(car a) ,(cadr a) ,@(loop (cddr a))))))))
       `(al-helper
         ,@(map (lambda (x)
                  (make-syntactic-closure env '() x))
                nargs))))))

;; Set key to value in alist al.
;; Replace al with the return value on return.
(define (al-set! al key value)
  (let ((v (or (assoc key al) (let ((v (cons key #f))) (set! al (cons v al)) v))))
    (set-cdr! v value)
    al))

(define (al-set!-dfl al key value)
  (let* ((not-set '(not-set))
         (v (al-get al key not-set)))
    (if (eq? v not-set)
        (al-set! al key value)
        al)))

