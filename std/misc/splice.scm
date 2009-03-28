(export splice
        splice?
        (rename: (splice-data unsplice))
        unsplice-list)

(define-type splice
  id: BFC5FE93-402B-4B7A-881E-8E864E6ED184
  (data read-only:))

(define (splice . args)
  (make-splice args))

(define (unsplice-list list)
  (cond
   ((and (pair? list)
         (splice? (car list)))
    (append (splice-data (car list))
            (unsplice-list (cdr list))))

   ((splice? list)
    (splice-data list))

   ((pair? list)
    (cons (car list)
          (unsplice-list (cdr list))))

   (else
    list)))
