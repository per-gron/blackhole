;; This should generate an error message:
(expand-macro '(let ((a 5) (a 6)) a))

;; When you import the macro mac in this code, inner-mac will not be
;; available
(let-syntax
    ((inner-mac
      (syntax-rules ()
        ((inner-mac) #t))))
  (define-syntax mac
    (syntax-rules ()
      ((mac) (inner-mac)))))
