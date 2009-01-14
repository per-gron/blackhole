
(let-syntax
    ((inner-mac
      (syntax-rules ()
        ((inner-mac) #t))))
  (define-syntax mac
    (syntax-rules ()
      ((mac) (inner-mac)))))
