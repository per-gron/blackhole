(import ../64)
(import ../61)

(test-begin "srfi-61" 1)

;; From SRFI-61 document
(define (port->char-list port)
  (cond
    ((read-char port) char?
     => (lambda (c) (cons c (port->char-list port))))
    (else '())))

(test-assert
  (port->char-list (open-input-file "61.scm")))

(test-end "srfi-61")
