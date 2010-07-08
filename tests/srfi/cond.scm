(import (srfi tests cond))

(test-begin "srfi-61")

(define this-file "61.scm")

(test-assert #t) ; Bug in srfi-64? Test must start with an assert

;; From SRFI-61 document
(define (port->char-list-r5rs port)
  (cond
    ((read-char port) char?
     => (lambda (c) (cons c (port->char-list-r5rs port))))
    (else '())))

(test-error "r5rs-cond"
  (test-read-eval-string
    "(port->char-list-r5rs (open-input-file this-file))"))

(import ../61)

(define (port->char-list-srfi port)
  (cond
    ((read-char port) char?
     => (lambda (c) (cons c (port->char-list-srfi port))))
    (else '())))

;; Tests that it actually expands properly when this srfi is loaded

(test-assert "srfi-cond"
  (test-read-eval-string
    "(port->char-list-srfi (open-input-file this-file))"))

(test-end "srfi-61")
