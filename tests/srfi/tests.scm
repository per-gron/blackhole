(import (srfi tests))

(define my-simple-runner (test-runner-simple))
(test-runner-factory
  (lambda () my-simple-runner))

(test-begin "simple-runner")

(test-assert #t)
(test-equal 0 0)
(test-approximate 0.01 0.02 0.1)
(test-error (error "error"))
;(test-error #t) ; This is commented because it should fail!

(test-end "simple-runner")
