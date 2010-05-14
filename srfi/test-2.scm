(import (std srfi/64))

(import ./2)

(test-begin "srfi-2" 28)

(test-equal  (and-let* () 1) 1)
(test-equal  (and-let* () 1 2) 2)
(test-equal  (and-let* () ) #t)

(test-equal (let ((x #f)) (and-let* (x))) #f)
(test-equal (let ((x 1)) (and-let* (x))) 1)
(test-equal (and-let* ((x #f)) ) #f)
(test-equal (and-let* ((x 1)) ) 1)
;(test-error (and-let* ( #f (x 1))) )
(test-equal (and-let* ( (#f) (x 1)) ) #f)
;(test-error (and-let* (2 (x 1))) )
(test-equal (and-let* ( (2) (x 1)) ) 1)
(test-equal (and-let* ( (x 1) (2)) ) 2)
(test-equal (let ((x #f)) (and-let* (x) x)) #f)
(test-equal (let ((x "")) (and-let* (x) x)) "")
(test-equal (let ((x "")) (and-let* (x)  )) "")
(test-equal (let ((x 1)) (and-let* (x) (+ x 1))) 2)
(test-equal (let ((x #f)) (and-let* (x) (+ x 1))) #f)
(test-equal (let ((x 1)) (and-let* (((positive? x))) (+ x 1))) 2)
(test-equal (let ((x 1)) (and-let* (((positive? x))) )) #t)
(test-equal (let ((x 0)) (and-let* (((positive? x))) (+ x 1))) #f)
(test-equal (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1)))  3)
#;(test-error
  #t
    (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
    )

(test-equal (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))) 2)
(test-equal (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) 2)
(test-equal (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))) #f)
(test-equal (let ((x #f)) (and-let* (x ((positive? x))) (+ x 1))) #f)
(test-equal (let ((x #f)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) #f)

(test-equal  (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(test-equal  (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(test-equal  (let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(test-equal  (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) 3/2)

(test-end "srfi-2")
