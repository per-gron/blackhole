; Sebastian.Egner@philips.com, 3-Jun-2002.
;; 
;; Adapted to Blackhole for Gambit by Álvaro Castro-Castilla
;; Uses srfi-64 for testing

(import (srfi tests))
(import (srfi specialize-procedures))

(test-begin "srfi-26" 25)

(test-equal ((cut list)) '())
(test-equal ((cut list <...>)) '())
(test-equal ((cut list 1)) '(1))
(test-equal ((cut list <>) 1) '(1))
(test-equal ((cut list <...>) 1) '(1))
(test-equal ((cut list 1 2)) '(1 2))
(test-equal ((cut list 1 <>) 2) '(1 2))
(test-equal ((cut list 1 <...>) 2) '(1 2))
(test-equal ((cut list 1 <...>) 2 3 4) '(1 2 3 4))
(test-equal ((cut list 1 <> 3 <>) 2 4) '(1 2 3 4))
(test-equal ((cut list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
(test-equal (let* ((x 'wrong) (y (cut list x))) (set! x 'ok) (y)) '(ok))
(test-equal 
  (let ((a 0))
    (map (cut + (begin (set! a (+ a 1)) a) <>)
         '(1 2))
    a)
  2)
(test-equal ((cute list)) '())
(test-equal ((cute list <...>)) '())
(test-equal ((cute list 1)) '(1))
(test-equal ((cute list <>) 1) '(1))
(test-equal ((cute list <...>) 1) '(1))
(test-equal ((cute list 1 2)) '(1 2))
(test-equal ((cute list 1 <>) 2) '(1 2))
(test-equal ((cute list 1 <...>) 2) '(1 2))
(test-equal ((cute list 1 <...>) 2 3 4) '(1 2 3 4))
(test-equal ((cute list 1 <> 3 <>) 2 4) '(1 2 3 4))
(test-equal ((cute list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
(test-equal 
  (let ((a 0))
    (map (cute + (begin (set! a (+ a 1)) a) <>)
         '(1 2))
    a)
  1)

(test-end "srfi-26")
