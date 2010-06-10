(import ../64)
(import ../11)

(test-begin "srfi-11" 3)

(test-equal
  (let-values (((a b) (values 1 2))
			   ((c d) (values 3 4)))
	(list a b c d))
  (list 1 2 3 4))

(test-error
  (let-values (((a b) (values 1 2))
			   ((c d e) (values a b 3)))
	(list a b c d e)))

(test-equal
  (let*-values (((a b) (values 1 2))
			    ((c d e) (values a b 3)))
	(list a b c d e))
  (list 1 2 1 2 3))

(test-end "srfi-11")
