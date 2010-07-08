(import (srfi tests hash-tables))
(import ../69)

(test-begin "srfi-69" 5)

(let ((ht (make-hash-table)))
  (hash-table-set! ht 0 'element)

  (test-equal
    (hash-table-ref ht 0 (lambda () #f))
    'element)
  
  (hash-table-update! ht 
                      0
                      (lambda (x) 'updated))

  (test-equal
    (hash-table-ref ht 0 (lambda () #f))
    'updated)

  (hash-table-delete! ht 0)
  (test-error
    (hash-table-ref ht 0)))


(let* ((al `((a "uno")
             (b "dos")
             (c "tres")
             (d "cuatro")
             (e "cinco")
             (f "seis")))
       (ht (alist->hash-table al equal?)))

  (test-equal
    (car (hash-table-ref ht 'e))
    "cinco")

  (hash-table-set! ht 'e "five")

  (test-equal
    (hash-table-ref ht 'e)
    "five"))

(test-end "srfi-69")
