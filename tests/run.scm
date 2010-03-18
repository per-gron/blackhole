(define (test #!optional (file "units.scm"))
  (let ((test-expressions
         (with-input-from-file file
           (lambda () (read-all))))
        (success 0)
        (fail 0))
    (for-each
     (lambda (expression)
       (let ((result (eval expression)))
         (cond
          ((eq? result #t)
           (set! success (+ 1 success)))
          
          (else
           (set! fail (+ 1 fail))
           (println "  Test number " (+ fail success) " failed:")
           (write expression)
           (println "\n  With result:")
           (write result)
           (println)))))
     test-expressions)
    
    (println "\nRan " (+ fail success) " tests. " fail " tests failed.")))
