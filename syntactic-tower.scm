;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                 Syntactic tower data structure                   ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;


(define-type syntactic-tower
  id: 06C5040E-666F-4744-91AF-FAA356820340
  constructor: make-syntactic-tower/internal

  phases)

(define (make-syntactic-tower)
  (make-syntactic-tower/internal
   (vector (make-table) (make-table))))

(define (syntactic-tower-phase tower phase)
  (let* ((phases (syntactic-tower-phases tower))
         (number-of-phases (vector-length phases)))
    (cond
     ((< phase 1)
      (error "Invalid parameter"))

     ((> phase number-of-phases)
      (let ((new-phases (make-vector phase)))
        (let loop ((i 0))
          (cond
           ((< i number-of-phases)
            (vector-set! new-phases i (vector-ref phases i))
            (loop (+ 1 i)))
           ((< i phase)
            (vector-set! new-phases i (make-table))
            (loop (+ 1 i)))))
        (syntactic-tower-phases-set! tower new-phases)
        (syntactic-tower-phase tower phase)))

     (else
      (vector-ref phases (- phase 1))))))

(define (syntactic-tower-module-instance tower phase absolute-path)
  (table-ref (syntactic-tower-phase tower phase) absolute-path))

(define repl-syntactic-tower (make-syntactic-tower))
