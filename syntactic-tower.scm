;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                 Syntactic tower data structure                   ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;


(define-type expansion-phase
  id: 73B5764F-51BF-4959-8A5A-C65667253AA5
  constructor: make-expansion-phase/internal

  (syntactic-tower read-only:)
  ;; An integer
  (number read-only:)
  ;; A hashtable of an absolute module reference to a module instance object
  (module-instances read-only:))

(define (make-expansion-phase tower phase)
  (make-expansion-phase/internal tower
                                 phase
                                 (and (positive? phase)
                                      (make-table))))

(define-type syntactic-tower
  id: 06C5040E-666F-4744-91AF-FAA356820340
  constructor: make-syntactic-tower/internal

  (phases init: (vector)))

(define (make-syntactic-tower)
  (let ((tower (make-syntactic-tower/internal)))
    (syntactic-tower-phases-set!
     (vector (make-expansion-phase tower 0)
             (make-expansion-phase tower 1)))
    tower))

(define (syntactic-tower-phase tower phase)
  (let* ((phases (syntactic-tower-phases tower))
         (number-of-phases (vector-length phases)))
    (cond
     ((negative? phase)
      (error "Invalid parameter"))

     ((> phase number-of-phases)
      (let ((new-phases (make-vector (+ 1 phase))))
        (let loop ((i 0))
          (cond
           ((< i number-of-phases)
            (vector-set! new-phases i (vector-ref phases i))
            (loop (+ 1 i)))
           ((<= i phase)
            (vector-set! new-phases i (make-expansion-phase tower i))
            (loop (+ 1 i)))))
        (syntactic-tower-phases-set! tower new-phases)
        (syntactic-tower-phase tower phase)))

     (else
      (vector-ref phases phase)))))

(define (syntactic-tower-first-phase tower)
  (syntactic-tower-phase tower 0))

(define (expansion-phase-runtime? phase)
  (zero? (expansion-phase-number phase)))

(define (expansion-phase-compiletime? phase)
  (positive? (expansion-phase-number phase)))

(define (expansion-phase-next-phase phase)
  (syntactic-tower-phase (expansion-phase-syntactic-tower phase)
                         (+ 1 (expansion-phase-number phase))))

(define (expansion-phase-module-instance phase module-reference)
  (table-ref (expansion-phase-module-instances phase)
             module-reference))

(define repl-syntactic-tower (make-syntactic-tower))
