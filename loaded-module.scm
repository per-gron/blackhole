;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                    Loaded module data structure                  ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define-type loaded-module
  id: 08E43248-AC39-4DA1-863E-49AFA9FFE84E
  constructor: make-loaded-module/internal

  (instantiate-runtime read-only:)
  (runtime-instantiated init: #f)
  (instantiate-compiletime read-only:)
  (info read-only:)
  (stamp read-only:)
  ;; An absolute module reference object
  (reference read-only:))

(define (make-loaded-module #!key
                            instantiate-runtime
                            instantiate-compiletime
                            info
                            stamp
                            reference)
  (if (not (and (procedure? instantiate-runtime)
                (procedure? instantiate-compiletime)
                (module-info? info)
                (module-reference? reference)))
      (error "Invalid parameters"))
  (make-loaded-module/internal instantiate-runtime
                               instantiate-compiletime
                               info
                               stamp
                               reference))

(define (loaded-module-loader mod)
  (module-reference-loader (loaded-module-reference mod)))

(define (loaded-module-path mod)
  (module-reference-path (loaded-module-reference mod)))

(define *loaded-module-registry* (make-table))

(define (module-reference-load! ref)
  (call-with-values
      (lambda ()
        (loader-load-module (module-reference-loader ref)
                            (module-reference-path path)))
    (lambda (instantiate-runtime
             instantiate-compiletime
             module-info
             stamp)
      (make-loaded-module instantiate-runtime: instantiate-runtime
                          instantiate-compiletime: instantiate-compiletime
                          info: module-info
                          stamp: stamp
                          reference: ref))))

(define (module-reference-ref ref #!key compare-stamps)
  (if (not (module-reference-absolute? ref))
      (error "Module reference must be absolute"))
  (or (let ((loaded-module (table-ref *loaded-module-registry* ref #f)))
        (if compare-stamps
            (and loaded-module
                 (loader-compare-stamp (loaded-module-loader loaded-module)
                                       (module-reference-path ref)
                                       (loaded-module-stamp loaded-module))
                 loaded-module)
            loaded-module))
      (let ((loaded-module
             (module-reference-load ref)))
        (table-set! *loaded-module-registry* ref loaded-module)
        loaded-module)))

;;;; ---------- Module utility functions ----------

(define (loaded-module-instantiate! lm phase)
  (if (zero? (expansion-phase-number phase))
      ((loaded-module-instantiate-runtime lm))
      ((loaded-module-instantiate-compiletime lm) lm phase)))

(define (loaded-module-instantiated? lm phase)
  (if (zero? (expansion-phase-number phase))
      (loaded-module-runtime-instantiated lm)
      (not (eq? #f
                (table-ref (expansion-phase-module-instances phase)
                           (loaded-module-reference lm)
                           #f)))))

(define (loaded-modules-instantiate/deps lms phase
                                                #!key force)
  (letrec ((next-phase (expansion-phase-next-phase phase))
           ;; Table of module-reference
           ;; objects to #t
           (load-table (make-table))
           (rec (lambda (lm phase)
                  (cond
                   ((and (not (table-ref load-table
                                         (loaded-module-reference lm)
                                         #f))
                         (or force
                             (not (loaded-module-instantiated? lm
                                                               phase))))
                    (table-set! load-table
                                (loaded-module-reference lm)
                                #t)
                    
                    (for-each (lambda (lm)
                                (rec lm phase))
                              (module-info-runtime-dependencies
                               (loaded-module-info lm)))

                    (loaded-module-instantiate! lm phase))))))
    (for-each (lambda (lm)
                (for-each (lambda (lm)
                            (rec lm next-phase)))
                (module-info-compiletime-dependencies
                 (loaded-module-info lm))
                (rec lm phase))
              lms)))

(define (loaded-module-instantiate/deps lm phase
                                               #!key force)
  (loaded-modules-instantiate/deps! (list lm)
                                          phase
                                          force: force))


(define (module-import modules env phase)
  ;; TODO This procedure doesn't work properly.
  (call-with-values
      (lambda () (resolve-imports modules))
    (lambda (defs module-references)
      ;; TODO We need to load the modules from the references
      ;; first. Compare stamps?
      (let ((loaded-modules
             ...))
        (if (or (repl-environment? env)
                (expansion-phase-compiletime? phase))
            (loaded-modules-instantiate/deps loaded-modules
                                             phase)))
      
      (module-add-defs-to-env defs env
                              phase-number: (expansion-phase-number phase)))))


(define module-module
  (let* ((repl-environment #f)
         (fn (lambda (mod)
               (let ((mod (resolve-one-module mod)))
                 (if (not (environment-module (*top-environment*)))
                     (set! repl-environment (*top-environment*)))
                 
                 (*top-environment* (make-top-environment mod))
                 (module-load/deps (list mod))
                 
                 (let ((info (module-info mod)))
                   (module-add-defs-to-env (module-info-imports info)
                                           (*top-environment*))
                   (module-add-defs-to-env
                    (macroexpansion-symbol-defs (module-info-symbols info)
                                                (module-info-environment info))
                    (*top-environment*)))
                 (void)))))
    (lambda (mod)
      ;; This function might be called with #f as argument
      (if mod
          (fn mod)
          (begin
            (*top-environment* repl-environment)
            (void))))))
