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

;; TODO This doesn't work atm
(define (loaded-module-instantiate/deps modules)
  (let ((load-table (make-table)))
    (for-each
     (lambda (module)
       (let rec ((module module))
         (cond
          ((not (table-ref load-table
                           (module-reference-path module)
                           #f))
           (table-set! load-table
                       (module-reference-path module)
                       #t)
           
           (for-each rec
                     (module-info-uses
                      (module-info module)))
           
           (let ((fn (module-file module)))
             (if fn
                 (load-once fn module)))))))
     modules)))

(define (module-import modules env phase)
  (call-with-values
      (lambda () (resolve-imports modules))
    (lambda (defs mods)
      (if (or (not (environment-module env))
              (> phase 0))
          (loaded-module-instantiate/deps mods))
      
      (module-add-defs-to-env defs env phase: phase))))


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
