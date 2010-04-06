;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                    Loaded module data structure                  ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define-type loaded-module
  id: 08E43248-AC39-4DA1-863E-49AFA9FFE84E
  constructor: make-loaded-module/internal

  (instantiate-runtime read-only:)
  (instantiate-compiletime read-only:)
  (info read-only:)
  (stamp read-only:)
  ;; An absolute module reference object
  (reference read-only:)
  
  (runtime-instantiated init: #f)
  ;; A list of the currently loaded loaded-module objects that
  ;; directly depend on this loaded-module
  (dependent-modules init: '()))

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

(define (loaded-module-stamp-is-fresh? lm)
  (loader-compare-stamp (loaded-module-loader lm)
                        (module-reference-path
                         (loaded-module-reference lm))
                        (loaded-module-stamp lm)))

(define *loaded-module-registry* (make-table))

;; Loads a module, regardless of whether it's already loaded or not.
(define (module-reference-load! ref)
  (let ((loaded-module
         (loader-load-module (module-reference-loader ref)
                             (module-reference-path ref))))
    (table-set! *loaded-module-registry* ref loaded-module)
    loaded-module))

(define (module-reference-ref ref #!key compare-stamps)
  (if (not (module-reference-absolute? ref))
      (error "Module reference must be absolute"))
  (or (let ((loaded-module (table-ref *loaded-module-registry* ref #f)))
        (if compare-stamps
            (and loaded-module
                 (loaded-module-stamp-is-fresh? loaded-module)
                 loaded-module)
            loaded-module))
      (module-reference-load! ref)))

(define (loaded-module-instantiated? lm phase)
  (if (zero? (expansion-phase-number phase))
      (loaded-module-runtime-instantiated lm)
      (not (eq? #f
                (table-ref (expansion-phase-module-instances phase)
                           (loaded-module-reference lm)
                           #f)))))

;; This procedure doesn't make sure that the module's dependencies are
;; instantiated first, nor that the modules that depend on this module
;; are reinstantiated or that the dependent-modules field of the
;; relevant loaded-module objects are kept up-to-date.
;;
;; It is not intended to be used directly, it is just a utility
;; function for loaded-modules-instantiate/deps and
;; loaded-modules-reinstantiate.
(define (loaded-module-instantiate! lm phase)
  (if (zero? (expansion-phase-number phase))
      ((loaded-module-instantiate-runtime lm))
      (let ((instance ((loaded-module-instantiate-compiletime lm)
                       lm phase)))
        (table-set! (expansion-phase-module-instances phase)
                    (loaded-module-reference lm)
                    instance)
        instance)))

(define (loaded-modules-instantiate/deps lms phase)
  (letrec
      ((next-phase (expansion-phase-next-phase phase))
       ;; Table of module-reference objects to #t
       (instantiate-table (make-table))
       (rec (lambda (lm phase)
              (cond
               ((and (not (table-ref instantiate-table
                                     (loaded-module-reference lm)
                                     #f))
                     (not (loaded-module-instantiated? lm phase)))
                ;; Flag that this module is to be instantiated, to
                ;; avoid double-instantiations.
                (table-set! instantiate-table
                            (loaded-module-reference lm)
                            #t)
                
                ;; Make sure to instantiate the module's dependencies first
                (for-each
                 (lambda (dep-ref)
                   (let ((dependency
                          (module-reference-ref dep-ref)))
                     ;; Update the dependent-modules field
                     (loaded-module-dependent-modules-set!
                      dependency
                      (cons lm
                            (loaded-module-dependent-modules
                             dependency)))
                     ;; Recurse
                     (rec dependency phase)))
                 
                 (module-info-runtime-dependencies
                  (loaded-module-info lm)))
                
                ;; Instantiate the module
                (loaded-module-instantiate! lm phase))))))
    (for-each (lambda (lm)
                (for-each (lambda (lm)
                            (rec lm next-phase))
                  (module-info-compiletime-dependencies
                           (loaded-module-info lm)))
                (rec lm phase))
              lms)))

(define (loaded-modules-reinstantiate lms phase)
  (letrec
      (;; Table of module-reference objects to #t
       (instantiate-table (make-table))
       (rec (lambda (lm)
              (cond
               ((and (not (table-ref instantiate-table
                                     (loaded-module-reference lm)
                                     #f))
                     (not (loaded-module-instantiated? lm phase)))
                ;; Flag that this module is to be instantiated, to
                ;; avoid double-instantiations.
                (table-set! instantiate-table
                            (loaded-module-reference lm)
                            #t)
                
                ;; Re-instantiate the module
                (loaded-module-instantiate! lm phase)
                
                ;; Re-instantiate the dependent modules
                (for-each rec
                 (loaded-module-dependent-modules lm)))))))
    (for-each rec lms)))

(define (module-import modules env phase)
  (define (module-loaded-but-not-fresh? ref)
    ;; This function is pure (not a closure). It is here because it's
    ;; only used here.
    (let ((lm (table-ref *loaded-module-registry* ref #f)))
      (and lm
           (not (loaded-module-stamp-is-fresh? lm)))))

  (let loop ()
    (call-with-values
        (lambda () (resolve-imports modules))
      (lambda (defs module-references)
        ;; Modules with a non-fresh stamp (that is, modules that have
        ;; changed since they were last loaded) will be reloaded if
        ;; they are imported from the REPL. And when a module is
        ;; reloaded all modules that depend on it must be
        ;; reinstantiated.
        (let ((loaded-modules '())
              (reloaded-modules '()))
          
          (cond
           ((expansion-phase-compiletime? phase)
            (set! loaded-modules
                  (map module-reference-ref
                    module-references)))
           
           ((repl-environment? env)
            (for-each (lambda (ref)
                        (if (module-loaded-but-not-fresh? ref)
                            (set! reloaded-modules
                                  (cons (module-reference-load! ref)
                                        reloaded-modules))
                            (set! loaded-modules
                                  (cons (module-reference-ref ref)
                                        loaded-modules))))
              module-references)))
          
          (loaded-modules-instantiate/deps loaded-modules phase)
          (if (null? reloaded-modules)
              (module-add-defs-to-env defs env
                                      phase-number: (expansion-phase-number phase))
              (begin
                (loaded-modules-reinstantiate reloaded-modules phase)
                ;; We need to re-resolve the imports, because the
                ;; reloads might have caused definitions to change.
                (loop))))))))


(define (macroexpansion-symbol-defs symbols env)
  (let* ((ref (environment-module-reference env))
         (ns (module-reference-namespace
              (environment-module-reference env))))
    (map (lambda (pair)
           (let ((name (car pair))
                 (type (cdr pair)))
             (if (eq? 'def type)
                 (list name
                       'def
                       (gen-symbol ns name)
                       ref)
                 (let ((mac (environment-get env name)))
                   (if (or (not mac)
                           (not (eq? 'mac (car mac))))
                       (error "Internal error in macroexpansion-symbol-defs:"
                              mac))
                   `(,name 'mac ,@(cdr mac))))))
      symbols)))

(define module-module
  (let* ((repl-environment #f)
         (fn (lambda (mod)
               (let* ((module-reference (resolve-one-module mod))
                      (loaded-module (module-reference-ref
                                      module-reference))
                      (info (loaded-module-info loaded-module)))
                 (if (repl-environment? (*top-environment*))
                     (set! repl-environment (*top-environment*)))
                 
                 (*top-environment* (make-top-environment module-reference))
                 (loaded-modules-instantiate/deps (list loaded-module))
                 
                 (module-add-defs-to-env (module-info-imports info)
                                         (*top-environment*))))))
    (lambda (mod)
      (if mod
          (fn mod)
          (*top-environment* repl-environment))
      (void))))
