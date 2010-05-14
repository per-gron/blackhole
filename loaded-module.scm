;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                    Loaded module data structure                  ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define-type loaded-module
  id: 08E43248-AC39-4DA1-863E-49AFA9FFE84E
  constructor: make-loaded-module/internal

  (invoke-runtime read-only:)
  (invoke-compiletime read-only:)
  (visit read-only:)
  (info read-only:)
  (stamp read-only:)
  ;; An absolute module reference object
  (reference read-only:)
  
  (runtime-invoked init: #f)
  ;; A list of the currently loaded loaded-module objects that
  ;; directly depend on this loaded-module
  (dependent-modules init: '()))

(define (make-loaded-module #!key
                            invoke-runtime
                            invoke-compiletime
                            visit
                            info
                            stamp
                            reference)
  (if (not (and (procedure? invoke-runtime)
                (procedure? invoke-compiletime)
                (procedure? visit)
                (module-info? info)
                (module-reference? reference)))
      (error "Invalid parameters"))
  (make-loaded-module/internal invoke-runtime
                               invoke-compiletime
                               visit
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

;; This function will break if invoked when macroexpanding a module
(define (loaded-module-unload! lm) ;; This function isn't used or tested
  ;; First, de-instantiate the modules that depend on this module.
  (let rec ((lm lm))
    (for-each rec
      (loaded-module-dependent-modules lm))

    (let ((ref (loaded-module-reference lm)))
      (vector-for-each
       (lambda (phase)
         (table-set! (expansion-phase-instances phase)
                     ref))
       (syntactic-tower-phases *repl-syntactic-tower*))))
  
  (table-set! *loaded-module-registry*
              (loaded-module-reference lm)))

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

;;;; Visiting and invoking functionality

;; Invoking a module means loading everything that needs to be loaded
;; to be able to use its exported variable bindings. Before invoking a
;; module, its runtime dependencies must be invoked.
;;
;; Visiting a module means loading everything that needs to be loaded
;; to be able to perform macro expansions on macros defined in that
;; module.  Before visiting a module, its compile time dependencies
;; must be invoked, and its runtime dependencies must be visited.
;;
;; When importing a module to the REPL the module should be invoked
;; and visited.

;; A module instance object implicitly belongs to an expansion phase
;; and to a module reference, because it's stored in the expansion
;; phase object's instance table, with a module reference as the key.
;;
;; Because of this, there's no need to store pointers back to these
;; objects.
(define-type module-instance
  id: F366DFDB-6F11-47EB-9558-04BF7B31225D

  (getter init: #f)
  (setter init: #f)
  (macros init: #f))

(define (module-instance-ref phase lm)
  (let ((table (expansion-phase-module-instances phase)))
    (and table
         (table-ref table
                    (loaded-module-reference lm)
                    #f))))

(define (module-instance-get! phase lm)
  (or (module-instance-ref phase lm)
      (let ((instance (make-module-instance)))
        (table-set! (expansion-phase-module-instances phase)
                    (loaded-module-reference lm)
                    instance)
        instance)))

(define (loaded-module-invoked? lm phase)
  (if (zero? (expansion-phase-number phase))
      (loaded-module-runtime-invoked lm)
      (let ((instance (module-instance-ref phase lm)))
        (and instance
             (module-instance-getter instance)
             (module-instance-setter instance)
             #t))))

(define (loaded-module-visited? lm phase)
  (let ((instance (module-instance-ref phase lm)))
    (and instance
         (module-instance-macros instance)
         #t)))

;; This procedure doesn't make sure that the module's dependencies are
;; invoked first, nor that the modules that depend on this module
;; are reinvoked or that the dependent-modules field of the
;; relevant loaded-module objects are kept up-to-date.
;;
;; It is not intended to be used directly, it is just a utility
;; function for loaded-modules-invoke/deps and
;; loaded-modules-reinvoke.
(define (loaded-module-invoke! lm phase)
  (if (zero? (expansion-phase-number phase))
      ((loaded-module-invoke-runtime lm))
      (let ((getter
             setter
             ((loaded-module-invoke-compiletime lm)
              lm phase))
            (instance (module-instance-get! phase lm)))
        (module-instance-getter-set! instance getter)
        (module-instance-getter-set! instance setter)))
  ;; It doesn't make sense to return anything, because
  ;; loaded-module-invoke-runtime doesn't return anything useful.
  (void))

;; Analogous to loaded-module-invoke!
(define (loaded-module-visit! lm phase)
  (let ((macros (list->table
                 ((loaded-module-visit lm) lm phase)))
        (instance (module-instance-get! phase lm)))
    (module-instance-macros-set! instance macros)
    macros))

(define (loaded-module-macros lm phase)
  (let ((instance (module-instance-get! phase lm)))
    (or (module-instance-macros instance)
        (loaded-module-visit! lm phase))))

;; Utility procedure
(define (invoke-dependencies info->dependencies lm invoke)
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
          (invoke dependency)))
    (info->dependencies
     (loaded-module-info lm))))

(define (loaded-module-invoke/deps lm phase
                                   #!key
                                   ;; Table of module-reference objects to #t
                                   (memo (make-table)))
  (cond
   ((and (not (table-ref memo
                         (loaded-module-reference lm)
                         #f))
         (not (loaded-module-invoked? lm phase)))
    ;; Flag that this module is to be invoked, to avoid
    ;; double-invocations and to improve the performance of this
    ;; function.
    (table-set! memo
                (loaded-module-reference lm)
                #t)
    
    ;; Invoke the module's runtime dependencies
    (invoke-dependencies module-info-runtime-dependencies
                         lm
                         (lambda (dependency)
                           (loaded-module-invoke/deps dependency phase
                                                      memo: memo)))
    
    ;; Invoke the module
    (loaded-module-invoke! lm phase))))

(define (loaded-modules-invoke/deps lms phase)
  (let ((memo (make-table)))
    (for-each (lambda (lm)
                (loaded-module-invoke/deps lm phase
                                           memo: memo))
      lms)))

(define (loaded-module-visit/deps lm phase
                                  #!key
                                  ;; Table of module-reference objects to #t
                                  (invoke-memo (make-table))
                                  (visit-memo (make-table)))
  (cond
   ((and (not (table-ref visit-memo
                         (loaded-module-reference lm)
                         #f))
         (not (loaded-module-visited? lm phase)))
    ;; Flag that this module is to be invoked, to avoid
    ;; double-visitations and to improve the performance of this
    ;; function.
    (table-set! visit-memo
                (loaded-module-reference lm)
                #t)
    
    ;; Invoke the module's compile time dependencies
    (let ((next-phase (expansion-phase-next-phase phase)))
      (invoke-dependencies module-info-runtime-dependencies
                           lm
                           (lambda (dependency)
                             (loaded-module-invoke/deps dependency phase
                                                        memo: invoke-memo))))
    
    ;; Visit the module's runtime dependencies
    (let ((next-phase (expansion-phase-next-phase phase)))
      (invoke-dependencies module-info-compiletime-dependencies
                           lm
                           (lambda (dependency)
                             (loaded-module-visit/deps dependency phase
                                                       visit-memo: visit-memo
                                                       invoke-memo: invoke-memo))))
    
    ;; Visit the module
    (loaded-module-visit! lm phase))))

(define (loaded-modules-visit/deps lms phase)
  (let ((invoke-memo (make-table))
        (visit-memo (make-table)))
    (for-each (lambda (lm)
                (loaded-module-visit/deps lm phase
                                          invoke-memo: invoke-memo
                                          visit-memo: visit-memo))
      lms)))

(define (loaded-modules-reinvoke lms phase)
  (error "This function is incorrect")
  (letrec
      (;; Table of module-reference objects to #t
       (invoke-table (make-table))
       (rec (lambda (lm)
              (cond
               ((and (not (table-ref invoke-table
                                     (loaded-module-reference lm)
                                     #f))
                     (not (loaded-module-invoked? lm phase)))
                ;; Flag that this module is to be invoked, to
                ;; avoid double-instantiations.
                (table-set! invoke-table
                            (loaded-module-reference lm)
                            #t)
                
                ;; Re-invoke and re-visit the module
                (loaded-module-invoke! lm phase)
                
                ;; Re-invoke the dependent modules
                (for-each rec
                 (loaded-module-dependent-modules lm)))))))
    (for-each rec lms)))

(define (module-import modules env phase)
  (define (module-loaded-but-not-fresh? ref)
    ;; This function is not a closure. It is here because
    ;; it's only used here.
    (let ((lm (table-ref *loaded-module-registry* ref #f)))
      (and lm
           (not (loaded-module-stamp-is-fresh? lm)))))

  (let loop ()
    (let ((defs
           module-references
           (resolve-imports modules))
          
          ;; Modules with a non-fresh stamp (that is, modules that have
          ;; changed since they were last loaded) will be reloaded if
          ;; they are imported from the REPL. And when a module is
          ;; reloaded all modules that depend on it must be
          ;; reinvoked.
          (loaded-modules '())
          (reloaded-modules '()))
      
      (cond
       ((repl-environment? env)
        (for-each (lambda (ref)
                    (if (module-loaded-but-not-fresh? ref)
                        (set! reloaded-modules
                              (cons (module-reference-load! ref)
                                    reloaded-modules))
                        (set! loaded-modules
                              (cons (module-reference-ref ref)
                                    loaded-modules))))
          module-references))
       
       (else
        (set! loaded-modules
              (map module-reference-ref
                module-references))))
      
      (loaded-modules-invoke/deps loaded-modules phase)
      (loaded-modules-visit/deps loaded-modules phase)
      (if (null? reloaded-modules)
          (module-add-defs-to-env defs env
                                  phase-number: (expansion-phase-number phase))
          (begin
            (loaded-modules-reinvoke reloaded-modules phase)
            ;; We need to re-resolve the imports, because the
            ;; reloads might have caused definitions to change.
            (loop))))))

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
                 (loaded-modules-invoke/deps (list loaded-module))
                 
                 (module-add-defs-to-env (module-info-imports info)
                                         (*top-environment*))))))
    (lambda (mod)
      (if mod
          (fn mod)
          (*top-environment* repl-environment))
      (void))))
