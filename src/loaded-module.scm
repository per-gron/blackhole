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
  ;; A weak hashtable of the currently loaded loaded-module objects
  ;; that directly depend on this loaded-module
  dependent-modules)

(define (make-loaded-module #!key
                            invoke-runtime
                            invoke-compiletime
                            visit
                            info
                            stamp
                            reference)
  (if (not (and (procedure? invoke-runtime)
                (or (procedure? invoke-compiletime)
                    (not invoke-compiletime))
                (procedure? visit)
                (module-info? info)
                (module-reference? reference)))
      (error "Invalid parameters"))
  (make-loaded-module/internal invoke-runtime
                               invoke-compiletime
                               visit
                               info
                               stamp
                               reference
                               (make-table weak-keys: #t)))

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
  (let ((previously-loaded-module
         (table-ref *loaded-module-registry* ref #f))
        (loaded-module
         (loader-load-module (module-reference-loader ref)
                             (module-reference-path ref))))

    ;; Update the relevant dependent-modules fields
    (if previously-loaded-module
        (loaded-module-dependent-modules-set!
         loaded-module
         (loaded-module-dependent-modules previously-loaded-module)))
    (let ((action
           (lambda (dependency)
             (table-set!
              (loaded-module-dependent-modules
               (module-reference-ref dependency))
              loaded-module
              #t)))
          (info (loaded-module-info loaded-module)))
      (for-each action (module-info-runtime-dependencies info))
      (for-each action (module-info-compiletime-dependencies info)))
    
    (table-set! *loaded-module-registry* ref loaded-module)
    loaded-module))

;; This function will break if invoked when macroexpanding a module
(define (loaded-module-unload! lm) ;; This function isn't used or tested
  ;; First, de-instantiate the modules that depend on this module.
  (let rec ((lm lm))
    (table-for-each (lambda (k v)
                      (rec k))
                    (loaded-module-dependent-modules lm))

    (let ((ref (loaded-module-reference lm)))
      (vector-for-each
       (lambda (phase)
         (table-set! (expansion-phase-module-instances phase)
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
  
  (macros init: #f)
  (macro-env init: #f))

(define (module-instance-ref phase ref)
  (let ((table (expansion-phase-module-instances phase)))
    (and table
         (table-ref table ref #f))))

(define (module-instance-get! phase ref)
  (or (module-instance-ref phase ref)
      (let ((instance (make-module-instance)))
        (table-set! (expansion-phase-module-instances phase)
                    ref
                    instance)
        instance)))

(define (loaded-module-invoked? lm phase)
  (if (zero? (expansion-phase-number phase))
      (loaded-module-runtime-invoked lm)
      (let ((instance (module-instance-ref
                       phase
                       (loaded-module-reference lm))))
        (and instance
             (module-instance-getter instance)
             (module-instance-setter instance)
             #t))))

(define (loaded-module-visited? lm phase)
  (let ((instance (module-instance-ref
                   phase
                   (loaded-module-reference lm))))
    (and instance
         (module-instance-macros instance)
         (module-instance-macro-env instance)
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
      (begin
        ((loaded-module-invoke-runtime lm))
        (loaded-module-runtime-invoked-set! lm #t))
      (let ((invoke-compiletime
             (loaded-module-invoke-compiletime lm)))
        (if invoke-compiletime
            (let ((getter
                   setter
                   ((loaded-module-invoke-compiletime lm)
                    lm phase))
                  (instance (module-instance-get!
                             phase
                             (loaded-module-reference lm))))
              (module-instance-getter-set! instance getter)
              (module-instance-setter-set! instance setter)))))
  ;; It doesn't make sense to return anything, because
  ;; loaded-module-invoke-runtime doesn't return anything useful.
  (void))

;; Analogous to loaded-module-invoke!
(define (loaded-module-visit! lm phase)
  (let* ((next-phase (expansion-phase-next-phase phase))
         (phase-number (expansion-phase-number phase))
         (macro-procedures (list->table
                            ((loaded-module-visit lm) lm next-phase)))
         (macros (make-table))
         
         (module-ref (loaded-module-reference lm))
         (module-info (loaded-module-info lm))
         (namespace-string (module-info-namespace-string module-info))
         
         (env (make-top-environment module-ref))
         (memo-table (make-table
                      test: eq?
                      hash: eq?-hash)))
    (letrec
        ((make-env-from-letsyntax-environment
          (lambda (parent-env ls-env)
            (cond
             ((null? ls-env)
              parent-env)
             
             ((table-ref memo-table
                         ls-env
                         #f) =>
                         (lambda (result)
                           result))
             
             (else
              (let* ((env (make-environment parent-env)))
                (table-set! memo-table
                            ls-env
                            env)

                (for-each
                    (lambda (ls-def)
                      (let ((mac-name (car ls-def))
                            (unique-name (cadr ls-def))
                            (macro-ls-env (caddr ls-def)))
                        (ns-add! env
                                 0
                                 mac-name
                                 (list 'mac
                                       unique-name ;; TODO Is this right?
                                       (make-env-from-letsyntax-environment
                                        env
                                        macro-ls-env)
                                       unique-name))))
                  ls-env)
                
                env))))))
      
      (for-each
          (lambda (def)
            (cond
             ((eq? 'def (cadr def))
              ;; Regular define
              (let ((name (car def)))
                (environment-add-def!
                 env
                 ;; The name it's imported as
                 name
                 ;; The name it's imported from
                 (gen-symbol namespace-string name)
                 phase-number: phase-number)))
             ((eq? 'mac (cadr def))
              ;; Macro
              (let ((name (car def)))
                (environment-add-mac!
                 env
                 name
                 (table-ref macro-procedures name)
                 (make-env-from-letsyntax-environment env (caddr def))
                 phase-number: phase-number)))
             (else
              (error "Internal error in Black Hole (loaded-module.scm)"))))
        (module-info-definitions module-info)))

    (module-add-defs-to-env (module-info-imports module-info)
                            env
                            phase: phase)

    (let ((instance (module-instance-get!
                     phase
                     (loaded-module-reference lm))))
      (module-instance-macros-set! instance macro-procedures)
      (module-instance-macro-env-set! instance env))
    
    env))

;; Utility procedure
(define (invoke-dependencies info->dependencies lm invoke)
  (for-each
      (lambda (dep-ref)
        (let ((dependency
               (module-reference-ref dep-ref)))
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
    #;(pp (list 'invoking ;; TODO Remove this
              (loaded-module-reference lm)
              phase))
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
      (invoke-dependencies module-info-compiletime-dependencies
                           lm
                           (lambda (dependency)
                             (loaded-module-invoke/deps dependency
                                                        next-phase
                                                        memo: invoke-memo))))
    
    ;; Visit the module's runtime dependencies
    (invoke-dependencies module-info-runtime-dependencies
                         lm
                         (lambda (dependency)
                           (loaded-module-visit/deps dependency
                                                     phase
                                                     visit-memo: visit-memo
                                                     invoke-memo: invoke-memo)))
    
    ;; Visit the module
    #;(pp (list 'visiting TODO Remove this
              (loaded-module-reference lm)
              phase))
    (loaded-module-visit! lm phase))))

(define (loaded-modules-visit/deps lms phase)
  (let ((invoke-memo (make-table))
        (visit-memo (make-table)))
    (for-each (lambda (lm)
                (loaded-module-visit/deps lm phase
                                          invoke-memo: invoke-memo
                                          visit-memo: visit-memo))
      lms)))

;; Returns #t if any module was reloaded
(define (module-load module-references
                     #!key
                     (phase (*expansion-phase*))
                     ;; One of #t, #f, 'force
                     (reload? 'force))
  (define (module-loaded-but-not-fresh? ref)
    ;; This function is not a closure. It is here because
    ;; it's only used here.
    (let ((lm (table-ref *loaded-module-registry* ref #f)))
      (and lm
           (not (loaded-module-stamp-is-fresh? lm)))))

  (define (loaded-modules-reinvoke lms phase)
    (letrec
        (;; Table of module-reference objects to #t
         (invoke-table (make-table))
         (rec (lambda (lm #!optional force?)
                (cond
                 ((and (not (table-ref invoke-table
                                       (loaded-module-reference lm)
                                       #f))
                       (or force?
                           (not (loaded-module-invoked? lm phase))))
                  ;; Flag that this module is to be invoked, to
                  ;; avoid double-instantiations.
                  (table-set! invoke-table
                              (loaded-module-reference lm)
                              #t)
                  
                  ;; Re-invoke and re-visit the module
                  (loaded-module-invoke! lm phase)
                  (loaded-module-visit! lm phase)
                  
                  ;; Re-invoke the dependent modules
                  (table-for-each
                   (lambda (k v)
                     (rec k #t))
                   (loaded-module-dependent-modules lm)))))))
      (for-each rec lms)))
  
  (let ((loaded-modules '())
        (reloaded-modules '()))
    
    (for-each
        (lambda (ref)
          (if (or (eq? 'force reload?)
                  (and reload?
                       (module-loaded-but-not-fresh? ref)))
              (push! reloaded-modules
                     (module-reference-load! ref))
              (push! loaded-modules
                     (module-reference-ref ref))))
      module-references)

    (loaded-modules-reinvoke reloaded-modules phase)
    (loaded-modules-invoke/deps loaded-modules phase)
    (loaded-modules-visit/deps loaded-modules phase)
    (pair? reloaded-modules)))

(define (module-import modules env phase)
  (let loop ()
    (let ((defs
           module-references
           (resolve-imports modules)))

      (let ((a-module-was-reloaded?
             (module-load module-references
                          phase: phase
                          reload?: (repl-environment? env))))
        
        (if (not a-module-was-reloaded?)
            (module-add-defs-to-env defs env
                                    phase: phase)
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
                 (loaded-modules-invoke/deps
                  (list loaded-module)
                  (syntactic-tower-first-phase
                   *repl-syntactic-tower*))
                 
                 (module-add-defs-to-env (module-info-imports info)
                                         (*top-environment*))))))
    (lambda (mod)
      (if mod
          (fn mod)
          (*top-environment* repl-environment))
      (void))))



;;;; ---------- Some environment creation stuff ----------

(define (module-add-defs-to-env defs
                                env
                                #!key
                                ;; phase can be #f to apply to all phases
                                (phase
                                 (*expansion-phase*)))
  (define (macro+env-from-module-ref phase ref)
    (loaded-module-visit/deps (module-reference-ref ref) phase)
    (let* ((mi (module-instance-ref phase ref))
           (macros (and mi (module-instance-macros mi)))
           (env (and mi (module-instance-macro-env mi))))
      (values macros
              env)))
  
  (let ((phase-number (and phase (expansion-phase-number phase))))
    (for-each
        (lambda (def)
          (if (eq? 'def (cadr def))
              ;; Regular define
              (environment-add-def!
               env
               (car def) ;; The name it's imported as
               (caddr def) ;; The name it's imported from
               module-reference: (cadddr def)
               phase-number: phase-number)
              ;; Macro
              (let ((fn (caddr def)))
                (if (symbol? fn)
                    (let ((macros
                           macros-env
                           (macro+env-from-module-ref phase (cadddr def))))
                      (if (not (and macros macros-env))
                          (error "Internal error in Black Hole (loaded-module.scm)"))
                      (environment-add-mac! env
                                            ;; The name it's imported as
                                            (car def)
                                            ;; The macro procedure
                                            (table-ref macros fn)
                                            ;; The macro's environment
                                            macros-env
                                            phase-number: phase-number))
                    (let* ((env-or-ref
                            (cadddr def))
                           (macro-env
                            (if (env? env-or-ref)
                                env-or-ref
                                (let ((mi (module-instance-ref phase env-or-ref)))
                                  (or (and mi (module-instance-macro-env mi))
                                      (error "Internal error in Black Hole (loaded-module.scm)"))))))
                      (environment-add-mac! env
                                            ;; The name it's imported as
                                            (car def)
                                            ;; The macro procedure
                                            fn
                                            ;; The macro's environment
                                            macro-env
                                            phase-number: phase-number))))))
      defs)))

(define module-env-table
  (let* ((ns (make-table))
         (env (make-environment #f ns: ns)))
    (module-add-defs-to-env module-exports-list
                            env
                            phase: #f)
    ns))


(define builtin-ns
  (let* ((builtin-pair
          (cons (env-ns builtin-environment)
                gambit-builtin-table))
         (inside-letrec-table
          (env-ns inside-letrec-environment))
         (inside-letrec-pair
          (cons inside-letrec-table
                builtin-pair)))
    (lambda ()
      (let ((ns
             (if (inside-letrec)
                 inside-letrec-pair
                 builtin-pair)))
        (values (if (or (not (zero? (expansion-phase-number
                                     (*expansion-phase*))))
                        (not (environment-module-reference
                              (*top-environment*))))
                    (cons module-env-table ns)
                    ns)
                #f)))))

(define (make-top-environment module-reference)
  (let ((ns (cons (make-table)
                  builtin-ns)))
    (make-environment module-reference ns: ns)))

(define empty-environment
  (make-top-environment #f))

(*top-environment* (make-top-environment #f))
