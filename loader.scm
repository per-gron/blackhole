;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Loader data structure                      ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;;;; ---------- Module utility functions ----------

(define (current-module)
  (environment-module-reference (top-environment)))

(define (current-loader)
  (let ((cm (current-module)))
    (if cm
        (module-loader (current-module))
        local-loader)))


(define *calc-info-cache* (make-parameter #f))

(define (with-module-cache thunk)
  (if (*calc-info-cache*)
      (thunk)
      (parameterize
       ((*calc-info-cache* (make-table)))
       (suspend-ns-table-changes thunk))))

(define (make-module-util-function fn)
  (lambda (mod)
    (with-module-cache
     (lambda ()
       (fn (resolve-one-module mod))))))

(define module-info
  (make-module-util-function
   (lambda (mod)
     (let ((mp (module-path mod)))
       (or (table-ref (*calc-info-cache*) mp #f)
           (let ((ret ((loader-calculate-info
                        (module-loader mod))
                       mod)))
             (table-set! (*calc-info-cache*) mp ret)
             ret))))))

(define module-needs-compile?
  (make-module-util-function
   (lambda (mod)
     (let* ((path (module-file mod))
            (of (last-object-file path)))
       (if of
           (not (file-newer? of path))
           'not-compiled)))))

(define (module-compile! mod
                         #!key
                         continue-on-error
                         to-c)
  (let ((mod (resolve-one-module mod)))
    (with-module-cache
     (lambda ()
       (with-exception-catcher
        (lambda (e)
          (if continue-on-error
              (begin
                (display "Warning: Compilation failed: ")
                (display-exception e)
                #f)
              (raise e)))
        (lambda ()
          (let ((info (module-info mod)))
            (with-module-macroexpansion ;; For *module-macroexpansion-uses*
             (lambda ()
               (let ((result (compile-with-options
                              mod
                              (module-file mod)
                              to-c: to-c
                              options: (module-info-options info)
                              cc-options: (module-info-cc-options info)
                              ld-options-prelude: (module-info-ld-options-prelude
                                                   info)
                              ld-options: (module-info-ld-options info))))
                 (if (not result)
                     (error "Compilation failed"))))))))))))

(define module-clean!
  (make-module-util-function
   (lambda (mod)
     (let ((fn (module-file mod)))
       (and fn (clean-file (module-file mod)))))))

(define module-namespace
  (let ((fn
         (make-module-util-function
          (lambda (mod)
            (string-append
             (let ((loader (module-reference-loader mod)))
               (if (eq? loader module-module-loader)
                   "module"
                   (namespace-choose-unique
                    ((loader-module-name loader)
                     (module-reference-path mod))
                    (module-reference-path mod))))
             "#")))))
    (lambda (mod)
      ;; This function might be called with #f as argument
      (if mod (fn mod) "~#"))))

(define (module-load/deps modules)
  (with-module-cache
   (lambda ()
     (let ((load-table (make-table)))
       (for-each
        (lambda (module)
          (let rec ((module module))
            (cond
             ((not (table-ref load-table
                              (module-path module)
                              #f))
              (table-set! load-table
                          (module-path module)
                          #t)
              
              (for-each rec
                        (module-info-uses
                         (module-info module)))

              (let ((fn (module-file module)))
                (if fn
                    (load-once fn module)))))))
        modules)))))


(define (module-import modules #!optional (env (top-environment)))
  (with-module-cache
   (lambda ()
     (call-with-values
         (lambda () (resolve-imports modules))
       (lambda (defs mods)
         (if (not (environment-top? env))
             (error "Incorrectly placed import"))
         
         (if (or (not (environment-module env))
                 (> (expansion-phase) 0))
             (module-load/deps mods))
         
         (module-add-defs-to-env defs env))))))

(define (module-import-for-syntax modules #!optional (env (top-environment)))
  (error "TODO To be implemented"))

(define module-module
  (let* ((repl-environment #f)
         (fn (make-module-util-function
              (lambda (mod)
                (if (not (environment-module (top-environment)))
                    (set! repl-environment (top-environment)))

                (top-environment (make-top-environment mod))
                (module-load/deps (list mod))
                
                (let ((info (module-info mod)))
                  (module-add-defs-to-env (module-info-imports info)
                                          (top-environment))
                  (module-add-defs-to-env
                   (macroexpansion-symbol-defs (module-info-symbols info)
                                               (module-info-environment info))
                   (top-environment)))
                (void)))))
    (lambda (mod)
      ;; This function might be called with #f as argument
      (if mod
          (fn mod)
          (begin
            (top-environment repl-environment)
            (void))))))


;;;; ---------- The loader data structure ----------

(define-type loader
  id: 786F06E1-BAF1-45A5-B31F-ED09AE93514F
  constructor: make-loader/internal

  ;; A unique symbol identifying this loader
  (name read-only:)
  (path-absolute? unprintable: equality-skip: read-only:)
  ;; Takes a possible relative path and an origin path to which the
  ;; path should be interpreted relative to, and returns an absolute
  ;; path object.
  (path-absolutize unprintable: equality-skip: read-only:)
  (load-module unprintable: equality-skip: read-only:)
  ;; Takes a module and returns a string for the name of the module
  (module-name unprintable: equality-skip: read-only:))

(define loader-registry (make-table))

(define (make-loader #!key
                     name
                     path-absolute?
                     path-absolutize
                     load-module
                     module-name)
  (if (not (and (symbol? name)
                (procedure? path-absolute?)
                (procedure? path-absolutize)
                (procedure? load-module)
                (procedure? module-name)))
      (error "Invalid parameters"))
  (let ((result
         (make-loader/internal name
                               path-absolute?
                               path-absolutize
                               load-module
                               module-name)))
    (table-set! loader-registry name result)
    result))

(define (loader->skeleton loader)
  (make-loader/internal (loader-name loader)
                        #f
                        #f
                        #f
                        #f))

(define (skeleton->loader loader)
  (table-ref loader-registry (loader-name loader)))

(define module-exports-list
  (cons
   (list 'syntax-rules
         'mac
         (lambda (code env mac-env)
           `(apply module#syntax-rules-proc
                   ',(expr*:cdr code)))
         builtin-environment)
   (map (lambda (x)
          (list x 'def (gen-symbol "module#" x)))
        ;; TODO Make sure this list is updated
        '(expand-macro
          make-syntactic-closure
          capture-syntactic-environment
          extract-syntactic-closure-list
          identifier?
          identifier=?
          sc-macro-transformer
          rsc-macro-transformer
          er-macro-transformer
          nh-macro-transformer
          
          make-loader
          loader-load
          loader-calculate-info
          loader-needs-compile?
          loader-clean!
          loader-compile!
          
          make-module-info
          module-info-symbols
          module-info-exports
          module-info-uses
          module-info-options
          module-info-cc-options
          module-info-ld-options-prelude
          module-info-ld-options
          module-info-force-compile?
          module-info-environment
          module-info-calculate
          
          resolve-module
          resolve-modules
          resolve-one-module
          make-singleton-module-resolver
          package-module-resolver
          module-resolver-add!
          make-external-module-loader
          make-external-module-resolver
          
          current-module
          current-loader
          
          with-module-cache
          
          make-module
          module-loader
          module-path
          module-info
          module-needs-compile?
          module-compile!
          module-clean!
          module-namespace
          module-load/deps
          module-import
          module-module
          
          modules-compile!
          modules-clean!
          modules-in-dir
          module-deps
          module-compile/deps!
          module-clean/deps!
          module-generate-export-list
          module-compile-bunch
          module-compile-to-standalone
          module-files-in-dir
          
          loader
          module-module-loader))))


;;;; ---------- Some environment creation stuff ----------

;; This code is here because it depends on the module-info machinery.

(define (module-add-defs-to-env defs env
                                #!key (phase (expansion-phase)))
  (with-module-cache
   (lambda ()
     (for-each
      (lambda (def)
        (if (eq? 'def (cadr def))
            ;; Regular define
            (environment-add-def!
             env
             (car def) ;; The name it's imported as
             (caddr def) ;; The name it's imported from
             phase: phase)
            ;; Macro
            (environment-add-mac!
             env
             ;; The name it's imported as
             (car def)
             ;; The macro procedure
             (caddr def)
             ;; The macro's environment
             (cadddr def)
             phase: phase)))
      defs))))

(define module-env-table
  (let* ((ns (make-table))
         (env (make-environment #f ns)))
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
        (values (if (or (not (zero? (expansion-phase)))
                        (not (environment-module-reference
                              (top-environment))))
                    (cons module-env-table ns)
                    ns)
                #f)))))

(define (make-top-environment module-reference)
  (let ((ns (cons (make-table)
                  builtin-ns)))
    (make-environment module-reference ns)))

(define empty-environment
  (make-top-environment #f))

(define top-environment
  (make-parameter (make-top-environment #f)))





;;;; ---------- Loader implementations ----------

(define local-loader
  (make-loader
   name:
   'local

   path-absolute?:
   path-absolute?
   
   path-absolutize:
   (lambda (path #!optional ref)
     (path-normalize (string-append (symbol->string path) ".scm")
                     #f ;; Don't allow relative paths
                     (if ref
                         (path-normalize
                          ;; This ensures that (path-directory ref)
                          ;; actually exists. Otherwise path-normalize
                          ;; might segfault.
                          (path-directory ref))
                         (current-directory))))
   
   load-module:
   (lambda (path)
     (load-module path local-loader))

   module-name:
   (lambda (path)
     (path-strip-directory
      (path-strip-extension path)))))

(define module-module-loader
  (make-loader
   name:
   'module

   path-absolute?:
   (lambda (path) #t)
   
   path-absolutize:
   (lambda (path #!optional ref) #f)

   load-module:
   (lambda (path)
     (make-loaded-module
      instantiate-runtime:
      (lambda ()
        #!void)
      
      instantiate-compiletime:
      (lambda (phase loaded-module syntactic-tower)
        (values (lambda (name value)
                  (error "You can't side-effect the internals of Black Hole"))
                (lambda (name)
                  (eval (gen-symbol "module#" name)))))

      info:
      (make-module-info
       namespace-string:
       "module#"

       exports:
       module-exports-list

       environment:
       (make-top-environment #f))

      reference:
      (make-module-reference module-module-loader #f)))

   module-name:
   (lambda (path) "module")))
