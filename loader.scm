;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Loader data structure                      ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;;;; ---------- The loader data structure ----------

(define-type loader
  id: 786F06E1-BAF1-45A5-B31F-ED09AE93514F
  constructor: make-loader/internal

  ;; A unique symbol identifying this loader
  (name read-only:)
  (path-absolute?-fn unprintable: equality-skip: read-only:)
  ;; Takes a possibly relative path and an origin path to which the
  ;; path should be interpreted relative to, and returns an absolute
  ;; path object.
  (path-absolutize-fn unprintable: equality-skip: read-only:)
  ;; Takes an absolute path and returns the instantiate-runtime
  ;; procedure, the instantiate-compiletime procedure, the module-info
  ;; structure and a stamp object.
  (load-module-fn unprintable: equality-skip: read-only:)
  (compare-stamp-fn unprintable: equality-skip: read-only:)
  ;; Takes a path and returns a string for the name of the module
  (module-name-fn unprintable: equality-skip: read-only:))

(define (loader-path-absolute? loader path)
  ((loader-path-absolute?-fn loader) path))

(define (loader-path-absolutize loader path ref)
  ((loader-path-absolutize-fn loader) path ref))

(define (loader-load-module loader path)
  ((loader-load-module-fn loader) path))

(define (loader-compare-stamp loader path stamp)
  ((loader-compare-stamp-fn loader) path stamp))

(define (loader-module-name loader path)
  ((loader-module-name-fn loader) path))

(define loader-registry (make-table))

(define (make-loader #!key
                     name
                     path-absolute?
                     path-absolutize
                     load-module
                     compare-stamp
                     module-name)
  (if (not (and (symbol? name)
                (procedure? path-absolute?)
                (procedure? path-absolutize)
                (procedure? load-module)
                (procedure? compare-stamp)
                (procedure? module-name)))
      (error "Invalid parameters"))
  (let ((result
         (make-loader/internal name
                               path-absolute?
                               path-absolutize
                               load-module
                               compare-stamp
                               module-name)))
    (table-set! loader-registry name result)
    result))

(define (loader->skeleton loader)
  (make-loader/internal (loader-name loader)
                        #f
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
          (list x
                'def
                (gen-symbol "module#" x)
                ;; #f because this doesn't belong to a real module
                ;; (this is usually a module reference)
                #f))
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
          module-info-dependencies
          module-info-options
          module-info-cc-options
          module-info-ld-options-prelude
          module-info-ld-options
          module-info-force-compile
          module-info-calculate
          
          resolve-module
          resolve-modules
          resolve-one-module
          make-singleton-module-resolver
          package-module-resolver
          module-resolver-add!
          make-external-module-loader
          make-external-module-resolver
          
          current-module-reference
          current-loader
          
          make-module-reference
          module-reference-loader
          module-reference-path
          module-reference-namespace
          module-info
          module-needs-compile?
          module-compile!
          module-clean!
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

(define (module-add-defs-to-env defs
                                env
                                #!key
                                (phase-number
                                 (expansion-phase-number
                                  (*expansion-phase*))))
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
         (environment-add-mac!
          env
          ;; The name it's imported as
          (car def)
          ;; The macro procedure
          (let ((fn (caddr def)))
            (if (symbol? fn)
                (eval fn) ;; TODO Make sure this is done in the
                          ;; correct phase (I think this always takes
                          ;; the runtime phase)
                fn))
          ;; The macro's environment
          (cadddr def)
          phase-number: phase-number)))
   defs))

(define module-env-table
  (let* ((ns (make-table))
         (env (make-environment #f ns)))
    (module-add-defs-to-env module-exports-list
                            env
                            phase-number: #f)
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
    (make-environment module-reference ns)))

(define empty-environment
  (make-top-environment #f))

(*top-environment* (make-top-environment #f))





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
     (let ((ref (make-module-reference local-loader path)))
       (call-with-values
           (lambda ()
             (load-module-from-file ref path))
         (lambda (instantiate-runtime
                  instantiate-compiletime
                  info-alist)
           (make-loaded-module
            instantiate-runtime: instantiate-runtime
            instantiate-compiletime: instantiate-compiletime
            info: (make-module-info-from-alist ref info-alist)
            stamp: (file-last-changed-seconds path)
            reference: ref)))))

   compare-stamp:
   (lambda (path stamp)
     (= (file-last-changed-seconds path)
        stamp))

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
      (lambda (loaded-module phase)
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

   compare-stamp:
   (lambda (path stamp) #t)

   module-name:
   (lambda (path) "module")))
