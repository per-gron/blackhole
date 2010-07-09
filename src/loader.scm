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
  ;; Takes an absolute path and returns the invoke-runtime
  ;; procedure, the invoke-compiletime procedure, the module-info
  ;; structure and a stamp object.
  (load-module-fn unprintable: equality-skip: read-only:)
  (compare-stamp-fn unprintable: equality-skip: read-only:)
  ;; Takes a path and returns a string for the name of the module
  (module-name-fn unprintable: equality-skip: read-only:))

(define (loader<? a b)
  (string<? (symbol->string (loader-name a))
            (symbol->string (loader-name b))))

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
           `(apply bh#syntax-rules-proc
                   ',(expr*:cdr code)))
         builtin-environment)
   (map (lambda (x)
          (list x
                'def
                (gen-symbol "bh#" x)
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
          module-info-no-global-state
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
          black-hole-module-loader))))




;;;; ---------- Loader implementations ----------

(define (local-loader-get-stamp path)
  (let ((lof (last-object-file path))
        (p-lc (file-last-changed-seconds path)))
    (if lof
        (max (file-last-changed-seconds (last-object-file path))
             p-lc)
        p-lc)))

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
         (lambda (invoke-runtime
                  invoke-compiletime
                  visit
                  info-alist)
           (make-loaded-module
            invoke-runtime: invoke-runtime
            invoke-compiletime: invoke-compiletime
            visit: visit
            info: (make-module-info-from-alist ref info-alist)
            stamp: (local-loader-get-stamp path)
            reference: ref)))))

   compare-stamp:
   (lambda (path stamp)
     (= (local-loader-get-stamp path)
        stamp))

   module-name:
   (lambda (path)
     (cond ((symbol? path)
            (symbol->string path))
           ((string? path)
            (path-strip-directory
             (path-strip-extension path)))
           (else
            (error "Invalid path" path))))))

(define black-hole-module-loader
  (make-loader
   name:
   'module

   path-absolute?:
   (lambda (path) #t)
   
   path-absolutize:
   (lambda (path #!optional ref)
     (if path
         (error "Module does not exist in this loader:" path)))

   load-module:
   (lambda (path)
     (make-loaded-module
      invoke-runtime:
      (lambda ()
        #!void)
      
      invoke-compiletime:
      (lambda (loaded-module phase)
        (values (lambda (name value)
                  (error "You can't side-effect the internals of Black Hole"))
                (lambda (name)
                  (eval (gen-symbol "bh#" name)))))
      
      visit:
      (lambda (loaded-module phase)
        `((syntax-rules .
            ,(lambda (code env mac-env)
               `(apply bh#syntax-rules-proc
                       ',(expr*:cdr code))))))
      
      info:
      (make-module-info
       namespace-string:
       "bh#"

       exports:
       module-exports-list)

      reference:
      (make-module-reference black-hole-module-loader #f)))

   compare-stamp:
   (lambda (path stamp) #t)

   module-name:
   (lambda (path) "bh")))
