;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Loader data structure                      ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; Was previously (TODO Remove)
;; (define-type loader
;;   id: 786F06E1-BAF1-45A5-B31F-ED09AE93514F
;;  
;;   ;; Returns a module-info object for the given module
;;   (calculate-info unprintable: equality-skip: read-only:)
;;   (path-absolutize unprintable: equality-skip: read-only:)
;;   ;; Returns what should be in the module-file field of the module
;;   ;; object.
;;   (absolute-file unprintable: equality-skip: read-only:)
;;   (module-name unprintable: equality-skip: read-only:))


(define-type loader
  id: 786F06E1-BAF1-45A5-B31F-ED09AE93514F

  ;; Takes a possible relative path and an origin path to which the
  ;; path should be interpreted relative to, and returns an absolute
  ;; path object.
  (path-absolutize unprintable: equality-skip: read-only:)
  (load-module unprintable: equality-skip: read-only:)
  ;; Takes a module and returns a string for the name of the module
  (module-name unprintable: equality-skip: read-only:))




;;;; ---------- Loader implementations ----------

(define local-loader
  (make-loader   
   ;; path-absolutize
   (lambda (path ref)
     (path-normalize (string-append (symbol->string path) ".scm")
                     #f ;; Don't allow relative paths
                     (path-normalize
                      (path-directory ref))))
   
   ;; load-module
   (lambda (path)
     (load-module path local-loader))

   ;; module-name
   (lambda (path)
     (path-strip-directory
      (path-strip-extension path)))))

(define module-module-loader
  (make-loader
   ;; path-absolutize
   (lambda (path ref) #f)

   ;; load-module
   (lambda (path)
     (make-loaded-module
      ;; instantiate-runtime
      (lambda ()
        #!void)

      ;; instantiate-compiletime
      (lambda (phase loaded-module syntactic-tower)
        ...)

      ;; info
      (make-module-info
       ;; TODO Make sure these parameters are right
       '()
       (cons
        (list 'syntax-rules
              'mac
              (lambda (code env mac-env)
                `(apply module#syntax-rules-proc
                        ',(expr*:cdr code)))
              builtin-environment)
        (map (lambda (x)
               (list x 'def (gen-symbol "module#" x)))
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
               module-module-loader)))
       '() '() '() "" "" "" #f builtin-environment)

      ;; loader
      module-module-loader

      ;; path
      #f))

   ;; module-name
   (lambda (path) "module")))

