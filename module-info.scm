;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                     Module info data structure                   ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; Previous definition: (TODO remove this)
;;  
;; (define-type module-info
;;   id: 726DB40B-AB18-4396-A570-BB715B602DB9
;;  
;;   (symbols read-only:)
;;   (exports read-only:)
;;   (imports read-only:)
;;   (uses read-only:)
;;  
;;   (options read-only:)
;;   (cc-options read-only:)
;;   (ld-options-prelude read-only:)
;;   (ld-options read-only:)
;;   (force-compile? read-only:)
;;  
;;   (environment read-only:))

(define-type module-info
  id: 726DB40B-AB18-4396-A570-BB715B602DB9
  constructor: make-module-info/internal

  (symbols read-only:)
  (exports read-only:)
  (imports read-only:)
  (imports-for-syntax read-only:)
  (definitions read-only:)
  
  ;; A list of module references, possibly relative
  (runtime-dependencies read-only:)
  ;; A list of module references, possibly relative
  (compiletime-dependencies read-only:)
  
  ;; A list with Gambit compiler options
  (options read-only:)
  ;; A string with options for the C compiler
  (cc-options read-only:)
  ;; A string with options for the linker
  (ld-options-prelude read-only:)
  ;; A string with options for the linker
  (ld-options read-only:)
  ;; A boolean
  (force-compile read-only:)

  (namespace-string read-only:))

(define (make-module-info #!key
                          (symbols '())
                          (exports '())
                          (imports '())
                          (imports-for-syntax '())
                          (definitions '())
                          (runtime-dependencies '())
                          (compiletime-dependencies '())
                          (options '())
                          (cc-options "")
                          (ld-options-prelude "")
                          (ld-options "")
                          (force-compile #f)
                          namespace-string)
  (make-module-info/internal symbols
                             exports
                             imports
                             imports-for-syntax
                             definitions
                             runtime-dependencies
                             compiletime-dependencies
                             options
                             cc-options
                             ld-options-prelude
                             ld-options
                             force-compile
                             namespace-string))

(define (module-info-dependencies info)
  (append (module-info-runtime-dependencies info)
          (module-info-compiletime-dependencies info)))

(define (resolve-export-self-reference ref exports)
  (map (lambda (export)
         (if (eq? 'self-reference
                  (cadddr export))
             (list (car export)
                   (cadr export)
                   (caddr export)
                   ref)
             export))
    exports))

(define (make-module-info-from-alist module-ref module-info-alist)
  (if (not (module-reference-absolute? module-ref))
      (error "Module reference must be absolute"))
  
  (let* ((tbl (list->table module-info-alist))
         
         (definitions (table-ref tbl 'definitions '()))
         (imports (table-ref tbl 'imports '()))
         (imports-for-syntax (table-ref tbl 'imports-for-syntax '()))
         (exports (resolve-export-self-reference
                   module-ref
                   (table-ref tbl 'exports #f)))
         (namespace-string (table-ref tbl 'namespace-string))
         (symbols (map (lambda (def)
                         (cons (car def)
                               (cadr def)))
                    definitions))

         (runtime-dependencies
          (map (lambda (ref)
                 (module-reference-absolutize ref module-ref))
            (table-ref tbl 'runtime-dependencies '())))
         
         (compiletime-dependencies
          (map (lambda (ref)
                 (module-reference-absolutize ref module-ref))
            (table-ref tbl 'compiletime-dependencies '()))))

    (if (not (equal? (module-reference-namespace module-ref)
                     namespace-string))
        (error "The compiled module's namespace and its current \
                namespace don't match"
               namespace-string
               (module-reference-namespace module-ref)))
    
    (let ()
      (pp 
      (make-module-info
       symbols: symbols
       exports: exports
       imports: imports
       imports-for-syntax: imports-for-syntax
       definitions: definitions
       runtime-dependencies: runtime-dependencies
       compiletime-dependencies: compiletime-dependencies
       options: (table-ref tbl 'options '())
       cc-options: (table-ref tbl 'cc-options "")
       ld-options-prelude: (table-ref tbl 'ld-options-prelude "")
       ld-options: (table-ref tbl 'ld-options "")
       force-compile: (table-ref tbl 'force-compile #f)
       namespace-string: namespace-string))
      (pp definitions))
    
    (make-module-info
     symbols: symbols
     exports: exports
     imports: imports
     imports-for-syntax: imports-for-syntax
     definitions: definitions
     runtime-dependencies: runtime-dependencies
     compiletime-dependencies: compiletime-dependencies
     options: (table-ref tbl 'options '())
     cc-options: (table-ref tbl 'cc-options "")
     ld-options-prelude: (table-ref tbl 'ld-options-prelude "")
     ld-options: (table-ref tbl 'ld-options "")
     force-compile: (table-ref tbl 'force-compile #f)
     namespace-string: namespace-string)))
