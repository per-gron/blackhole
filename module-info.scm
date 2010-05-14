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

(define (recreate-module-environment module-ref
                                     namespace-string
                                     definitions
                                     imports
                                     imports-for-syntax)
  (let ((env (make-top-environment module-ref))
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
              (let* ((ns (box #f))
                     (env (make-environment parent-env ns)))
                (table-set! memo-table
                            ls-env
                            env)
                (set-box!
                 ns
                 (map (lambda (ls-def)
                        (let ((mac-name (car ls-def))
                              (unique-name (cadr ls-def))
                              (macro-ls-env (caddr ls-def)))
                          (list (cons 0 mac-name)
                                'mac
                                unique-name ;; TODO Is this right?
                                (make-env-from-letsyntax-environment
                                 env
                                 macro-ls-env)
                                unique-name)))
                   ls-env))

                env))))))

      (for-each
          (lambda (def)
            (cond
             ((eq? 'def (cadr def))
              ;; Regular define
              (environment-add-def!
               env
               ;; The name it's imported as
               (car def)
               ;; The name it's imported from
               (gen-symbol namespace-string (car def))
               phase-number: 0))
             ((eq? 'mac (cadr def))
              ;; Macro
              (environment-add-mac!
               env
               ;; The name it's imported as
               (car def)
               ;; The macro procedure
               (cons module-ref (car def))
               ;; The macro's environment
               (make-env-from-letsyntax-environment env (caddr def))
               phase-number: 0))
             (else
              (error "Internal error"))))
        definitions))

    (module-add-defs-to-env imports env phase-number: 0)
    (module-add-defs-to-env imports-for-syntax env phase-number: 1)
    
    env))

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
    
    (let (#;(env (recreate-module-environment
          ;; TODO This isn't used anywhere. And it
          ;; doesn't make sense that it isn't
          module-ref
          namespace-string
          definitions
          imports
          imports-for-syntax)))
      (pp 
      (make-module-info
       symbols: symbols
       exports: exports
       imports: imports
       imports-for-syntax: imports-for-syntax
       runtime-dependencies: runtime-dependencies
       compiletime-dependencies: compiletime-dependencies
       options: (table-ref tbl 'options '())
       cc-options: (table-ref tbl 'cc-options "")
       ld-options-prelude: (table-ref tbl 'ld-options-prelude "")
       ld-options: (table-ref tbl 'ld-options "")
       force-compile: (table-ref tbl 'force-compile #f)
       namespace-string: namespace-string))
      (make-module-info
       symbols: symbols
       exports: exports
       imports: imports
       imports-for-syntax: imports-for-syntax
       runtime-dependencies: runtime-dependencies
       compiletime-dependencies: compiletime-dependencies
       options: (table-ref tbl 'options '())
       cc-options: (table-ref tbl 'cc-options "")
       ld-options-prelude: (table-ref tbl 'ld-options-prelude "")
       ld-options: (table-ref tbl 'ld-options "")
       force-compile: (table-ref tbl 'force-compile #f)
       namespace-string: namespace-string))))
