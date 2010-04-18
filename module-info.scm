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
  (exports-list read-only:) ;; As written in the source file
  (imports-list read-only:) ;; As written in the source file
  (imports-for-syntax-list read-only:) ;; As written in the source file
  (exports read-only:) ;; In digested form
  (imports read-only:) ;; In digested form
  (imports-for-syntax read-only:) ;; In digested form
  
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
                          (exports-list '())
                          (imports-list '())
                          (imports-for-syntax-list '())
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
                             exports-list
                             imports-list
                             imports-for-syntax-list
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

(define (make-module-info-from-alist module-ref module-info-alist)
  (if (not (module-reference-absolute? module-ref))
      (error "Module reference must be absolute"))
  
  (let* ((tbl (list->table module-info-alist))
         (definitions (table-ref tbl 'definitions '()))
         (exports (table-ref tbl 'exports #f))
         (imports (table-ref tbl 'imports '()))
         (imports-for-syntax (table-ref tbl 'imports-for-syntax '()))
         (namespace-string (table-ref tbl 'namespace-string))
         (symbols (map (lambda (def)
                         (cons (car def)
                               (cadr def)))
                    definitions)))

    (if (not (equal? (module-reference-namespace module-ref)
                     namespace-string))
        (error "The compiled module's namespace and its current \
                namespace don't match"
               namespace-string
               (module-reference-namespace module-ref)))
    
    (call-with-values
        (lambda ()
          (resolve-imports imports))
      (lambda (import-defs import-module-refs)
        (call-with-values
            (lambda ()
              (resolve-imports imports-for-syntax))
          (lambda (import-defs-for-syntax import-for-syntax-module-refs)
            (let ((env (recreate-module-environment
                        module-ref
                        namespace-string
                        definitions
                        import-defs
                        import-defs-for-syntax)))
              (call-with-values
                  (lambda ()
                    (if exports
                        (resolve-exports exports env)
                        (values (macroexpansion-symbol-defs symbols
                                                            env)
                                '())))
                (lambda (export-defs export-uses-module-refs)
                  (make-module-info
                   symbols: symbols
                   exports-list: exports
                   imports-list: imports
                   imports-for-syntax-list: imports-for-syntax
                   exports: export-defs
                   imports: import-defs
                   imports-for-syntax: import-defs-for-syntax
                   runtime-dependencies: (append import-module-refs
                                                 export-uses-module-refs)
                   compiletime-dependencies: import-for-syntax-module-refs
                   options: (table-ref tbl 'options '())
                   cc-options: (table-ref tbl 'cc-options "")
                   ld-options-prelude: (table-ref tbl 'ld-options-prelude "")
                   ld-options: (table-ref tbl 'ld-options "")
                   force-compile: (table-ref tbl 'force-compile #f)
                   namespace-string: namespace-string))))))))))
