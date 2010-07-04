;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;               Utilities for module macroexpansion                ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define (transform-to-let source rest)
  (let ((names '()))
    (letrec
        ((loop
          (lambda (source)
            (let ((code
                   (expr*:value source)))
              (cond
               ((pair? code)
                (let ((code-car (expr*:value (car code))))
                  (case code-car
                    ((begin ##begin)
                     (expr*:value-set
                      source
                      (cons (car code)
                            (map loop
                              (filter (lambda (x)
                                        (not (eq? #!void
                                                  (expr*:value x))))
                                      (cdr code))))))
                    
                    ((c-define)
                     ;; TODO
                     (error "c-define is not implemented"))
                    
                    ((c-define-type)
                     ;; TODO
                     (error "c-define-type is not implemented"))
                    
                    ((c-initialize)
                     ;; TODO
                     (error "c-define-type is not implemented"))
                    
                    ((c-declare)
                     ;; TODO
                     (error "c-declare is not implemented"))
                    
                    ((cond-expand
                      let-syntax
                      letrec-syntax
                      ##define-macro
                      ##define-syntax
                      define-macro
                      define-syntax)
                     ;; This shouldn't happen
                     (error "Internal error in transform-to-let"))
                    
                    ((declare)
                     source)
                    
                    ((##define define)
                     (if (not (and (pair? (cdr code))
                                   (symbol? (expr*:value (cadr code)))))
                         (error "Internal error in transform-to-let"))
                     (set! names (cons (expr*:value (cadr code)) names))
                     (expr*:value-set source
                                      (cons 'set!
                                            (cdr code))))
                    
                    (else
                     source))))
               
               ((eq? #!void code)
                #!void)
               
               (else
                source))))))
      `(let ,(map (lambda (name) `(name #!unbound))
               names)
         ,(loop source)
         ,rest))))

(define-type external-reference
  id: 40985F98-6814-41B6-90FE-0FBFB1A8F42D
  ref
  (phase unprintable:))

(define (clone-sexp source transform-access transform-set!)
  (let beginning-of-list ((source source))
    (let ((code (expr*:value source)))
      (expr*:value-set
       source
       ;; TODO Make this function tail recursive
       (let found-pair ((code code) (beginning #t))
         (cond
          ((and (pair? code)
                beginning
                (eq? 'set! (car code)))
           (let ((ref (expr*:value (cadr code))))
             (if (external-reference? ref)
                 (beginning-of-list
                  (transform-set! (external-reference-ref ref)
                                  (external-reference-phase ref)
                                  (caddr code)))
                 (cons 'set!
                       (found-pair (cdr code) #f)))))
          
          ((pair? code)
           (cons (beginning-of-list (car code))
                 (found-pair (cdr code) #f)))
          
          ((external-reference? code)
           (beginning-of-list
            (transform-access (external-reference-ref code)
                              (external-reference-phase code))))
          
          (else
           code)))))))

(define loaded-module-sym 'module#sym#loaded-module)
(define expansion-phase-sym 'module#sym#expansion-phase)
(define name-sym 'module#sym#name)
(define val-sym 'module#sym#val)

(define (generate-module-instance-symbol dep #!optional (extra ""))
  (string->symbol
   (string-append (module-reference-namespace
                   dep)
                  "module#dep#"
                  extra)))

(define (generate-runtime-code namespace-string
                               module-reference
                               expanded-code)
  (clone-sexp
   expanded-code
   (lambda (def phase)
     (if (not (expansion-phase-runtime? phase))
         (error "Internal error in generate-macro-code"))
     (cadr def))
   (lambda (def phase val)
     (if (not (expansion-phase-runtime? phase))
         (error "Internal error in generate-macro-code"))
     `(set! ,(cadr def) ,val))))

(define (module-instance-let-fn dep table)
  (let ((sym (generate-module-instance-symbol dep "instance"))
        (get-sym (generate-module-instance-symbol dep "get"))
        (set-sym (generate-module-instance-symbol dep "set")))
    (table-set! table dep (cons get-sym
                                set-sym))
    `((,sym
       (let ((tmp (module#module-instance-ref
                   ,expansion-phase-sym
                   (module#module-reference-absolutize
                    (module#u8vector->module-reference
                     ',(module-reference->u8vector dep))
                    (module#loaded-module-reference
                     ,loaded-module-sym)))))
         (or tmp (error "Internal error"))))
      (,get-sym
       (let ((tmp (module#module-instance-getter ,sym)))
         (or tmp (error "Internal error"))))
      (,set-sym
       (let ((tmp (module#module-instance-setter ,sym)))
         (or tmp (error "Internal error")))))))

(define (clone-sexp/sym-table sexp ref->sym-table)
  (clone-sexp sexp
              ;; References to external modules
              (lambda (def phase)
                (let ((ref (caddr def)))
                  (if ref
                      `(,(car
                          (table-ref ref->sym-table
                                     ref))
                        ',(cadr def))
                      (cadr def))))
              ;; set!s to external modules
              (lambda (def phase val)
                (let ((ref (caddr def)))
                  (if ref
                      `(,(cdr
                          (table-ref ref->sym-table
                                     ref))
                        ',(cadr def)
                        ,val)
                      `(set! ,(cadr def) ,val))))))

(define (generate-compiletime-code module-reference
                                   namespace-string
                                   expanded-code
                                   definitions
                                   dependencies)
  (let ((names
         (map (lambda (x)
                (cons (car x)
                      (gen-symbol namespace-string
                                  (car x))))
           (filter (lambda (x) (eq? 'def (cadr x)))
                   definitions)))
        (ref->rt-sym-table (make-table)))
    `(lambda (,loaded-module-sym ,expansion-phase-sym)
       (let* (,@(apply
                 append
                 (map (lambda (dep)
                        (module-instance-let-fn (module-reference-absolutize
                                                 dep
                                                 module-reference)
                                                ref->rt-sym-table))
                   dependencies)))
         ,(transform-to-let
           (clone-sexp/sym-table expanded-code
                                 ref->rt-sym-table)
           `(values
             (lambda (,name-sym)
               (case ,name-sym
                 ,@(map (lambda (name)
                          `((,(cdr name))
                            ,(cdr name)))
                     names)
                 (else (error "Unbound variable" ,name-sym))))
             (lambda (,name-sym ,val-sym)
               (case ,name-sym
                 ,@(map (lambda (name)
                          `((,(cdr name))
                            (set! ,(cdr name) ,val-sym)))
                     names)
                 (else (error "Unbound variable" ,name-sym))))))))))

(define (generate-visit-code module-reference
                             macros
                             dependencies)
  (let ((ref->ct-sym-table (make-table)))
    `(lambda (,loaded-module-sym ,expansion-phase-sym)
       (let* (,@(apply
                 append
                 (map (lambda (dep)
                        (module-instance-let-fn (module-reference-absolutize
                                                 dep
                                                 module-reference)
                                                ref->ct-sym-table))
                   dependencies)))
         (list
          ,@(map
                (lambda (name/sexp-pair)
                  `(cons
                    ',(car name/sexp-pair)
                    ,(clone-sexp/sym-table
                      (cdr name/sexp-pair)
                      ref->ct-sym-table)))
              macros))))))

(define (calculate-letsyntax-environment memo-table env)
  (define (memoize-function-with-one-parameter fn)
    (lambda (param)
      (or (table-ref memo-table param #f)
          (let ((res (fn param)))
            (table-set! memo-table param res)
            res))))
  
  (letrec
      ((rec
        (lambda (env)
          (cond
           ((box? (env-ns env))
            (let ((rest
                   (rec (env-parent env))))
              (for-each
                  (lambda (ns-entry)
                    (if (eq? 'mac (cadr ns-entry))
                        (push!
                         rest
                         (list
                          ;; The macro name
                          (cdar ns-entry)
                          ;; The name of the procedure
                          (list-ref ns-entry 4)
                          ;; The let-syntax env of this macro
                          (rec (list-ref ns-entry 3))))))
                (unbox (env-ns env)))
              rest))
           (else
            '())))))
    ;; Memoize calculate-letsyntax-environment
    (set! rec
          (memoize-function-with-one-parameter
           rec))
    ;; Perform the computation
    (rec env)))

(define (module-macroexpand module-reference
                            sexpr
                            #!optional (tower (make-syntactic-tower)))
  (let ((definitions '())
        (macros '())
        (imports '())
        (imports-for-syntax '())
        (exports #f)
        (options- '())
        (cc-options- "")
        (ld-options-prelude- "")
        (ld-options- "")
        (force-compile- #f)

        (calculate-letsyntax-environment-memo
         (make-table test: eq? hash: eq?-hash)))
    (parameterize
        ((*module-macroexpansion-import*
          (lambda (pkgs env phase)
            (push! imports
                   pkgs)))
         
         (*module-macroexpansion-import-for-syntax*
          (lambda (pkgs env phase)
            (push! imports-for-syntax
                   pkgs)))
         
         (*module-macroexpansion-export*
          (lambda (e)
            (set! exports (cons e (or exports '())))))
         
         (*module-macroexpansion-define*
          (lambda (name)
            (push! definitions
                   (list name 'def))))
         
         (*module-macroexpansion-define-syntax*
          (lambda (name proc-sexp env)
            (push! definitions
                   (list name
                         'mac
                         (calculate-letsyntax-environment
                          calculate-letsyntax-environment-memo
                          env)))
            (push! macros
                   (cons name
                         proc-sexp))))
         
         (*module-macroexpansion-force-compile*
          (lambda ()
            (set! force-compile #t)))
         
         (*module-macroexpansion-compile-options*
          (lambda (#!key options
                         cc-options
                         ld-options-prelude
                         ld-options
                         force-compile)
            (if options
                (set! options- options))
            (if cc-options
                (set! cc-options- cc-options))
            (if ld-options-prelude
                (set! ld-options-prelude- ld-options-prelude))
            (if ld-options
                (set! ld-options- ld-options))
            (if force-compile
                (set! force-compile- force-compile)))))
      
      (let ((expanded-code
             env
             (parameterize
                 ((*top-environment*
                   (make-top-environment module-reference))
                  (*expansion-phase*
                   (syntactic-tower-first-phase
                    (make-syntactic-tower)))
                  (*external-reference-access-hook*
                   (lambda (def phase)
                     (if (equal? (caddr def) module-reference)
                         (cadr def)
                         (make-external-reference def phase))))
                  (*external-reference-cleanup-hook*
                   (lambda (code)
                     (clone-sexp
                      code
                      ;; TODO Do something with phase
                      (lambda (def phase) (cadr def))
                      ;; TODO Do something with phase
                      (lambda (def phase val)
                        `(set! ,(cadr def) ,val))))))
               (values (expand-macro sexpr)
                       (*top-environment*))))
            (imports
             (apply append imports))
            (imports-for-syntax
             (apply append imports-for-syntax)))
        ;; TODO Add something to check for duplicate imports and
        ;; exports.
        (let ((export-defs
               export-uses-module-refs
               (if exports
                   (resolve-exports (apply append exports)
                                    env)
                   (values
                    (macroexpansion-symbol-defs
                     ;; TODO This map feels quite misplaced. Change
                     ;; macroexpansion-symbol-defs
                     (map (lambda (def)
                            (cons (car def)
                                  (cadr def)))
                       definitions)
                     env)
                    '())))
              (import-defs
               import-module-refs
               (resolve-imports imports
                                module-reference
                                relative: #t))
              (import-for-syntax-defs
               import-for-syntax-module-refs
               (resolve-imports imports-for-syntax
                                module-reference
                                relative: #t)))

          (values (generate-runtime-code (environment-namespace env)
                                         module-reference
                                         expanded-code)
                  (generate-compiletime-code module-reference
                                             (environment-namespace env)
                                             expanded-code
                                             definitions
                                             import-module-refs)
                  (generate-visit-code module-reference
                                       macros
                                       import-for-syntax-module-refs)
                  (let* ((info
                          `((definitions ,@definitions)
                            (imports ,@import-defs)
                            (imports-for-syntax ,@import-for-syntax-defs)
                            (exports ,@export-defs)
                            (runtime-dependencies
                             ,@(append export-uses-module-refs
                                       import-module-refs))
                            (compiletime-dependencies
                             ,@import-for-syntax-module-refs)
                            (namespace-string ,@(environment-namespace env))
                            (options ,@options-)
                            (cc-options ,@cc-options-)
                            (ld-options-prelude ,@ld-options-prelude-)
                            (ld-options ,@ld-options-)
                            (force-compile ,@force-compile-)))
                         (vec (module-reference->u8vector info)))
                    `',vec)))))))
