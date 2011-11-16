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
                     (error "c-initialize is not implemented"))
                    
                    ((c-declare)
                     ;; TODO
                     (error "c-declare is not implemented"))

                    ((##define-macro
                      ##define-syntax)
                     (error "##define-macro and ##define-syntax are not allowed" code))
                    
                    ((cond-expand
                      let-syntax
                      letrec-syntax
                      ##define-macro
                      ##define-syntax
                      define-macro
                      define-syntax)
                     ;; This shouldn't happen
                     (error "Internal error in Black Hole, transform-to-let (1)" code))
                    
                    ((declare
                      ##declare)
                     source)
                    
                    ((##define define)
                     (if (not (and (pair? (cdr code))
                                   (symbol? (expr*:value (cadr code)))))
                         (error "Internal error in Black Hole, transform-to-let (2)"
                                code))
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
      (let (;; This needs to be called before we map over names
            (body (loop source)))
        `(let ,(map (lambda (name) `(,name #!unbound))
                 names)
           ,body
           ,rest)))))

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

(define loaded-module-sym 'bh#sym#loaded-module)
(define expansion-phase-sym 'bh#sym#expansion-phase)
(define name-sym 'bh#sym#name)
(define val-sym 'bh#sym#val)

(define (generate-module-instance-symbol dep #!optional (extra ""))
  (string->symbol
   (string-append (module-reference-namespace
                   dep)
                  "bh#dep#"
                  extra)))

(define (generate-runtime-code namespace-string
                               module-reference
                               expanded-code)
  (let* (;; Keys are absolute module references, values are the relative ones
         (dep-table (make-table))

         (register-dep!
          (lambda (def)
            (let ((ref (caddr def)))
              (if ref
                  (table-set! dep-table
                              (module-reference-absolutize ref
                                                           module-reference)
                              ref))))))
    (define result-code
      (clone-sexp
       expanded-code
       (lambda (def phase)
         (if (not (expansion-phase-runtime? phase))
             (error "Internal error in Black Hole, generate-macro-code"))
         (register-dep! def)
         (cadr def))
       (lambda (def phase val)
         (if (not (expansion-phase-runtime? phase))
             (error "Internal error in Black Hole, generate-macro-code"))
         (register-dep! def)
         `(set! ,(cadr def) ,val))))
    
    (values result-code
            ;; This creates a list of runtime dependencies
            (map cdr (table->list dep-table)))))

(define (module-instance-let-fn table)
  (let ((result '()))
    (table-for-each
     (lambda (dep sym-pair)
       (cond
        ((eqv? 'no-global-state sym-pair)
         ;; Do nothing
         #!void)

        ((pair? sym-pair)
         (let ((sym (generate-module-instance-symbol dep "instance"))
               (get-sym (car sym-pair))
               (set-sym (cadr sym-pair))
               (relative-ref (caddr sym-pair)))
           (push!
            result
            `(,sym
              (let ((tmp (bh#module-instance-ref
                          ,expansion-phase-sym
                          (bh#module-reference-absolutize
                           (bh#u8vector->module-reference
                            ',(module-reference->u8vector relative-ref))
                           (bh#loaded-module-reference
                            ,loaded-module-sym)))))
                (or tmp
                    (error
                     "Internal error in Black Hole (module-macroexpansion.scm)")))))
           (push!
            result
            `(,get-sym
              (let ((tmp (bh#module-instance-getter ,sym)))
                (or tmp
                    (error
                     "Internal error in Black Hole (module-macroexpansion.scm)")))))
           (push!
            result
            `(,set-sym
              (let ((tmp (bh#module-instance-setter ,sym)))
                (or tmp
                    (error
                     "Internal error in Black Hole (module-macroexpansion.scm)")))))))

        (else
         (error "Internal error in module-instance-let-fn"
                sym-pair))))
     table)
    
    (reverse! result)))

(define (clone-sexp/sym-table ref->sym-table base-module-ref sexp)
  (define (module-ref->sym-pair ref relative-ref)
    (let ((info (loaded-module-info (module-reference-ref ref))))
      (if (module-info-no-global-state info)
          (begin
            (table-set! ref->sym-table ref 'no-global-state)
            'no-global-state)
          (let* ((sym (generate-module-instance-symbol ref "instance"))
                 (get-sym (generate-module-instance-symbol ref "get"))
                 (set-sym (generate-module-instance-symbol ref "set"))
                 (sym-pair (list get-sym
                                 set-sym
                                 relative-ref)))
            (table-set! ref->sym-table
                        ref
                        sym-pair)
            sym-pair))))
  
  (let ((get-ref
         (lambda (relative-ref)
           (and
            relative-ref
            (let ((ref (module-reference-absolutize relative-ref
                                                    base-module-ref)))
              (or (table-ref ref->sym-table ref #f)
                  (let ((sym-pair (module-ref->sym-pair ref relative-ref)))
                    (table-set! ref->sym-table ref sym-pair)
                    sym-pair)))))))
    (clone-sexp sexp
                ;; References to external modules
                (lambda (def phase)
                  (let* ((ref (caddr def))
                         (sym-pair (get-ref ref)))
                    (if (and ref
                             (not (eq? 'no-global-state
                                       sym-pair)))
                        `(,(car sym-pair)
                          ',(cadr def))
                        (cadr def))))
                ;; set!s to external modules
                (lambda (def phase val)
                  (let* ((ref (caddr def))
                         (sym-pair (get-ref ref)))
                    (if (and ref
                             (not (eq? 'no-global-state
                                       sym-pair)))
                        `(,(cadr sym-pair)
                          ',(cadr def)
                          ,val)
                        `(set! ,(cadr def) ,val)))))))

(define (generate-compiletime-code module-reference
                                   namespace-string
                                   expanded-code
                                   definitions)
  (let* ((names
          (map (lambda (x)
                 (cons (car x)
                       (gen-symbol namespace-string
                                   (car x))))
            (filter (lambda (x) (eq? 'def (cadr x)))
                    definitions)))
         (ref->rt-sym-table (make-table))
         (cloned-code
          (clone-sexp/sym-table ref->rt-sym-table
                                module-reference
                                expanded-code)))
    `(lambda (,loaded-module-sym ,expansion-phase-sym)
       (let* ,(module-instance-let-fn ref->rt-sym-table)
         ,(transform-to-let
           cloned-code
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
                             macros)
  (let* ((ref->ct-sym-table (make-table))
         (cloned-macros
          (map (lambda (name/sexp-pair)
                 `(cons
                   ',(car name/sexp-pair)
                   ,(clone-sexp/sym-table ref->ct-sym-table
                                          module-reference
                                          (cdr name/sexp-pair))))
            macros)))
    (values
     `(lambda (,loaded-module-sym ,expansion-phase-sym)
        (let* ,(module-instance-let-fn ref->ct-sym-table)
          (list ,@cloned-macros)))
     (map caddr (table->list ref->ct-sym-table)))))

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
        (unknown-definitions '())
        (macros '())
        (imports '())
        (exports #f)
        (options- '())
        (cc-options- "")
        (ld-options-prelude- "")
        (ld-options- "")
        (force-compile- #f)
        (no-global-state- #f)

        (calculate-letsyntax-environment-memo
         (make-table test: eq? hash: eq?-hash)))
    (parameterize
        ((*module-macroexpansion-import*
          (lambda (pkgs env phase)
            (push! imports
                   pkgs)
            (void)))
         
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
         
         (*module-macroexpansion-compile-options*
          (lambda (#!key options
                         cc-options
                         ld-options-prelude
                         ld-options
                         force-compile
                         no-global-state)
            (if options
                (set! options- options))
            (if cc-options
                (set! cc-options- cc-options))
            (if ld-options-prelude
                (set! ld-options-prelude- ld-options-prelude))
            (if ld-options
                (set! ld-options- ld-options))
            (if force-compile
                (set! force-compile- force-compile))
            (if no-global-state
                (set! no-global-state- no-global-state)))))
      
      (let ((expanded-code
             env
             (parameterize
                 ((*top-environment*
                   (make-top-environment module-reference))
                  (*expansion-phase*
                   (syntactic-tower-first-phase
                    (make-syntactic-tower)))
                  (*external-reference-access-hook*
                   (lambda (def phase unknown?)
                     (cond
                      (unknown?
                       (push! unknown-definitions
                              (list unknown? phase def))
                       (cadr def))
                      ((equal? (caddr def) module-reference)
                       (cadr def))
                      (else
                       (make-external-reference def phase)))))
                  (*external-reference-cleanup-hook*
                   (lambda (code)
                     (let ((instance-ref
                            (lambda (phase def)
                              `(bh#module-instance-ref
                                ',phase
                                ',(module-reference-absolutize
                                   (caddr def)
                                   (environment-module-reference
                                    ;; Beware of ugly use of the
                                    ;; dynamic environment
                                    (*top-environment*)))))))
                       (clone-sexp
                        code
                        (lambda (def phase)
                          (if (or (zero? (expansion-phase-number phase))
                                  ;; (caddr def) is #f when
                                  ;; referencing built-in functions
                                  (not (caddr def)))
                              (cadr def)
                              `((bh#module-instance-getter
                                 ,(instance-ref phase def))
                                ',(cadr def))))
                        (lambda (def phase val)
                          (if (or (zero? (expansion-phase-number phase))
                                  ;; (caddr def) is #f when
                                  ;; referencing built-in functions
                                  (not (caddr def)))
                              `(set! ,(cadr def) ,val)
                              `((bh#module-instance-setter
                                 ,(instance-ref phase def))
                                ',(cadr def)
                                val))))))))
               (values (expand-macro sexpr)
                       (*top-environment*))))
            (imports
             (apply append imports)))

        (let ((undefined-names '()))
          (for-each
              (lambda (name/phase/def-list)
                (apply
                 (lambda (name phase def)
                   (if (not (environment-get
                             env
                             name
                             phase-number: (expansion-phase-number phase)))
                       (push! undefined-names name)))
                 name/phase/def-list))
            unknown-definitions)

          (if (not (null? undefined-names))
              (error "These variables are undefined:"
                     (remove-duplicates undefined-names)
                     module-reference)))
        
        ;; TODO Add something to check for duplicate imports and
        ;; exports.
        (let ((export-defs
               (if exports
                   (resolve-exports (apply append exports)
                                    env)
                   (macroexpansion-symbol-defs
                    ;; TODO This map feels quite misplaced. Change
                    ;; macroexpansion-symbol-defs
                    (map (lambda (def)
                           (cons (car def)
                                 (cadr def)))
                      definitions)
                    env)))
              (import-defs
               import-module-refs ;; TODO Remove this return value.
               (resolve-imports imports
                                module-reference
                                relative: #t))

              (ns-str
               (environment-namespace
                env
                phase-number: 0)))
          (let ((runtime-code
                 runtime-deps
                 (generate-runtime-code ns-str
                                        module-reference
                                        expanded-code))

                (visit-code
                 compiletime-deps
                 (generate-visit-code module-reference macros)))
            (values runtime-code
                    (and
                     (not no-global-state-)
                     (generate-compiletime-code module-reference
                                                ns-str
                                                expanded-code
                                                definitions))
                    visit-code
                    (let* ((info
                            `((definitions ,@definitions)
                              (imports ,@import-defs)
                              (exports ,@export-defs)
                              (runtime-dependencies ,@runtime-deps)
                              (compiletime-dependencies ,@compiletime-deps)
                              (namespace-string ,@ns-str)
                              (options ,@options-)
                              (cc-options ,@cc-options-)
                              (ld-options-prelude ,@ld-options-prelude-)
                              (ld-options ,@ld-options-)
                              (force-compile ,@force-compile-)
                              (no-global-state ,@no-global-state-)))
                           (vec (module-reference->u8vector info)))
                      `',vec))))))))
