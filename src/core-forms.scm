;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                           Core forms                             ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; This file is closely tied to hygiene.scm

(##define-macro (define-env name prefix macs)
  (let* ((gen-symbol
          (lambda (ns sym)
            (string->symbol
             (string-append
              ns
              (symbol->string sym)))))
         (macro-names
          (map (lambda (mac)
                 (string->symbol
                  (string-append
                   prefix
                   (symbol->string (car mac))
                   "|||macro|||")))
               macs)))
    `(begin
       ,@(map (lambda (mac name)
                `(define ,name
                   ,(cadr mac)))
              macs macro-names)
       (define ,name #f)
       (let ((-ns-
              (list->table
               (list
                ,@(map (lambda (mac mac-name)
                         (list 'list
                               `(cons #f ',(car mac))
                               ''mac
                               mac-name
                               name))
                       macs macro-names)))))
       (set! ,name
             (make-environment #f ns: -ns-))))))

(define-env inside-letrec-environment
  "bh#inside-letrec-env-"
  ((define
     (lambda (code env mac-env)
       (let ((src (expr*:transform-to-lambda
                   (expand-syncapture
                    (cdr (expand-syncapture (expr*:value code)
                                            env))
                    env))))
         `(,transform-forms-to-triple-define-constant
           ,(let-expr* (name (car src))
              (make-syntactic-closure env
                                      '()
                                      name))
           ,(let ((src-cdr (cdr src)))
              (if (pair? src-cdr)
                  (car src-cdr)
                  #!void))))))

   (define-syntax
     (lambda (source env mac-env)
       (with-expr* source
         (list transform-forms-to-triple-define-syntax-constant
               (let-expr* (expander (cadr source))
                 (make-syntactic-closure env
                                         '()
                                         expander))
               (caddr source)))))
   
   (let-syntax
       (lambda (code env mac-env)
         (let/letrec-syntax-helper
          #f
          code
          env
          (lambda (body inner-env name/code-pairs)
            ((inside-letrec)
             body
             inner-env)))))

   (letrec-syntax
       (lambda (code env mac-env)
         (let/letrec-syntax-helper
          #t
          code
          env
          (lambda (body inner-env name/code-pairs)
            ((inside-letrec)
             body
             inner-env)))))
   (begin
     (lambda (code env mac-env)
       ((inside-letrec)
        (cdr (expand-syncapture (expr*:value code)
                                env))
        env)))))

;; This stuff is for use by the machinery that extracts module information
(define-macro (make-macroexpansion-vars . vars)
  (let ((syms (map (lambda (var)
                     (string->symbol
                      (string-append
                       "*module-macroexpansion-"
                       (symbol->string (car var))
                       "*")))
                   vars))
        (defaults (map cadr vars)))
    `(begin
       ,@(map (lambda (sym default)
                `(define ,sym (make-parameter ,default)))
              syms defaults))))

(make-macroexpansion-vars (import
                           (lambda (pkgs env phase)
                             (void)))
                          (export
                           (lambda (exports) (void)))
                          (define
                            (lambda (name) (void)))
                          (define-syntax
                            (lambda (name proc-sexp env) (void)))
                          (compile-options
                           (lambda _ (void))))

(define-env builtin-environment
  "bh#"
  ((import
    (lambda (source env mac-env)
      (if (not (environment-top? env))
          (error "Incorrectly placed import form"
                 (expr*:strip-locationinfo source)))

      (let ((pkgs (extract-synclosure-crawler
                   (cdr (expr*:strip-locationinfo source))))
            (phase (*expansion-phase*)))
        (module-import pkgs env phase)
        ((*module-macroexpansion-import*) pkgs env phase))))

   (export
    (lambda (code env mac-env)
      (if (or (not (environment-top? env))
              (not (environment-module-reference env)))
          (error "Incorrectly placed export form"
                 (expr*:strip-locationinfo code)))
      ((*module-macroexpansion-export*)
       (extract-synclosure-crawler
        (cdr (expr*:strip-locationinfo code))))
      (void)))
   
   (module
    (lambda (code env mac-env)
      (apply
       (lambda (#!optional name)
         (if (not (environment-top? env))
             (error "Incorrectly placed module form"))
         (module-module name))
       (cdr (expr*:strip-locationinfo code)))))

   (quote
    (lambda (code env mac-env)
      (extract-synclosure-crawler code)))

   (quasiquote
    (lambda (code env mac-env)
      (with-expr* code
        (list
         'quasiquote
         (parameterize
             ((inside-letrec #f))
           (hyg-expand-macro-quasiquote
            env
            (cadr (expand-syncapture
                   code
                   env))))))))
   
   (define
     (lambda (code env mac-env)
       (let* ((code-cdr (cdr (expand-syncapture
                              (expr*:value code)
                              env)))
              (src (expr*:transform-to-lambda
                    (if (null? code-cdr)
                        (error "Ill-formed define form" code)
                        code-cdr))))
         (cond
          ((top-level)
           (let* ((name-form (car (expr*:value src)))
                  (name (expand-synclosure name-form env))
                  (name-symbol (expr*:value name))
                  (def-env (if (syntactic-closure? name-form)
                               (syntactic-closure-env name-form)
                               env))
                  (ns (environment-add-define! def-env
                                               name-symbol)))
             ((*module-macroexpansion-define*) name-symbol)
             
             (expr*:value-set
              code
              `(,(expr*:value-set (car (expr*:value code)) 'define)
                ,(with-expr* name
                   (gen-symbol ns name))
                ,(expand-macro (let ((src-v (expr*:value src)))
                                 (if (pair? src-v)
                                     (let ((src-v-cdr (cdr src-v)))
                                       (if (pair? src-v-cdr)
                                           (car src-v-cdr)
                                           #!void))
                                     (error "Ill-formed define form"
                                            code)))
                               env)))))
          
          (else
           (error "Incorrectly placed define:"
                  (expr*:strip-locationinfo code)))))))

   (define-syntax
     (lambda (source env mac-env)
       (let ((code (expr*:value source)))
         (if (not (= 3 (length code)))
             (error "Invalid define-syntax form (wrong number of arguments)"
                    (expr*:strip-locationinfo source)))
         
         (let* ((name (cadr code))
                (trans (caddr code)) ;; trans is for transformer expression
                (before-name (expr*:value name)))
           (cond
            ((top-level)
             (let ((expanded-trans
                    (parameterize
                     ((*expansion-phase* (expansion-phase-next-phase
                                          (*expansion-phase*)))
                      (inside-letrec #f))
                     (expand-macro trans env)))
                   (transformer-name
                    (generate-unique-macro-name env
                                                before-name)))
               (environment-add-macro-fun before-name
                                          expanded-trans
                                          env)
               ((*module-macroexpansion-define-syntax*)
                before-name
                expanded-trans
                env)
               
               (void)))
            
            (else
             (error "Incorrectly placed define-syntax:"
                    (expr*:strip-locationinfo code))))))))
   
   (begin
     (lambda (code env mac-env)
       (with-expr* code
         `(begin
            ,@(map
                  (lambda (x)
                    (with-expr* x
                      (expand-macro x env)))
                (cdr code))))))
   
   (let
       (lambda (code env mac-env)
         (let/letrec-helper #f code env)))

   (letrec
       (lambda (code env mac-env)
         (let/letrec-helper #t code env)))
   
   (let-syntax
       (lambda (code env mac-env)
         (let/letrec-syntax-helper #f
                                   code
                                   env
                                   (top-level-let/letrec-syntax-helper
                                    env))))

   (letrec-syntax
       (lambda (code env mac-env)
         (let/letrec-syntax-helper #t
                                   code
                                   env
                                   (top-level-let/letrec-syntax-helper
                                    env))))
   
   (lambda
       (lambda (code env mac-env)
         (lambda-helper code env)))

   (let*
       (lambda (code env mac-env)
         (with-expr* code
           (apply
            (lambda (params-source . body)
              (let ((params (expr*:value params-source)))
                ;; Two null? checks are needed to avoid unneccesary
                ;; (let () ...)s while still allowing (let* () ...)
                (expand-macro
                 (if (null? params)
                     `(let () ,@body)
                     (let* ((bindings-source (car params))
                            (bindings (expr*:value bindings-source)))
                       (if (not
                            (and (list? bindings)
                                 (= 2 (length bindings))))
                           (error "Invalid binding"
                                  (expr*:strip-locationinfo code)))
                       `(let (,bindings-source)
                          ,@(if (null? (cdr params))
                                body
                                `((let* ,(cdr params)
                                    ,@body))))))
                 env)))
            (expand-syncapture (cdr code) env)))))

   (define-macro
     (lambda (source env mac-env)
       (expand-macro
        (with-expr* source
          (let ((src (expr*:value
                      (expr*:transform-to-lambda (cdr source)))))
            `(,(make-syntactic-closure
                empty-environment
                '()
                'define-syntax)
              ,(car src)
              (bh#nh-macro-transformer ,(cadr src)))))
        env)))

   (declare
    (lambda (code env mac-env)
      `(declare
        ,@(cdr (extract-synclosure-crawler
                (expr*:strip-locationinfo code))))))
   
   (cond
    (lambda (source env mac-env)
      (with-expr* source
        (cons
         (expr*:value-set (car source)
                          'cond)
         (map (lambda (inner-source)
                (with-expr* inner-source
                 (let* ((hd-source
                         (if (pair? inner-source)
                             (car inner-source)
                             (error "Invalid cond form: "
                                    (expr*:strip-locationinfo
                                     source))))
                        (hd (expr*:value hd-source)))
                   (cond
                    ((and (identifier? hd)
                          (identifier=? empty-environment
                                        'else
                                        env
                                        hd))
                     (cons (expr*:value-set hd-source
                                            'else)
                           (expand-macro (cdr inner-source)
                                         env)))

                    ((and (pair? (cdr inner-source))
                          (identifier? (expr*:value (cadr inner-source)))
                          (identifier=? empty-environment
                                        '=>
                                        env
                                        (expr*:value
                                         (cadr inner-source))))
                     (cons (expand-macro hd-source env)
                           (cons (expr*:value-set (cadr inner-source)
                                                  '=>)
                                 (expand-macro (cddr inner-source)
                                               env))))
                    
                    (else
                     (expand-macro inner-source env))))))
           
           (cdr source))))))

   (cond-expand
    (lambda (source env mac-env)
      (expand-macro
       (##deconstruct-call
        source
        -1
        (lambda clauses
          (cond-expand-build source
                             clauses
                             ##cond-expand-features)))
       env)))

   (case
       (lambda (source env mac-env)
         (with-expr* source
           `(,(expr*:value-set (car source)
                               'case)
             ,(expand-macro (cadr source) env)
             ,@(map (lambda (inner-source)
                      (let ((inner-code (expr*:value inner-source)))
                        `(,(extract-synclosure-crawler
                            (car inner-code))
                          ,@(map (lambda (f)
                                   (expand-macro f env))
                              (cdr inner-code)))))
                 (cddr source))))))

   (time
    (lambda (source env mac-env)
      (with-expr* source
        (if (not (= 2 (length source)))
            (error "Ill-formed special form"
                   (expr*:strip-locationinfo source)))
        `(,(expr*:value-set (car source) '##time)
          (lambda () ,@(expand-macro (cdr source)
                                     env))
          ',(cadr source)))))

   (c-declare
    (lambda (source env mac-env)
      ((*module-macroexpansion-compile-options*)
       force-compile: #t
       no-global-state: #t)
      (extract-synclosure-crawler source)))

   (c-initialize
    (lambda (source env mac-env)
      ((*module-macroexpansion-compile-options*)
       force-compile: #t
       no-global-state: #t)
      (extract-synclosure-crawler source)))

   (c-define-type
    (lambda (source env mac-env)
      ((*module-macroexpansion-compile-options*)
       force-compile: #t
       no-global-state: #t)
      (extract-synclosure-crawler source)))

   (c-lambda
    (lambda (source env mac-env)
      ((*module-macroexpansion-compile-options*)
       force-compile: #t
       no-global-state: #t)
      (extract-synclosure-crawler source)))

   (c-define
    ;; TODO I'm pretty sure that this isn't correct; you have to
    ;; macro-expand the body of the function.
    (lambda (source env mac-env)
      ((*module-macroexpansion-compile-options*)
       force-compile: #t
       no-global-state: #t)
      (extract-synclosure-crawler source)))

   (receive
    (lambda (source env mac-env)
      (with-expr* source
        (if (< (length source) 4)
            (error "Invalid receive form"
                   (expr*:strip-locationinfo source)))
        (apply
         (lambda (formals expression . body)
           (let ((lmb (make-syntactic-closure env '() 'lambda)))
             (expand-macro
              `(,(make-syntactic-closure env '() 'call-with-values)
                (,lmb () ,expression)
                (,lmb ,formals
                      ,@body))
              env)))
         (cdr source)))))

   (do
    (lambda (source env mac-env)
      (with-expr* source
        (let* ((assert
                (lambda (x)
                  (if (not x)
                      (error "Invalid do form"
                             (expr*:strip-locationinfo source)))))
               (c (lambda (x)
                    (make-syntactic-closure builtin-environment '() x)))
               (do-loop (c 'do-loop)))
          (assert (>= (length source) 3))
          (let ((vars (expr*:value (cadr source)))
                (test (expr*:value (caddr source)))
                (exprs (expr*:value (cdddr source))))
            (assert (and (list? vars)
                         (list? test)
                         (not (null? test))
                         (list? exprs)))
            (expand-macro
             `(,(c 'let)
               ,do-loop
               ,(map (lambda (var)
                       (let ((var (expr*:value var)))
                         (assert (and (list? var)
                                      (<= 2 (length var) 3)))
                         (list (car var)
                               (cadr var))))
                  vars)
               (,(c 'cond)
                (,(car test)
                 ,(if (null? (cdr test))
                      #!void
                      `(,(c 'begin) ,@(cdr test))))
                (,(c 'else)
                 ,@exprs
                 (,do-loop
                  ,@(map (lambda (var)
                           (let ((var (expr*:value var)))
                             (if (null? (cddr var))
                                 (car var)
                                 (caddr var))))
                      vars)))))
             env))))))
   
   (compile-options
    (nh-macro-transformer
     (lambda args
       (apply
        (*module-macroexpansion-compile-options*)
        args)
       (void))))

   (define-type
     (nh-macro-transformer
      (lambda args
        (expand 'define-type #f #f args))))

   (define-record-type
     (nh-macro-transformer
      (lambda (name constructor predicate . fields)
        `(define-type ,name
           constructor: ,constructor
           predicate: ,predicate
           ,@fields))))

   (define-structure
     (nh-macro-transformer
      (lambda args
        (expand 'define-type #f #f args))))))
