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
  "module#inside-letrec-env-"
  ((define
     (lambda (code env mac-env)
       (let ((src (expr*:transform-to-lambda
                   (expand-syncapture
                    (cdr (expand-syncapture (expr*:value code)
                                            env))
                    env))))
         `(,transform-forms-to-triple-define-constant
           ,(expr*:value-set
             (car src)
             (make-syntactic-closure env
                                     '()
                                     (expr*:value (car src))))
           ,(let ((src-cdr (cdr src)))
              (if (pair? src-cdr)
                  (car src-cdr)
                  #!void))))))

   (define-syntax
     (lambda (source env mac-env)
       (let ((code (expr*:value source)))
         (expr*:value-set
          source
          (list transform-forms-to-triple-define-syntax-constant
                (expr*:value-set
                 (cadr code)
                 (make-syntactic-closure env
                                         '()
                                         (expr*:value (cadr code))))
                (caddr code))))))
   
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
                          (syntax-begin
                           (lambda (phase code)
                             (void)))
                          (export
                           (lambda (exports) (void)))
                          (define
                            (lambda (name) (void)))
                          (define-syntax
                            (lambda (name proc-sexp env) (void)))
                          (force-compile
                           (lambda () (void)))
                          (compile-options
                           (lambda (#!key options
                                          cc-options
                                          ld-options-prelude
                                          ld-options
                                          force-compile)
                             (void))))

(define-env builtin-environment
  "module#"
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

   (syntax-begin
    (lambda (code env mac-env)
      (if (not (environment-top? env))
          (error "Incorrectly placed syntax-begin form"
                 (expr*:strip-locationinfo code)))
      (let* ((next-phase
              (expansion-phase-next-phase
               (*expansion-phase*)))
             (expansion
              (parameterize
                  ((*expansion-phase* next-phase)
                   ;; Inside-letrec must be set to #f, otherwise
                   ;; strange errors will occur when the continuation
                   ;; that is within that closure gets invoked at the
                   ;; wrong time.
                   (inside-letrec #f))
                (expand-macro
                 (expr*:value-set
                  code
                  `(begin ,@(cdr (expr*:value code))))
                 env))))
        ((*module-macroexpansion-syntax-begin*)
         next-phase
         expansion)
        (parameterize
            ((*expansion-phase* next-phase))
          (eval-no-hook ((*external-reference-cleanup-hook*)
                         expansion))))))

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
         (if (or (not (environment-top? env))
                 (environment-module-reference env))
             (error "Incorrectly placed module form"))
         (module-module name))
       (cdr (expr*:strip-locationinfo code)))))

   (quote
    (lambda (code env mac-env)
      (extract-synclosure-crawler code)))

   (quasiquote
    (lambda (code env mac-env)
      (expr*:value-set
       code
       (cons 'quasiquote
             (list
              (hyg-expand-macro-quasiquote
               env
               (cadr (expand-syncapture
                      (expr*:value code)
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
              `(define ,(expr*:value-set
                         name
                         (gen-symbol ns
                                     (expr*:value name)))
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
       (expr*:value-set
        code
        `(begin
           ,@(map
              (lambda (x)
                (expr*:value-set x
                                 (expand-macro (expr*:value x)
                                               env)))
              (cdr (expr*:value code)))))))
   
   (let
       (lambda (code env mac-env)
         ;; TODO This doesn't generate source code locations correctly
         (let ((code (expr*:strip-locationinfo code)))
           (let/letrec-helper #f code env))))

   (letrec
       (lambda (code env mac-env)
         ;; TODO This doesn't generate source code locations correctly
         (let ((code (expr*:strip-locationinfo code)))
           (let/letrec-helper #t code env))))
   
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
         ;; TODO This doesn't generate source code locations correctly
         (let ((code (expr*:strip-locationinfo code)))
           (lambda-helper code env))))

   (let*
       (lambda (code env mac-env)
         ;; TODO This doesn't generate source code locations correctly
         (let ((code (expr*:strip-locationinfo code)))
           (apply
            (lambda (params . body)
              ;; Two null? checks are needed to avoid unneccesary
              ;; (let () ...)s while still allowing (let* () ...)
              (expand-macro
               (if (null? params)
                   `(let () ,@body)
                   (let ((bindings (car params)))
                     (if (not
                          (and (list? bindings)
                               (= 2 (length bindings))))
                         (error "Invalid binding" code))
                     `(let (,bindings)
                        ,@(if (null? (cdr params))
                              body
                              `((let* ,(cdr params)
                                  ,@body))))))
               env))
            (cdr (expand-syncapture code env))))))

   (define-macro
     (lambda (source env mac-env)
       (let ((code (expr*:value source)))
         (let* ((src (expr*:value
                      (expr*:transform-to-lambda (cdr code)))))
           (expand-macro
            (expr*:value-set
             source
             `(,(make-syntactic-closure
                 empty-environment
                 '()
                 'define-syntax)
               ,(car src)
               (module#nh-macro-transformer ,(cadr src))))
            env)))))

   (declare
    (lambda (code env mac-env)
      `(declare
        ,@(cdr (extract-synclosure-crawler
                (expr*:strip-locationinfo code))))))
   
   (cond
    (lambda (source env mac-env)
      (expr*:value-set
       source
       (let ((code (expr*:value source)))
         (cons
          (expr*:value-set (car code)
                           'cond)
          (map (lambda (inner-source)
                 (let* ((inner-code
                         (expr*:value inner-source))
                        (hd-source
                         (if (pair? inner-code)
                             (car inner-code)
                             (error "Invalid cond form: "
                                    (expr*:strip-locationinfo
                                     source))))
                        (hd (expr*:value hd-source)))
                   (expr*:value-set
                    inner-source
                    (cond
                     ((and (identifier? hd)
                           (identifier=? empty-environment
                                         'else
                                         env
                                         hd))
                      (cons (expr*:value-set hd-source
                                             'else)
                            (expand-macro (cdr inner-code)
                                          env)))

                     ((and (pair? (cdr inner-code))
                           (identifier? (expr*:value (cadr inner-code)))
                           (identifier=? empty-environment
                                         '=>
                                         env
                                         (expr*:value
                                          (cadr inner-code))))
                      (cons (expand-macro hd-source env)
                            (cons (expr*:value-set (cadr inner-code)
                                                   '=>)
                                  (expand-macro (cddr inner-code)
                                                env))))
                     
                     (else
                      (expand-macro inner-code
                                    env))))))
                      
               (cdr code)))))))

   (cond-expand
    (lambda (source env mac-env)
      (expand-macro
       (##deconstruct-call
        source
        -1
        (lambda clauses
          (cond-expand-build source
                             clauses
                             (cons 'black-hole ##cond-expand-features))))
       env)))

   (case
       (lambda (source env mac-env)
         (expr*:value-set
          source
          (let ((code (expr*:value source)))
            `(,(expr*:value-set (car code)
                                'case)
              ,(expand-macro (cadr code) env)
              ,@(map (lambda (inner-source)
                       (let ((inner-code (expr*:value inner-source)))
                         `(,(extract-synclosure-crawler
                             (car inner-code))
                           ,@(map (lambda (f)
                                    (expand-macro f env))
                                  (cdr inner-code)))))
                     (cddr code)))))))

   (time
    (lambda (source env mac-env)
      (expr*:value-set
       source
       (let ((code (expr*:value source)))
         (if (not (= 2 (length code)))
             (error "Ill-formed special form"
                    (expr*:strip-locationinfo source)))
         `(,(expr*:value-set (car code) '##time)
           (lambda () ,@(expand-macro (cdr code)
                                      env))
           ',(cadr code))))))

   (c-declare
    (lambda (source env mac-env)
      ((*module-macroexpansion-force-compile*))
      (extract-synclosure-crawler source)))

   (c-initialize
    (lambda (source env mac-env)
      ((*module-macroexpansion-force-compile*))
      (extract-synclosure-crawler source)))

   (c-define-type
    (lambda (source env mac-env)
      ((*module-macroexpansion-force-compile*))
      (extract-synclosure-crawler source)))

   (c-lambda
    (lambda (source env mac-env)
      ((*module-macroexpansion-force-compile*))
      (extract-synclosure-crawler source)))

   (c-define
    ;; TODO I'm pretty sure that this isn't correct; you have to
    ;; macro-expand the body of the function.
    (lambda (source env mac-env)
      ((*module-macroexpansion-force-compile*))
      (extract-synclosure-crawler source)))
   
   (receive
    (lambda (source env mac-env)
      (let ((code (expr*:value source)))
        (apply
         (lambda (formals expression . body)
           (let ((lmb (make-syntactic-closure env '() 'lambda)))
             (expand-macro
              (expr*:value-set
               source
               `(,(make-syntactic-closure env '() 'call-with-values)
                 (,lmb () ,expression)
                 (,lmb ,formals
                       ,@body)))
              env)))
         (cdr code)))))
   
   (compile-options
    (nh-macro-transformer
     (lambda (#!key options
                    cc-options
                    ld-options-prelude
                    ld-options
                    force-compile)
       ((*module-macroexpansion-compile-options*)
        options: options
        cc-options: cc-options
        ld-options-prelude: ld-options-prelude
        ld-options: ld-options
        force-compile: force-compile)
       (void))))

   (define-type
     (nh-macro-transformer
      (lambda args
        (expand 'define-type #f #f args))))

   (define-structure
     (nh-macro-transformer
      (lambda args
        (expand 'define-type #f #f args))))))
