;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                  Hygienic macro subsystem                        ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; Utilities

;; Takes an expression of the form ((mname . arglist) . funbody)
;; and transforms it into (mname (lambda arglist funbody))
;; (it does nothing if the input is already in the latter form)
(define (expr*:transform-to-lambda expr #!optional (lmb 'lambda))
  (let ((ret (let* ((code (expr*:value expr))
                    (code-car (extract-syntactic-closure-list
                               (expr*:value (car code)))))
               (if (pair? code-car)
                   `(,(expr*:car code-car)
                     (,lmb ,(cdr code-car)
                           ,@(cdr code)))
                   expr))))
    ret))

(define (dotted-map fun lst)
  (cond
   ((pair? lst)
    (cons (fun (car lst))
          (dotted-map fun (cdr lst))))
   ((null? lst) '())
   (else (fun lst))))


(define (dotted-map2 fun lst)
  (cond
   ((pair? lst)
    (cons (fun (car lst))
          (dotted-map2 fun (cdr lst))))
   ((null? lst) '())
   (else (list (fun lst)))))

(define (gen-symbol str sym)
  (string->symbol
   (string-append str
                  (symbol->string sym))))

;; Structures

(define-type env
  id: A8981FB8-BC38-47BA-8707-5A3F5962D610
  (parent unprintable:)
  module
  ;; It is unprintable because it often contains cyclic data
  ;; structures, which in practise crashes gambit when prettyprinting
  ;; it, which is a huge pain.
  (inner-ns unprintable:)
  (inner-ns-macro unprintable:)
  (top-ns unprintable:)
  (top-ns-macro unprintable:))

(define (make-environment parent
                          #!optional
                          (inner-ns '())
                          (inner-ns-macro '())
                          (top-ns (environment-top-ns parent))
                          (top-ns-macro (environment-top-ns-macro parent)))
  (make-env (and (env? parent) parent)
            (if (env? parent)
                (environment-module parent)
                parent)
            inner-ns
            inner-ns-macro
            top-ns
            top-ns-macro))

(define (environment-clone env)
  (make-env (environment-parent env)
            (environment-module env)
            (environment-inner-ns env)
            (environment-inner-ns-macro env)
            (environment-top-ns env)
            (environment-top-ns-macro)))

(define environment? env?)

(define environment-parent env-parent)

(define (environment-module env)
  (env-module env))

(define environment-top-ns env-top-ns)

(define (environment-top-ns-add env name str)
  (table-set! (environment-top-ns env) name str))

(define (environment-top-ns-get env name)
  (table-ref (environment-top-ns env) name #f))

(define environment-top-ns-macro env-top-ns-macro)

(define (environment-top-ns-macro-add env name fun m-env)
  (table-set! (environment-top-ns-macro env) name (list fun m-env)))

(define (environment-top-ns-macro-get env name)
  (table-ref (environment-top-ns-macro env) name #f))

(define environment-inner-ns env-inner-ns)
(define environment-inner-ns-set! env-inner-ns-set!)

(define (environment-inner-ns-get env name)
  (let* ((tns (environment-inner-ns env))
         (p (assq name tns)))
    (cond
     (p (cdr p))

     ((let ((ep (environment-parent env)))
        (and (not (string? ep)) ep)) =>
      (lambda (parent)
        (environment-inner-ns-get parent name)))

     (else #f))))

(define environment-inner-ns-macro env-inner-ns-macro)
(define environment-inner-ns-macro-set! env-inner-ns-macro-set!)

(define (environment-inner-ns-macro-get env name)
  (let* ((tns (environment-inner-ns-macro env))
         (p (assq name tns)))
    (cond
     (p (cdr p))

     ((let ((ep (environment-parent env)))
        (and (not (string? ep)) ep)) =>
      (lambda (parent)
        (environment-inner-ns-macro-get parent name)))

     (else #f))))

;; This function is a helper when finding whether an identifier is
;; in the current scope, and what it is. The primary problem it solves
;; is that macros have higher priority than normal variables, but the
;; depth of the scope has even higher importance.
;;   For instance, this should return #t:
;;  
;; (let-syntax ((test
;;               (syntax-rules ()
;;                 ((test) #f))))
;;   (let ((test (lambda () #t)))
;;     (test)))
(define (environment-inner-ns-search env name def-found macro-found)
  (cond
   ((assq name (environment-inner-ns-macro env)) =>
    (lambda (pair)
      (macro-found (cdr pair))))
   
   ((assq name (environment-inner-ns env)) =>
    (lambda (pair)
      (def-found (cdr pair))))

   ((let ((ep (environment-parent env)))
      (and (not (string? ep)) ep)) =>
    (lambda (parent)
      (environment-inner-ns-search parent name def-found macro-found)))

   (else
    #f)))

(define-type syntactic-closure
  id: 0B9FFE46-B995-48C2-B5E5-B66FD3FD335C
  constructor: make-syntactic-closure-internal
  
  (env read-only:)
  (ids read-only:)
  (form read-only:))

(define (make-syntactic-closure env ids form)
  (cond
   ((and (syntactic-closure? form)
         (null?
          (syntactic-closure-ids form)))
    form)

   ;; This, in combination with the next clause is an important
   ;; optimization because other parts of the code assumes that it is
   ;; done:
   ;;
   ;; What it does is to guarantee that identifiers are either just a
   ;; symbol, or a syntactic closure containing a symbol. In other
   ;; words, identifiers can never be multiple syntactic closures
   ;; nested in each other.
   ;;
   ;; There is no case where multiple nested syntactic closures that
   ;; are an identifier cannot be replaced with just a symbol or one
   ;; syntactic closure containing a symbol.
   ((and (syntactic-closure? form)
         (symbol? (syntactic-closure-form form)))
    form)

   ((symbol? form)
    (if (memq form ids)
        form
        (make-syntactic-closure-internal env '() form)))

   (else
    (make-syntactic-closure-internal env ids form))))

;; The structure that is returned by capture-syntactic-environment
(define-type syntactic-capture
  id: 268DA48C-6617-46FC-9DF9-F69233935278
  proc)

(define (capture-syntactic-environment proc)
  (make-syntactic-capture proc))

(define (make-top-environment ns)
  (make-environment ns
                    '()
                    '()
                    (make-table)
                    (make-table)))

(define empty-environment (make-top-environment #f))

(define top-environment
  (make-parameter (make-top-environment #f)))

(define scope-level (make-parameter 0))

(define (generate-namespace)
  (string-append
   "h"
   (number->string (scope-level))
   "#"))

(define (environment-add-macro-fun name fun env)
  (let ((fn
         (lambda (name env)
           (environment-top-ns-macro-add env name fun env)
           (string->symbol
            (string-append
             (module-namespace (environment-module env))
             (symbol->string name)
             "|||macro|||")))))
    (cond
     ((symbol? name)
      (fn name env))

     ((identifier? name)
      ;; This code assumes that make-syntactic-closure guarantees that
      ;; identifiers never contain nested syntactic closures and that
      ;; syntactic closures that contain a symbol never has that
      ;; particular symbol in the ids list.
      (fn (synclosure-extract-form name)
          (syntactic-closure-env name)))

     (else
      (error "Name must be an identifier" name)))))

;; Names is a list of symbols or pairs, where the car
;; is the symbol and the cdr is its namespace.
(define (environment-add-defines env names thunk)
  (parameterize
   ((scope-level (scope-level)))
   (let* ((new-env (make-environment env))
          (lists (map (lambda (x)
                        (if (pair? x)
                            (list (car x) (cdr x) new-env)
                            (begin
                              (scope-level (+ (scope-level) 1))
                              (list x (generate-namespace) new-env))))
                      names))
          (reset-funs '()))
     (dynamic-wind
         (lambda ()
           (environment-inner-ns-set!
            new-env
            (filter
             (lambda (n)
               (if (syntactic-closure? (car n))
                   (begin
                     (let* ((sc (car n))
                            (sc-env (syntactic-closure-env sc))
                            (old-ns (environment-inner-ns
                                     sc-env)))
                       (environment-inner-ns-set!
                        sc-env
                        (cons (cons (synclosure-extract-form sc)
                                    (cdr n))
                              old-ns))
                       (set! reset-funs
                             (cons
                              (lambda ()
                                (environment-inner-ns-set!
                                 sc-env
                                 old-ns))
                              reset-funs)))
                     #f)
                   (not (string-contains
                         (symbol->string (car n))
                         #\#))))
             lists)))
         (lambda ()
           (thunk new-env lists))
         (lambda ()
           (for-each (lambda (x) (x))
                     (reverse reset-funs)))))))

(define (make-macro-fun env trans)
  (let* ((lambda-sc (make-syntactic-closure
                     builtin-environment
                     '()
                     'lambda))
         (cons-sc (make-syntactic-closure
                   builtin-environment
                   '()
                   'cons))
         (apply-sc (make-syntactic-closure
                    builtin-environment
                    '()
                    'apply))
         (gs (gensym)))
    (expand-macro
     `(,lambda-sc ,gs
                  (build#expand-macro
                   (,apply-sc ,trans ,gs)
                   ',env))
     env)))

(define (environment-add-define! env name)
  (let* ((top (top-level))
         (ns (if (or (string-contains (symbol->string name)
                                     #\#)
                     top)
                ""
                (generate-namespace))))
    (if top
        (environment-top-ns-add env name ns)
        (environment-inner-ns-set!
         env
         (cons (list name ns env)
               (environment-inner-ns env))))))

;; Macs is a list of lists where car is name, cadr is the sexp
;; of the macro transformer
(define (environment-add-macros env macs rec)
  (let ((new-env (make-environment env)))
    (environment-inner-ns-macro-set!
     new-env
     (map (lambda (m)
            (list (car m)
                  (if rec new-env env)
                  ;; This eval is for creating a procedure object from
                  ;; the macro source code.
                  (parameterize
                   ((calcing #f))
                   (eval-no-hook
                    (make-macro-fun (if rec
                                        new-env
                                        env)
                                    (expand-macro (cadr m) env))))))
          macs))
    new-env))

;; The implementation of this function -forms-to-letrec is a
;; little bit odd. I made them this way because it's important
;; that no form gets macro-expanded more than once, since this
;; makes the entire macro expansion algorithm take exponential
;; time in respect to the nesting level of macros that use this
;; function (like lets, lambdas, syntax-rules macros)
;;
;; Also, the function is written in CPS. It mirrors the
;; environment-add-defines function, which is also CPS, to be
;; able to do the right thing when a synclosure is given as
;; the name in let bindings.
(define (transform-forms-to-triple form env cont)
  (let* ((exprs '())
         (defs '())
         (ieq? (lambda (a b env)
                (identifier=? builtin-environment
                              a
                              env
                              b)))
         (push-exprs-expand
          (lambda (expr rest env)
            (set! exprs
                  (append
                   exprs
                   (cons expr
                         (map (lambda (x)
                                (let ((ret (expand-macro x env)))
                                  (if (and (pair? ret)
                                           (ieq? 'define (car ret) env))
                                      (error "Incorrectly placed define"
                                             `(define ,@(cdr ret))))
                                  (if (and (pair? ret)
                                           (ieq? 'define-syntax (car ret) env))
                                      (error "Incorrectly placed define-syntax"
                                             `(define-syntax ,@(cdr ret))))
                                  ret))
                              rest))))
            (cont exprs defs env)))
         (push-def (lambda (x)
                     (set! defs (cons x defs)))))
    
    (let loop ((form form) (env env))
      (if (null? form)
          (cont exprs defs env)
          (let ((x (expand-macro (car form) env)))
            (cond
             ((not (pair? x))
              (push-exprs-expand x (cdr form) env))
             
             ((ieq? 'define (car x) env)
              (let ((src (transform-to-lambda (cdr x))))
                (push-def src)
                (environment-add-defines
                 env
                 (list (car src))
                 (lambda (new-env _)
                   (loop (cdr form) new-env)))))
             
             ((ieq? 'define-syntax (car x) env)
              (loop (cdr form)
                    (environment-add-macros
                     env
                     (list (cdr x))
                     #t)))
             
             ((or (ieq? 'begin (car x) env)
                  (ieq? '##begin (car x) env))
              (loop (append (cdr x) (cdr form)) env))
             
             (else
              (push-exprs-expand x (cdr form) env))))))))

(define top-level (make-parameter #t))
(define inside-letrec (make-parameter #f))

(define (transform-forms-to-letrec form env)
  (parameterize
   ((top-level #f)
    (inside-letrec #t))
   (transform-forms-to-triple
    form
    env
    (lambda (inner-exp defs new-env)
      (if (null? inner-exp)
          (error "Scope with no expression makes no sense" form))
      
      (if (null? defs)
          inner-exp
          `((##letrec
                ,(map (lambda (src)
                        (expand-macro src new-env))
                      defs)
              ,@inner-exp)))))))

(define (let/letrec-helper rec code env)
  (apply
   (lambda (prefix params . body)
     (let* ((params (extract-syntactic-closure-list params 3))
            ;; If this is not a letrec, do the expansion of the parameter
            ;; initializer here. It has to be done before the call to
            ;; environment-add-defines, otherwise the let will leak if
            ;; it's given syntactic closures as parameter names and/or
            ;; initializers.
            (param-values (map (lambda (x)
                                 (if (not
                                      (and (list? x)
                                           (= 2 (length x))))
                                     (error "Invalid binding" code))
                                 (if rec
                                     (cadr x)
                                     (expand-macro (cadr x) env)))
                               params)))
       (environment-add-defines
        env
        (let ((ps (map (lambda (pair)
                         (if (list? pair)
                             (car pair)
                             (error "Invalid form: " code)))
                       params)))
          (if prefix
              (cons prefix ps)
              ps))
        (lambda (let-env defined-params)
          `(,(if rec
                 '##letrec
                 '##let)
            ,@(if prefix
                  `(,(expand-macro prefix let-env))
                  '())
            ,(map (lambda (p p-val dp)
                    (cons (if (syntactic-closure? (car dp))
                              (expand-synclosure (car dp) env)
                              (gen-symbol (cadr dp) (car dp)))
                          (list
                           (if rec
                               (expand-macro p-val
                                             let-env)
                               p-val))))
                  params
                  param-values
                  (if prefix
                      (cdr defined-params)
                      defined-params))
            ,@(transform-forms-to-letrec body let-env))))))
   ;; Handle let loop
   (if (and (not rec)
            (identifier? (cadr code)))
       (cdr code)
       (cons #f (cdr code)))))

(define (lambda-helper code env)
  (apply
   (lambda (params . body)
     (let ((params (extract-syntactic-closure-list params 3)))
       (environment-add-defines
        env
        (filter (lambda (x) x)
                (let ((key #f))
                  (dotted-map2
                   (lambda (x)
                     (let ((id? (identifier? x)))
                       (cond
                        ((eq? x '#!key)
                         (set! key #t)
                         #f)
                        
                        ((or (eq? x '#!rest)
                             (eq? x '#!optional))
                         (set! key #f)
                         #f)
                        
                        ((or id? (pair? x))
                         (let ((s (if id?
                                      x
                                      (car x))))
                           (if key
                               (cons s "")
                               s)))
                        
                        (else #f))))
                   params)))
        (lambda (lambda-env defined-params)
          (let ((hygparams
                 (let ((key #f)
                       (current-defined-params defined-params))
                   (dotted-map
                    (lambda (p)
                      (cond
                       ((eq? p '#!key)
                        (set! key #t)
                        p)
                       
                       ((or (eq? p '#!rest)
                            (eq? p '#!optional))
                        (set! key #f)
                        p)
                       
                       (else
                        (let* ((dp (let ((dp (car current-defined-params)))
                                     (set! current-defined-params
                                           (cdr current-defined-params))
                                     dp))
                               (ns (cadr dp))
                               (gs-fun (lambda (x)
                                         (if (syntactic-closure? x)
                                             (expand-synclosure x env)
                                             (gen-symbol (if key "" ns)
                                                         x)))))
                          (cond
                           ((identifier? p)
                            (gs-fun p))
                           
                           ((pair? p)
                            (cons (gs-fun (car p))
                                  (expand-macro (cdr p) env)))
                           
                           (else
                            (error "Invalid parameter list: "
                                   params)))))))
                    params))))
            `(##lambda ,hygparams
               ,@(transform-forms-to-letrec body lambda-env)))))))
     (cdr code)))

(define (let/letrec-syntax-helper rec form env)
  (let ((form (expr*:strip-locationinfo form)))
    (apply
     (lambda (bindings . body)
       (if (null? body)
           (error "Form must contain at least one expression" form))
       
       (for-each (lambda (x)
                   (if (not
                        (and (list? x)
                             (= 2 (length x))))
                       (error "Invalid syntax binding" form)))
                 bindings)
       
       (let* ((let-syntax-env (environment-add-macros
                               env
                               bindings
                               rec))
              (res (parameterize
                    ((inside-letrec #t))
                    (map (lambda (x)
                           (expand-macro x let-syntax-env))
                         body))))
         `(##begin ,@res)))
     (cdr form))))

;; Expansion functions

(define (hyg-expand-macro-quasiquote env source #!optional (level 1))
  (let ((code (expr*:value source)))
    (cond
     ((zero? level)
      (expand-macro source env))
     
     ((pair? code)
      (let ((hd (expr*:value (car code))))
        (cond
         ((or (eq? hd 'unquote)
              (eq? hd 'unquote-splicing))
          (expr*:value-set
           source
           (list (car code)
                 (hyg-expand-macro-quasiquote
                  env
                  (cadr code)
                  (- level 1)))))
         
         ((eq? hd 'quasiquote)
          (expr*:value-set
           source
           (list 'quasiquote
                 (hyg-expand-macro-quasiquote
                  env
                  (cadr code)
                  (+ level 1)))))
         
         (else
          (letrec
              ((fn (lambda (x)
                     (hyg-expand-macro-quasiquote env x level)))
               (m
                (lambda (lst)
                  (cond
                   ;; Check for (... unquote (b c)) = (... . ,(b c))
                   ((and (pair? lst)
                         (eq? 'unquote (expr*:value (car lst)))
                         (pair? (expr*:value (cdr lst)))
                         (null? (expr*:value (cddr lst))))
                    (list
                     (cons
                      'unquote-splicing
                      (cdr (fn lst)))))
                   
                   ((pair? lst)
                    (cons (fn (car lst))
                          (m (expr*:value (cdr lst)))))
                   
                   ((null? lst) '())
                   
                   (else (fn lst))))))
            (expr*:value-set
             source
             (m code)))))))
     
     (else (expand-synclosure code env)))))

(define (synclosure-extract-form sc)
  (if (syntactic-closure? sc)
      (synclosure-extract-form (syntactic-closure-form sc))
      sc))

(define (extract-synclosure-crawler source)
  (let ((code (expr*:value source)))
     (cond
      ((pair? code)
       (expr*:value-set
        source
        (dotted-map (lambda (x)
                      (extract-synclosure-crawler x))
                    code)))

      ((syntactic-closure? code)
       (extract-synclosure-crawler
        (expr*:value-set
         source
         (syntactic-closure-form code))))
      
      (else
       source))))

;; I ran into trouble when implementing forms like let and lambda
;; which had to interpret their contents. An example should make this
;; clear:
;;  
;; (expand-macro
;;  (capture-syntactic-environment
;;   (lambda (env)
;;     `(let ,(make-syntactic-closure env '() '((a 5)))
;;        #f))))
;;  
;; This is valid code, but the first argument to the let form is not
;; a list, so it can't just look through it as usual. This is a problem,
;; but it has a rather simple solution, the extract-syntactic-closure-list
;; function.
;;  
;; It's based on the fact that lists themselves don't have or use syntactic
;; environments. If [x] is a syntactic closure with a particular environment
;; and free-names list with the form x, then [(a b)] is equal to ([a] [b]).
;;  
;; The extract-syntactic-closure-list function extracts a syntactic closure
;; with a list to the second form.
;;  
;; * fn([()]) = ()
;; * fn([(a . b)]) => ([a] . fn([b]))
;; * fn(x) => x
;;  
;; This function should be used whenever a sc-transformer macro needs to
;; parse its argument as a list. In the example above, let would use the
;; function twice; first on [((a 5))], which would return ([(a 5)]). Since
;; it needs to parse the inner list as well, it calls the function on that
;; too: [(a 5)] => ([a] [5]).
;;  
;; Because this function returns its argument if it's not a syntactic closure
;; or a syntactic closure not containing a list, it can be used on all
;; parameters that might be a syntactic closures that should be lists.
;;
;; Two important features that are not documented here (because I added
;; them after I wrote this) is levels and the fact that this function
;; suports nested syntactic closures as input.
;;
;; I also suspect that this function is really slow in respect to how
;; many nested syntax closures there are, because the expand function
;; in expand-sc isn't propagated to the top function. But i dunno.
(define (extract-syntactic-closure-list sc #!optional (levels 1))
  (letrec ((expand-sc
            (lambda (sc expand)
              (let ((form (syntactic-closure-form sc))
                    (env (syntactic-closure-env sc))
                    (ids (syntactic-closure-ids sc)))
                (cond
                 ((null? form)
                  '())
                 
                 ((pair? form)
                  (cons (extract-syntactic-closure-list
                         (expand
                          (make-syntactic-closure env ids (car form)))
                         (- levels 1))
                        (extract-syntactic-closure-list
                         (expand
                          (make-syntactic-closure env ids (cdr form)))
                         levels)))
                 
                 ((syntactic-closure? form)
                  (expand-sc form (lambda (x)
                                    (make-syntactic-closure
                                     env
                                     ids
                                     (expand x)))))
                 
                 (else
                  sc))))))
  (cond
   ((= 0 levels)
    sc)

   ((syntactic-closure? sc)
    (expand-sc sc (lambda (x) x)))

   ((pair? sc)
    (cons (extract-syntactic-closure-list
           (car sc)
           (- levels 1))
          (extract-syntactic-closure-list
           (cdr sc)
           levels)))

   (else
    sc))))

;; This function simply removes syntactic closures, without
;; taking the syntactic environments into account.
(define (extract-syntactic-closure sc)
  (let ((form (if (syntactic-closure? sc)
                  (syntactic-closure-form sc)
                  sc)))
    (cond
     ((null? form)
      '())
     
     ((pair? form)
      (cons (extract-syntactic-closure (car form))
            (extract-syntactic-closure (cdr form))))

     ((syntactic-closure? form)
      (extract-syntactic-closure form))

     (else
      form))))

(define (expand-synclosure source env)
  (let ((code (expr*:value source)))
    (cond
     ((syntactic-closure? code)
      (let* ((senv (syntactic-closure-env code))
             (ids (syntactic-closure-ids code))
             (new-env
              (make-environment
                senv
                (filter
                 (lambda (x) x)
                 (map (lambda (x)
                        (let ((val (or (environment-inner-ns-get env x)
                                       (environment-top-ns-get env x))))
                          (and val
                               (cons x val))))
                      ids))
                (filter
                 (lambda (x) #f)
                 (map (lambda (x)
                        (or (environment-inner-ns-macro-get env x)
                            (let ((val (environment-top-ns-macro-get env x)))
                              (and val (cons x val)))))
                      ids)))))
        (expand-macro (syntactic-closure-form code)
                      new-env)))
     
     ((syntactic-capture? code)
      (expand-macro
       ((syntactic-capture-proc code) env)
       env))
     
     (else source))))

(define (expand-macro source
                      #!optional
                      (env (top-environment)))
  (let ((code (expr*:value source)))
    (cond
     ((pair? code)
      ;; Expand the head if it is a synclosure; that closure might in
      ;; turn expand into a macro name.
      (let* ((hd-val (expr*:value (car code)))
             (hd (synclosure-extract-form hd-val))
             (search-env (if (syntactic-closure? hd-val)
                             (syntactic-closure-env hd-val)
                             env))
             (default-action
               (lambda ()
                 (parameterize
                  ((inside-letrec #f))
                  (expr*:dotted-map (lambda (x)
                                      (expand-macro x env))
                                    source)))))
        (if (symbol? hd) ;; This is equivalent to (identifier? hd-val)
            (cond
             ((environment-inner-ns-search
               search-env hd
               (lambda (val)
                 ;; I wrap the return value in a list, because macros might
                 ;; expand into #f, and that would result in the macro not
                 ;; being expanded if we didn't wrap it in something.
                 (list
                  (cons (expr*:value-set (car code)
                                         (gen-symbol (car val) hd))
                        (dotted-map (lambda (x)
                                      (expand-macro x env))
                                    (cdr code)))))
               (lambda (macro)
                 (parameterize
                  ((inside-letrec #f))
                  (list
                   ((cadr macro)
                    source
                    env
                    (car macro)))))) =>
                    (lambda (ret) (car ret)))
             
             ((and
               (symbol? hd)
               (let ((symstr (symbol->string hd)))
                 (cond
                  ;; This entire clause is a temporary hack that I do
                  ;; before I actually implement relative module names.
                  ;;
                  ;; This code doesn't work right now.
                  ((and #f
                        (not (string-begins-with symstr "#"))
                        (string-contains symstr #\#)) =>
                        (lambda (idx)
                          (let* ((sym (string->symbol
                                       (substring symstr
                                                  (+ idx 1)
                                                  (string-length symstr))))
                                 (mod-sym (string->symbol
                                           (substring symstr 0 idx)))
                                 (env (with-exception-catcher
                                       (lambda (e) #f)
                                       (lambda ()
                                         (module-info-environment
                                          (module-info mod-sym))))))
                            (and env
                                 (environment-top-ns-macro-get env sym)))))
                  
                  (else
                   (or (environment-top-ns-macro-get search-env hd)
                       (and (calcing)
                            (environment-top-ns-macro-get load-environment hd))
                       (environment-top-ns-macro-get builtin-environment hd)))))) =>
                       (lambda (mac)
                         (parameterize
                          ((inside-letrec (and (inside-letrec) (eq? hd 'begin))))
                          ((car mac) ;; Macro function
                           source
                           env ;; TODO I'm not sure if this should be search-env
                           ;; Macro namespace
                           (cadr mac)))))
             
             ((memq hd '(##let
                            ##let*
                          ##letrec
                          ##lambda
                          ##define
                          ##namespace
                          ##case
                          ##define-macro))
              (expr*:value-set code
                               (cons (expr*:value-set (car code)
                                                      hd)
                                     (cdr code))))
             
             (else
              (default-action)))
            
            (default-action))))
     
     ((or (syntactic-closure? code)
          (syntactic-capture? code))
      (expand-synclosure source env))
     
     ((symbol? code)
      (cond
       ((environment-inner-ns-get env code) =>
        (lambda (def)
          (expr*:value-set source
                           (gen-symbol (car def) code))))
       
       ((string-contains (symbol->string code) #\#)
        source)

       ((or (environment-top-ns-macro-get env code)
            (environment-top-ns-macro-get builtin-environment code))
        (error "Macro name can't be used as a variable:" code))
       
       (else
        (expr*:value-set
         source
         (gen-symbol (or (environment-top-ns-get env code)
                         (environment-top-ns-get builtin-environment
                                                 code)
                         "")
                     code)))))
     
     (else source))))

;; Identifiers

(define (identifier? id)
  (or (symbol? id)
      (and (syntactic-closure? id)
           ;; One might think that the next line has to be identifier?
           ;; and not just symbol?, to create a recursive test, but
           ;; optimizations that are done by make-syntactic-closure
           ;; that guarantee that identifiers never are multiple
           ;; nested syntactic closures makes that redundant.
           (symbol?
            (syntactic-closure-form id)))))

(define (identifier=? env1 id1 env2 id2)
  (define (strip-synclosure env id)
    (cond
     ((not (syntactic-closure? id))
      (cons id env))

     ((memq (syntactic-closure-form id)
            (syntactic-closure-ids id))
      (cons (syntactic-closure-form id)
            env))

     (else
      (strip-synclosure
       (syntactic-closure-env id)
       (syntactic-closure-form id)))))
  
  ;; First, strip the identifiers to symbols then, check if the
  ;; symbols are equal. If they are not, then this is not the same
  ;; identifier.
  ;;
  ;; However, even if the symbols are the same, they could be
  ;; different identifiers. This can only happen if the symbols are of
  ;; the form h[number]#name. If it is an absolute namespace like
  ;; build#, then the symbols must be the same.  This is an example:
  ;;
  ;; (let ()
  ;;   (let (x) x)
  ;;   (let (x) x))
  ;;
  ;; Both x variables would expand into something like h1#x, but they
  ;; are still not the same identifier.
  ;;
  ;; The way we check this is that every environment stores stuff for
  ;; every identifier it contains.  One of the things that are stored
  ;; is the "origin environment," that is, the scope where the
  ;; identifier was introduced. If the two identifiers' environments
  ;; are the same, then they are equal.
  (let* ((strip1 (strip-synclosure env1 id1))
         (id1 (car strip1))
         (env1 (cdr strip1))
         (one (environment-inner-ns-get env1 id1))
         (one-pair ;; Pair of (symbol . environment)
          (if one
              (cons (gen-symbol (car one) id1)
                    (cadr one))
              (let ((top (environment-top-ns-get env1 id1)))
                (if top
                    (cons (gen-symbol top id1)
                          #f)
                    (cons id1 #f)))))
         (strip2 (strip-synclosure env2 id2))
         (id2 (car strip2))
         (env2 (cdr strip2))
         (two (environment-inner-ns-get env2 id2))
         (two-pair ;; Pair of (symbol . environment)
          (if two
              (cons (gen-symbol (car two) id2)
                    (cadr two))
              (let ((top (environment-top-ns-get env2 id2)))
                (if top
                    (cons (gen-symbol top id2)
                          #f)
                    (cons id2 #f))))))
    (and
     ;; Compare symbols
     (eq? (car one-pair) (car two-pair))
     ;; Compare environments
     (eq? (cdr one-pair) (cdr two-pair)))))

;; Tools for defining macros

(define (sc-macro-transformer thunk)
  (lambda (form env mac-env)
    (expand-macro (thunk (expr*:strip-locationinfo form) env)
                  mac-env)))

(define (rsc-macro-transformer thunk)
  (lambda (form env mac-env)
    (expand-macro (thunk (expr*:strip-locationinfo form) mac-env)
                  env)))

(define (nh-macro-transformer thunk)
  (rsc-macro-transformer
   (lambda (form env)
     (apply thunk (cdr form)))))


;; Core macros

(##define-macro (define-env name prefix names macs)
  (let ((macro-names
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
       (set! ,name
             (make-environment
               #f
               '()
               '()
               (list->table
                ',(map (lambda (name)
                         (cons name prefix))
                       names))
               (list->table
                (list
                 ,@(map (lambda (mac mac-name)
                          (list 'list
                                `',(car mac)
                                mac-name
                                name))
                        macs macro-names))))))))

(define-env builtin-environment
  "build#"
  (expand-macro
   make-syntactic-closure
   capture-syntactic-environment
   extract-syntactic-closure-list
   identifier?
   identifier=?
   sc-macro-transformer
   rsc-macro-transformer
   nh-macro-transformer)
  ((use
    (nh-macro-transformer
     (lambda pkgs
       (apply module-use pkgs))))
   
   (module
    (nh-macro-transformer
     (lambda (name)
       (module-module name))))

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
               (expr*:cadr code)))))))
   
   (define
     (lambda (code env mac-env)
       (let ((src (expr*:transform-to-lambda (expr*:cdr code))))
         (if (top-level)
             (begin
               ;; If it's a top level define, it's required to make
               ;; defines shadow imported defines. (For instance,
               ;; lib/string imports lib/srfi13, defines string-map
               ;; and uses string-map.  Then lib/string's string-map
               ;; should be used.
               (let* ((name-form (car (expr*:value src)))
                      (def-env (if (syntactic-closure? name-form)
                                   (syntactic-closure-env name-form)
                                   env))
                      (name (expand-synclosure name-form env)))
                 (environment-add-define! def-env (expr*:value name))
                 (expr*:value-set
                  code
                  `(##begin
                     (##namespace (,(module-namespace
                                     (environment-module def-env))
                                   ,name))
                     (##define ,name
                       ,(expand-macro (cadr (expr*:value src)) env))))))
             
             ;; This is to make transform-to-letrec work. It needs that
             ;; defines are not expanded.
             `(,(make-syntactic-closure builtin-environment '() 'define)
               ,(car src)
               ,(cadr src))))))

   (define-syntax
     (lambda (source env mac-env)
       (let ((code (expr*:value source)))
         (if (not (= 3 (length code)))
             (error "Invalid define-syntax form (wrong number of arguments)"
                    (expr*:strip-locationinfo source)))
         
         (let* ((name (cadr code))
                (trans (caddr code))
                (before-name (expr*:value name)))
           (if (top-level)
               (let* ((fun (parameterize
                            ((calcing #f))
                            (expand-macro trans env)))
                      (fn-name (environment-add-macro-fun
                                before-name
                                (eval-no-hook fun)
                                env)))
                 (expand-macro
                  (if (calcing)
                      `(define-macro-register ,name ,trans)
                      `(##define ,fn-name
                         ,fun))
                  empty-environment))
               source)))))
   
   (define-macro-register
     (lambda (form env mac-env)
       (void)))

   (begin
     (lambda (code env mac-env)
       ;; This is to make transform-to-letrec work. It needs that
       ;; begins don't get expanded.
       (if (inside-letrec)
           code
           (expr*:value-set
            code
            ;; TODO This doesn't generate source code locations correctly
            `(##begin ,@(expr*:map
                         (lambda (x)
                           (expand-macro x env))
                         (cdr (expr*:value code))))))))
   
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
         (let/letrec-syntax-helper #f code env)))

   (letrec-syntax
       (lambda (code env mac-env)
         (let/letrec-syntax-helper #t code env)))
   
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
              ;; (##let () ...)s while still allowing (let* () ...)
              (let ((params (extract-syntactic-closure-list params 2)))
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
                 env)))
            (cdr code)))))

   (syntax-rules
       (lambda (code env mac-env)
         `(build#sc-macro-transformer
           (apply build#syntax-rules-proc
                  ',(expr*:cdr code)))))

   (define-macro
     (lambda (code env mac-env)
       ;; TODO This doesn't generate source code locations correctly
       (let ((code (expr*:strip-locationinfo code)))
         (let* ((src (transform-to-lambda (cdr code))))
           (expand-macro
            `(,(make-syntactic-closure
                builtin-environment
                '()
                'define-syntax)
              ,(car src)
              (build#nh-macro-transformer ,(cadr src)))
            env)))))

   (case
       (lambda (code env mac-env)
         ;; TODO This doesn't generate source code locations correctly
         (let ((code (expr*:strip-locationinfo code)))
           `(##case ,(expand-macro (cadr code) env)
              ,@(map (lambda (x)
                       `(,(car x)
                         ,@(map (lambda (f)
                                  (expand-macro f env))
                                (cdr x))))
                     (cddr code))))))
   
   (receive
    (lambda (code env mac-env)
      ;; TODO This doesn't generate source code locations correctly
      (let ((code (expr*:strip-locationinfo code)))
        (apply
         (lambda (formals expression . body)
           (let ((lmb (make-syntactic-closure env '() 'lambda)))
             (expand-macro
              `(,(make-syntactic-closure env '() 'call-with-values)
                (,lmb () ,expression)
                (,lmb ,formals
                      ,@body))
              env)))
         (cdr code)))))
   
   (define-once
     ;; TODO This doesn't quite work yet.
     ;; Idea stolen from Christian Jaeger
     (lambda (code env mac-env)
       ;; TODO This doesn't generate source code locations correctly
       (let ((code (expr*:strip-locationinfo code)))
         (let ((src (transform-to-lambda code)))
           (expand-macro
            `(,(make-syntactic-closure builtin-environment
                                       '()
                                       'define)
              ,(car src)
              (,(make-syntactic-closure builtin-environment
                                        '()
                                        'if)
               (build#defined? ',(car src))
               ',(car src)
               ,(cadr src)))
            env)))))
   
   (private
    (lambda (code env mac-env)
      (void)))
   
   (/private
    (lambda (code env mac-env)
      (void)))
   
   (compile-options
    (nh-macro-transformer
     (lambda (#!key options cc-options ld-options-prelude ld-options force-compile)
       (void))))

   (define-type
     (nh-macro-transformer
      (lambda args
        (expand 'define-type #f #f args))))))
