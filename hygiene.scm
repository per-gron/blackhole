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

;; Environments are rather odd data structures. They are designed the
;; way they are because they need to embody a couple of rather strange
;; concepts:
;;
;; One thing is that every top level environment has a conceptually
;; infinite number of "phases". Phases are numbered, and the first one
;; (there is a first one) is number 0. It is always possible to find
;; the next phase, but never possible to get to the previous one. This
;; is the practical implementation of the concept that there is a
;; potentially infinite number of macro expansion phases, each with a
;; unique set of availible identifiers.
;;
;; In practise this is implemented by letting top level environments'
;; parent field be the phase number, and having a lazily initialized
;; next-phase field that contains the next phase environment. It is
;; intentionally impossible to find the next phase of a non-top-level
;; environment. To do that, first go up to the top level, then find
;; the next phase.
;;
;; So far I've been talking about top-level and non-top-level
;; environments without defining them. This refers to lexical
;; scoping. For each scope, a new environment is created whose parent
;; is the environment that is in the scope above.
;;
;; The presence of syntactic closures further increases the complexity
;; of this data structure, because it must be possible to create
;; environments that are children of other environments in the sense
;; that they contain more identifiers than their parent, but also
;; don't introduce a new scope, that is, adding new identifiers to
;; these environments will add them also to their parents.
;;
;; The ns and ns-mutate fields are the fields that contain the hygiene
;; information. They contain the same kind of tree-like data
;; structure, and ns-mutate must be eq? to ns or a child node of
;; it. ns-mutate refers to the scope of the environment where new
;; things should be added. ns might be a parent of ns-mutate to be
;; able to express syntactic closures that add identifiers, without
;; introducing scope.
;;
;; The actual data structure looks like this. It is one of:
;; * A table. Lookups and modifications are done in the obvious way.
;;   An important thing to note here is that tables are used for
;;   top-level bindings, while alists are used for lexical
;;   bindings. identifier=? relies on this to be able to do the right
;;   thing.
;; * A box of an alist. Lookups and modifications are done in the
;;   obvious way.
;; * A pair of two datastructures of the this type. Lookups are done
;;   first in the car, and if nothing was found, search in the
;;   cdr. Modifications are done in the car.
;; * A procedure returning a datastructure. Lookups are done on the
;;   return value, modification is an error.
;;
;; The key is the name of the identifier, and the value is a list that
;; can either be ('mac [macro function] [macro environment]) or ('def
;; [symbol to be expanded to] [environment where it was introduced])
;;
;; (I don't think the environment in the 'def kind of list is needed,
;; it's an artifact of an earlier implementation of identifier=?)
(define-type env
  id: A8981FB8-BC38-47BA-8707-5A3F5962D610
  ;; The parent environment. This is how lexical scope is implemented;
  ;; a number means that it is a top-level environment, an environment
  ;; means that the scope above this one is that environment.
  ;;
  ;; If the environment is top-level, this should be a number, which
  ;; is the phase of the environment.
  (parent unprintable: read-only:)
  ;; The module of this environment. #f if REPL.
  (module unprintable: read-only:)
  ;; The next phase environment. This is part of the implementation of
  ;; proper separation between run-time and compile-time. When an
  ;; environment is created, this is set to #f, and is then lazily
  ;; initialized to an environment object when it is asked for.
  (next-phase unprintable:)
  
  ;; These are unprintable because it often contains cyclic data
  ;; structures, which in practise crashes gambit when prettyprinting
  ;; it, which is a huge pain.
  (ns unprintable: read-only:)
  (ns-mutate unprintable: read-only:))

(define (make-environment parent
                          #!optional
                          (ns (cons (box '())
                                    (env-ns parent)))
                          (ns-mutate ns))
  (make-env (if (env? parent)
                parent
                0)
            (if (env? parent)
                (environment-module parent)
                parent)
            #f
            ns
            ns-mutate))

(define (environment-find-top env)
  (let ((parent (environment-parent env)))
    (if (environment? parent)
        (environment-find-top parent)
        env)))

(define (environment-phase env)
  (env-parent (environment-find-top env)))

(define (environment-top? env)
  (not (env? (env-parent env))))

(define (environment-next-phase env)
  (or (env-next-phase env)
      (if (environment-top? env)
          (let* ((ns (cons (make-table)
                           builtin-ns-phase))
                 (val (make-env (+ 1 (env-parent env))
                                (env-module env)
                                #f
                                ns
                                ns)))
            (env-next-phase-set! env val)
            val)
          ;; Non-top-level environment.
          (make-environment
            (environment-next-phase
             (environment-find-top env))))))

(define environment? env?)

(define environment-parent env-parent)

(define environment-module env-module)

(define (ns-get ns name #!key ignore-globals)
  (cond
   ((box? ns)
    (let ((p (assq name (unbox ns))))
      (and p (cdr p))))
   
   ((pair? ns)
    (or (ns-get (car ns) name ignore-globals: ignore-globals)
        (ns-get (cdr ns) name ignore-globals: ignore-globals)))
   
   ((table? ns)
    (and (not ignore-globals)
         (table-ref ns name #f)))

   ((procedure? ns)
    (ns-get (ns) name ignore-globals: ignore-globals))
   
   (else
    (error "Invalid ns" ns))))

(define (environment-get env name #!key ignore-globals)
  (ns-get (env-ns env) name ignore-globals: ignore-globals))

(define (environment-is-scope? env)
  (eq? (env-ns env) (env-ns-mutate env)))

(define (environment-find-scope env)
  (cond
   ((not env)
    (error "environment-find-scope internal error"))
   
   ((environment-is-scope? env)
    env)

   (else
    (environment-find-scope (env-parent env)))))

(define (ns-add! ns name val)
  (cond
   ((table? ns)
    (table-set! ns name val))

   ((box? ns)
    (set-box! ns
              (cons (cons name val)
                    (unbox ns))))

   ((pair? ns)
    (ns-add! (car ns) name val))

   ((procedure? ns)
    (error "Cannot modify procedure ns" ns))

   (else
    (error "Invalid ns" ns))))

(define (ns-add/reset! ns name val)
  (cond
   ((table? ns)
    (let ((prev-val (table-ref ns name #f)))
      (table-set! ns name val)
      (if prev-val
          (lambda () (table-set! ns name prev-val))
          (lambda () (table-set! ns name)))))

   ((box? ns)
    (let ((prev-val (unbox ns)))
      (set-box! ns
                (cons (cons name val)
                      prev-val))
      (lambda ()
        (set-box! ns prev-val))))

   ((pair? ns)
    (ns-add/reset! (car ns) name val))

   ((procedure? ns)
    (error "Cannot modify procedure ns" ns))

   (else
    (error "Invalid ns" ns))))

(define (environment-add-def! env exported-name actual-name)
  (ns-add! (env-ns-mutate env)
           exported-name
           (list 'def
                 actual-name
                 (environment-find-scope env))))

(define (environment-add-mac! env exported-name fun m-env)
  (ns-add! (env-ns-mutate env)
           exported-name
           (list 'mac
                 fun
                 m-env)))


(define-type syntactic-closure
  id: 0B9FFE46-B995-48C2-B5E5-B66FD3FD335C
  constructor: make-syntactic-closure-internal
  
  (env read-only:)
  (ids read-only:)
  (form read-only:))

(define (make-syntactic-closure env ids form)
  (cond
   ;; Forms that are not symbols, pairs, vectors or syntactic closures
   ;; (for instance null, numbers or strings) don't need to be wrapped
   ;; in a syntactic closure, because their meaning is independent of
   ;; lexical context.
   ((not (or (symbol? form)
             (pair? form)
             (vector? form)
             (syntactic-closure? form)))
    form)
   
   ;; If form already is a fully closed syntactic closure, there is no
   ;; need to wrap it again.
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

(define scope-level (make-parameter 0))

(define (generate-namespace)
  (string-append
   (number->string (scope-level))
   "#"))

(define (environment-namespace env)
  (let ((phase (environment-phase env))
        (ns (module-namespace
             (environment-module env))))
    (if (zero? phase)
        ns
        (string-append
         (substring ns 0 (max 0 (- (string-length ns) 1)))
         "~"
         (if (> phase 1)
             (number->string phase)
             "")
         "#"))))

(define (eval-in-next-phase code env)
  (let ((np (environment-next-phase env)))
    (parameterize
     ((top-environment np))
     (eval-no-hook
      (expand-macro code
                    np)))))

(define (environment-add-macro-fun name fun env)
  (let ((fn
         (lambda (name env)
           (environment-add-mac!
            env
            name ;; The exported name
            (eval-in-next-phase fun env)
            env))))
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

(define (environment-add-define! env name)
  (let* ((top (top-level))
         (ns (cond
              ((string-contains (symbol->string name)
                                #\#)
               "")
              
              (top
               (environment-namespace env))

              (else
               (generate-namespace)))))
    (environment-add-def! env
                          name
                          (gen-symbol ns name))
    ns))

;; Helper function for environment-add-defines and
;; environment-add-macros. It takes:
;;
;; * an environment
;; * a list of pairs, where car is an identifier that is to be bound,
;;   and cdr is an arbitrary object, to be used by the list->val
;;   function
;; * a function that takes a pair from the list mentioned above and
;;   returns the object that should be passed to ns-add!.
;; * a thunk that takes the "return value" of this function.
;;
;; This function is in CPS to be able to undo the side effects that it
;; might cause, after thunk has returned.
;;
;; The problem that it addresses is that in any let and its flavours,
;; there might be a syntactic closure as the name in the binding
;; list. This makes things rather complicated (I might have missed the
;; easy solution to this)
(define (environment-add-to-ns env lists list->val thunk)
  (let ((reset-funs '()))
    (dynamic-wind
        (lambda ()
          (for-each
           (lambda (n)
             (let* ((pair
                     (if (syntactic-closure? (car n))
                         (let* ((sc (car n))
                                (sc-env (syntactic-closure-env sc))
                                (name (syntactic-closure-form sc)))
                           (cons (cons name
                                       (cdr n))
                                 sc-env))
                         (cons n env)))
                    (list->val-ret
                     (list->val (car pair)))
                    (sc-env (cdr pair)))
               (set! reset-funs
                     (cons
                      (ns-add/reset! (env-ns-mutate sc-env)
                                     (car list->val-ret)
                                     (cdr list->val-ret))
                      reset-funs))))
            lists))
        (lambda ()
          (thunk env lists))
        (lambda ()
          (for-each (lambda (x) (x))
                    (reverse reset-funs))
          (set! reset-funs '())))))

;; Names is a list of symbols or pairs, where the car
;; is the symbol and the cdr is its namespace.
(define (environment-add-defines env names thunk #!optional mutate)
  (let ((new-env
         (if mutate
             (make-environment env
                               (cons (box '())
                                     (env-ns env))
                               (env-ns-mutate env))
             (make-environment env))))
    (parameterize
     ((scope-level (scope-level)))
     (environment-add-to-ns
      new-env
      (map (lambda (n)
             (if (pair? n)
                 (list (car n) (cdr n) new-env)
                 (begin
                   (scope-level (+ (scope-level) 1))
                   (list n (generate-namespace) new-env))))
           names)
      (lambda (n)
        (list (car n)
              'def
              (gen-symbol (cadr n) (car n))
              (caddr n)))
      thunk))))

;; Macs is a list of lists where car is name, cadr is the sexp
;; of the macro transformer
(define (environment-add-macros env macs rec thunk)
  (let ((new-env (if rec
                     env
                     (make-environment env))))
    (environment-add-to-ns
     new-env
     macs
     (lambda (m)
       (list (car m)
             'mac
             ;; This eval is for creating a procedure object from
             ;; the macro source code.
             (eval-in-next-phase (cadr m) env)
             env))
     thunk)))

;; (inside-letrec) implies (not (top-level))
(define top-level (make-parameter #t))
(define inside-letrec (make-parameter #f))

;; The implementation of this function -forms-to-letrec is a
;; little bit odd. I made them this way because it's important
;; that no form gets macro-expanded more than once, since this
;; makes the entire macro expansion algorithm take exponential
;; time in respect to the nesting level of macros that use this
;; function (like lets, lambdas, syntax-rules macros)
;;
;; Also, the function is written in CPS. It mirrors the
;; environment-add-defines and environment-add-macros functions, which
;; are also CPS, to be able to do the right thing when a synclosure is
;; given as the name in let bindings or macro names.
(define (transform-forms-to-triple form parent-env cont)
  (let* ((exprs '())
         (defs '())
         (env (make-environment parent-env))
         (ieq? (lambda (a b env)
                 (identifier=? builtin-environment
                               a
                               env
                               b)))
         (push-exprs-expand
          (lambda (expr rest env)
            (parameterize
             ((inside-letrec #f))
             (set! exprs
                   (append
                    exprs
                    (cons expr
                          (map (lambda (x)
                                 (expand-macro x env))
                               rest))))
             (cont exprs defs env))))
         (push-def (lambda (x)
                     (set! defs (cons x defs)))))
    
    (parameterize
     ((top-level #f)
      (inside-letrec #t))
     ;; Bah... it's really not good to have to resort to continuations
     ;; here. It is used to be able to implement the interaction
     ;; between let-syntax, begin and this function properly; because
     ;; of how environment-add-macros work, let/letrec-syntax-helper
     ;; can't just return a syntactic-closure with its contents,
     ;; because environment-add-macros will remove the macros from
     ;; that environment when it returns.
     (call/cc
      (lambda (ret)
        (let loop ((form form) (env env))
          (inside-letrec
           (lambda (forms)
             (ret
              (loop (append forms (cdr form))
                    env))))
          
          (if (null? form)
              (cont exprs defs env)
              (let ((x (expand-macro (car form) env)))
                (cond
                 ((eq? #!void x)
                  (loop (cdr form) env))
                 
                 ((not (pair? x))
                  (push-exprs-expand x (cdr form) env))
                 
                 ((ieq? 'define (car x) env)
                  (let ((src (transform-to-lambda (cdr x))))
                    (push-def src)
                    (environment-add-defines
                     env
                     (list (car src))
                     (lambda (new-env _)
                       (loop (cdr form) new-env))
                     #t)))
                 
                 ((ieq? 'define-syntax (car x) env)
                  (environment-add-macros
                   env
                   (list (cdr x))
                   'side-effect
                   (lambda (new-env _)
                     (loop (cdr form) new-env))))
                 
                 ((or (ieq? 'begin (car x) env)
                      (ieq? '##begin (car x) env))
                  (loop (append (cdr x) (cdr form)) env))
                 
                 (else
                  (push-exprs-expand x (cdr form) env)))))))))))

(define (transform-forms-to-letrec form env)
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
             ,@inner-exp))))))

(define (let/letrec-helper rec code env)
  (parameterize
   ((inside-letrec #f))
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
        (cons #f (cdr code))))))

(define (lambda-helper code env)
  (parameterize
   ((inside-letrec #f))
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
    (cdr code))))

(define (let/letrec-syntax-helper rec form env thunk)
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

       (environment-add-macros
        env
        bindings
        rec
        (lambda (let-syntax-env _)
          (let ((inner-env
                 (make-environment let-syntax-env
                                   (env-ns let-syntax-env)
                                   (env-ns-mutate env))))
            (thunk body
                   inner-env)))))
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
           (list (car code)
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
     
     (else
      (expand-synclosure
       code
       env
       (lambda (code env)
         (hyg-expand-macro-quasiquote env
                                      code
                                      level)))))))

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

(define (expand-synclosure source env #!optional expand-function)
  (let ((code (expr*:value source)))
    (cond
     ((syntactic-closure? code)
      (let* ((senv (syntactic-closure-env code))
             (ns (env-ns senv))
             (ids (syntactic-closure-ids code))
             (new-env
              (make-environment
                senv
                (cons
                 (box
                  (filter
                   (lambda (x) x)
                   (map (lambda (x)
                          (cons x (ns-get ns x)))
                        ids)))
                 ns)
                (env-ns-mutate senv))))
        ((or expand-function expand-macro)
         (syntactic-closure-form code)
         new-env)))
     
     ((syntactic-capture? code)
      ((or expand-function expand-macro)
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
                  ((inside-letrec #f)
                   (top-level (if (eq? '##begin hd)
                                  (top-level)
                                  #f)))
                  (expr*:dotted-map (lambda (x)
                                      (expand-macro x env))
                                    source)))))
        (if (symbol? hd) ;; This is equivalent to (identifier? hd-val)
            (cond
             ((environment-get search-env hd) =>
              (lambda (val)
                (if (eq? 'def (car val))
                    (parameterize
                     ((inside-letrec #f))
                     (cons (expr*:value-set (car code)
                                            (cadr val))
                           (dotted-map (lambda (x)
                                         (expand-macro x env))
                                       (cdr code))))
                    ((cadr val)
                     source
                     env
                     (caddr val)))))

             ;; It's tempting to add ##begin to this list, it would
             ;; after all make ##begin a useful construct for
             ;; bypassing the macro expansion anywhere you want, but
             ;; it's not possible to do that (at least without other
             ;; modifications also), because the forms that are passed
             ;; to the expand-source callbacks contain ##begin as a
             ;; top-level container, so adding ##begin to this list
             ;; effectively disables macro expansion while compiling
             ;; and loading files.
             ((memq hd '(##namespace
                         ##let
                         ##let*
                         ##letrec
                         ##lambda
                         ##define
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
       ((string-contains (symbol->string code) #\#)
        source)

       ((environment-get env code) =>
        (lambda (val)
          (if (eq? 'def (car val))
              (expr*:value-set source (cadr val))
              (error "Macro name can't be used as a variable:" code))))
       
       (else
        source)))
     
     ((or (number? code)
          (string? code)
          (char? code)
          (boolean? code)
          (eq? code #!void)
          (eq? code #!unbound)
          (eq? code #!eof)
          (eq? code #!optional)
          (eq? code #!rest)
          (eq? code #!key)
          (keyword? code))
      source)

     ;; All other things (e.g. null, vectors, boxes, procedures,
     ;; abritrary objects) are invalid expressions.
     (else
      (error "Ill-formed expression" code)))))

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

     ((not (symbol? (syntactic-closure-form id)))
      (error "Supplied non-identifier argument to identifier=?" id))

     (else
      (cons (syntactic-closure-form id)
            (syntactic-closure-env id)))))
  
  ;; First, strip the identifiers to symbols then, check if the
  ;; symbols are equal. If they are not, then this is not the same
  ;; identifier.
  ;;
  ;; However, even if the symbols are the same, they could be
  ;; different identifiers. This can only happen if the symbols are of
  ;; the form h[number]#name. If it is an absolute namespace like
  ;; module#, then the symbols must be the same.  This is an example:
  ;;
  ;; (let ()
  ;;   (let (x) x)
  ;;   (let (x) x))
  ;;
  ;; Both x variables would expand into something like 1#x, but they
  ;; are still not the same identifier.
  (let* ((strip1 (strip-synclosure env1 id1))
         (id1 (car strip1))
         (env1 (cdr strip1))
         (one (environment-get env1
                               id1
                               ;; TODO I'm not sure this is the
                               ;; correct place to have this. It is
                               ;; here because syntax-rules needs it
                               ;; for the literals part to work.
                               ignore-globals: #t))
         
         (strip2 (strip-synclosure env2 id2))
         (id2 (car strip2))
         (env2 (cdr strip2))
         (two (environment-get env2
                               id2
                               ignore-globals: #t)))
    (or (and (not one) (not two) (eq? id1 id2))
        (and one two (eq? one two)))))

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
                               `',(car mac)
                               ''mac
                               mac-name
                               name))
                       macs macro-names)))))
       (set! ,name
             (make-environment #f
                               -ns-
                               -ns-))))))

(define-env inside-letrec-environment
  "module#inside-letrec-env-"
  ((define
     (lambda (code env mac-env)
       (let ((src (expr*:transform-to-lambda (expr*:cdr code))))
         `(,(make-syntactic-closure
             builtin-environment '() 'define)
           ,(make-syntactic-closure env '() (car src))
           ,(make-syntactic-closure env '() (cadr src))))))

   (define-syntax
     (lambda (source env mac-env)
       (let ((code (expr*:value source)))
         (expr*:value-set
          source
          (list (make-syntactic-closure
                 builtin-environment '() 'define-syntax)
                (make-syntactic-closure env '() (cadr code))
                (make-syntactic-closure env '() (caddr code)))))))
   
   (let-syntax
       (lambda (code env mac-env)
         (let/letrec-syntax-helper
          #f
          code
          env
          (lambda (body inner-env)
            ((inside-letrec)
             (map (lambda (x)
                    (make-syntactic-closure
                     inner-env '() x))
                  body))))))

   (letrec-syntax
       (lambda (code env mac-env)
         (let/letrec-syntax-helper
          #t
          code
          env
          (lambda (body inner-env)
            ((inside-letrec)
             (map (lambda (x)
                    (make-syntactic-closure
                     inner-env '() x))
                  body))))))
   (begin
     (lambda (code env mac-env)
       ((inside-letrec)
        (map (lambda (x)
               (make-syntactic-closure
                env '() x))
             (cdr (expr*:value code))))))))

(define-env builtin-environment
  "module#"
  ((import
    (lambda (source env mac-env)
      (module-import
       (extract-synclosure-crawler
        (cdr (expr*:strip-locationinfo source)))
       env)))

   (export
    (lambda (code env mac-env)
      (if (or (not (environment-top? env))
              (not (environment-module env)))
          (error "Incorrectly placed export form"
                 (expr*:strip-locationinfo code)))
      (void)))
   
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
               (cadr (expr*:value code))))))))
   
   (define
     (lambda (code env mac-env)
       (let ((src (expr*:transform-to-lambda (expr*:cdr code))))
         (cond
          ((top-level)
           (let* ((name-form (car (expr*:value src)))
                  (def-env (if (syntactic-closure? name-form)
                               (syntactic-closure-env name-form)
                               env))
                  (name (expand-synclosure name-form env))
                  (ns (environment-add-define! def-env
                                               (expr*:value name))))
             (expr*:value-set
              code
              `(##define ,(expr*:value-set
                           name
                           (gen-symbol ns
                                       (expr*:value name)))
                 ,(expand-macro (cadr (expr*:value src)) env)))))
          
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
                (trans (caddr code))
                (before-name (expr*:value name)))
           (cond
            ((top-level)
             (begin
               (environment-add-macro-fun before-name
                                          trans
                                          env)
               (expand-macro
                `(module#define-macro-register ,name ,trans)
                env)))
            
            (else
             (error "Incorrectly placed define-syntax:"
                    (expr*:strip-locationinfo code))))))))
   
   (module#define-macro-register
     (lambda (form env mac-env)
       (void)))

   (syntax-begin
    (lambda (code env mac-env)
      (eval-in-next-phase `(##begin
                             ,@(cdr (expr*:value code)))
                          env)))
   
   (begin
     (lambda (code env mac-env)
       (expr*:value-set
        code
        ;; TODO This doesn't generate source code locations correctly?
        `(##begin ,@(expr*:map
                     (lambda (x)
                       (expand-macro x env))
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
         (let/letrec-syntax-helper
          #f
          code
          env
          (lambda (body inner-env)
            `(##begin
               ,@(map (lambda (x)
                        (expand-macro x inner-env))
                      body))))))

   (letrec-syntax
       (lambda (code env mac-env)
         (let/letrec-syntax-helper
          #t
          code
          env
          (lambda (body inner-env)
            `(##begin
               ,@(map (lambda (x)
                        (expand-macro x inner-env))
                      body))))))
   
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

   (define-macro
     (lambda (code env mac-env)
       ;; TODO This doesn't generate source code locations correctly
       (let ((code (expr*:strip-locationinfo code)))
         (let* ((src (transform-to-lambda (cdr code))))
           (expand-macro
            `(,(make-syntactic-closure
                empty-environment
                '()
                'define-syntax)
              ,(car src)
              (module#nh-macro-transformer ,(cadr src)))
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
            `(,(make-syntactic-closure empty-environment
                                       '()
                                       'define)
              ,(car src)
              (,(make-syntactic-closure empty-environment
                                        '()
                                        'if)
               (module#defined? ',(car src))
               ',(car src)
               ,(cadr src)))
            env)))))
   
   (compile-options
    (nh-macro-transformer
     (lambda (#!key options
                    cc-options
                    ld-options-prelude
                    ld-options
                    force-compile)
       (void))))

   (define-type
     (nh-macro-transformer
      (lambda args
        (expand 'define-type #f #f args))))))
