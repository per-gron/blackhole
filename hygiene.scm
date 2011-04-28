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
                    (code-car (expr*:value (car code))))
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
  (if (uninterned-symbol? sym)
      sym
      (string->symbol
       (string-append str
                      (symbol->string sym)))))

(define (parse-gambit-header-file file)
  (apply
   append
   (map (lambda (form)
          (if (eq? '##include (car form))
              (parse-gambit-header-file (cadr form))
              (list form)))
        (call-with-input-file file
          read-all))))

(define (parse-gambit-headers)
  (let ((table (make-table)))
    (for-each
     (lambda (ns-form)
       (if (not (and (eq? '##namespace
                          (car ns-form))
                     (equal? (caadr ns-form) "")))
           (error "Wrong format on gambit header."))
       (for-each (lambda (name)
                   (table-set! table
                               (cons #f name)
                               (list 'def name)))
                 (cdadr ns-form)))
     (parse-gambit-header-file "~~lib/gambit#.scm"))
    table))

(define gambit-builtin-table (parse-gambit-headers))

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
;; So far I've been talking about top-level and non-top-level
;; environments without defining them. This refers to lexical
;; scoping. For each scope, a new environment is created whose parent
;; is the environment that is in the scope above.
;;
;; The presence of syntactic closures further increases the complexity
;; of this data structure, because it must be possible to create
;; environments that override other environments' identifiers, without
;; actually modifying other environments. If other environments would
;; be mutated, the code becomes very clumsy and difficult to
;; understand, but most of all, it seems to be impossible to implement
;; let-syntax and letrec-syntax correctly.
;;
;; The ns field is the field that contain the hygiene information. 
;;
;; The actual data structure looks like this. It is one of:
;; * A box of an alist, where the cars are pairs of (phase . identifier).
;; * A table, where the keys are pairs of (phase . symbol).
;; * A pair of two datastructures of the this type. Lookups are done
;;   first in the car, and if nothing was found, search in the
;;   cdr.
;; * A procedure returning two values; a ns datastructure and a value
;;   that will be used as the phase. Lookups are done on the ns return
;;   value. This is used for built-in names.
;;
;; An important thing to note here is that tables, pairs and
;; procedures are used exclusively for top-level bindings. Only the
;; boxed alists are used for lexical bindings. identifier=? and
;; environment-get relies on this to be able to do the right thing.
;;
;; The values that the ns data structur contains are lists that can
;; either be ('mac [macro function] [macro environment]) or ('def
;; [symbol to be expanded to])
(define-type env
  id: A8981FB8-BC38-47BA-8707-5A3F5962D610
  ;; The parent environment. This is how lexical scope is implemented;
  ;; a number means that it is a top-level environment, an environment
  ;; means that the scope above this one is that environment.
  ;;
  ;; If the environment is top-level, this should be #f.
  (parent unprintable: read-only:)
  ;; The module of this environment. #f if REPL.
  (module unprintable: read-only:)
  
  ;; The namespace string of the current environment. Lazily
  ;; initialized, so it's #f on a newly created environment.
  (ns-string unprintable:)
  
  ;; These are unprintable because it often contains cyclic data
  ;; structures, which in practise crashes gambit when prettyprinting
  ;; it, which is a huge pain.
  (ns unprintable: read-only:)
  
  ;; This is usually a pointer from the environment to itself, but if
  ;; the scope is a let(rec)-syntax scope, this is a pointer to the
  ;; nearest ancestor environment that introduces scope. Note that
  ;; scope-env always points to an environment whose scope-env points
  ;; to itself. It is used to be able to implement hygiene for things
  ;; like
  ;;
  ;; (let ()
  ;;   (let-syntax ()
  ;;     (define (b) #t))
  ;;   (b))
  ;;
  ;; It is not read-only, but that's only to be able to implement the
  ;; circular pointer.
  (scope-env unprintable:))

(define (make-environment parent
                          #!optional
                          (ns (box '()))
                          (introduces-scope? #t))
  (let ((res (make-env (and (env? parent) parent)
                       (if (env? parent)
                           (environment-module parent)
                           parent)
                       (and (env? parent)
                            (env-ns-string parent))
                       ns
                       #f)))
    (env-scope-env-set! res
                        (if introduces-scope?
                            res
                            (env-scope-env parent)))
    res))

(define (environment-find-top env)
  (let ((parent (env-parent env)))
    (if (environment? parent)
        (environment-find-top parent)
        env)))

(define (environment-top? env)
  (not (env? (env-parent env))))

(define environment? env?)

(define environment-parent env-parent)

(define environment-module env-module)

(define (environment-ancestor-of? env descendant #!optional (distance 0))
  ;; FIXME Make this test constant-time (is that possible)
  (if (eq? env descendant)
      distance
      (let ((p (env-parent descendant)))
        (and (env? p)
             (environment-ancestor-of? env p (+ 1 distance))))))

;; This is one of the really core functions of the hygiene system.
;;
;; It does a lookup in an orig-env for name. When looking up a
;; syntactic closure, sc-environment is the closure's environment.
;;
;; 
(define (environment-get orig-env name
                         #!key
                         sc-environment
                         ignore-globals
                         (phase (expansion-phase)))
  (if (not sc-environment)
      (set! sc-environment orig-env))
  (let* ((gone-through-sc-env #f)
         (best-def #f)
         (best-distance #f)
         (maybe-update-best-def
          (lambda (distance def)
            (and distance
                 def
                 (if (or (not best-distance)
                         (< distance best-distance))
                     (begin
                       (set! best-def def)
                       (set! best-distance distance)))))))
    
    (let env-get ((env orig-env))
      (let ns-get ((ns (env-ns env)) (phase phase))
        (cond
         ((and (not gone-through-sc-env)
               (eq? env sc-environment))
          (set! gone-through-sc-env #t)
          (env-get env))
         
         ((box? ns)
          (let ((env-ancestor-of?-env-sc-env
                 ;; Avoid caculating this many times
                 (environment-ancestor-of? env sc-environment)))
            (for-each
             (lambda (def)
               (let* ((phase/name-pair (car def))
                      (p (car phase/name-pair))
                      (n (cdr phase/name-pair)))
                 (and (= p phase)
                      (maybe-update-best-def
                       (cond
                        ((syntactic-closure? n)
                         (and (eq? name
                                   (syntactic-closure-symbol n))
                              (environment-ancestor-of?
                               (env-scope-env
                                (syntactic-closure-env n))
                               sc-environment)))
                        
                        (else
                         (and (eq? name n)
                              env-ancestor-of?-env-sc-env)))
                       (cdr def)))))
             (unbox ns)))
          
          (env-get (env-parent env)))
         
         ((pair? ns)
          (or (ns-get (car ns) phase)
              (ns-get (cdr ns) phase)))
         
         ((table? ns)
          (if (not gone-through-sc-env)
              (env-get sc-environment)
              (and (not ignore-globals)
                   (maybe-update-best-def
                    (environment-ancestor-of? env sc-environment)
                    (table-ref ns
                               (cons phase name)
                               #f)))))
         
         ((procedure? ns)
          (if (not gone-through-sc-env)
              (env-get sc-environment)
              (call-with-values ns
                (lambda (new-ns new-phase)
                  (ns-get new-ns new-phase)))))
         
         (else
          (error "Invalid ns" ns)))))
    
    best-def))

(define scope-level (make-parameter 0))

(define (generate-namespace)
  (string-append
   (number->string (scope-level))
   "#"))

(define (environment-namespace env)
  (or (env-ns-string env)
      (let ((ns-string
             (let ((phase (expansion-phase))
                   (ns (module-namespace
                        (environment-module env))))
               (if (zero? phase)
                   ns
                   (string-append
                    (substring ns
                               0
                               (max 0 (- (string-length ns) 1)))
                    "~"
                    (if (> phase 1)
                        (number->string phase)
                        "")
                    "#")))))
        (env-ns-string-set! env ns-string)
        ns-string)))

(define (eval-in-next-phase code env)
  (parameterize
   ((expansion-phase (+ 1 (expansion-phase)))
    (calc-mode 'load)
    ;; Inside-letrec must be set to #f, otherwise strange errors
    ;; will occur when the continuation that is within that closure
    ;; gets invoked at the wrong time.
    (inside-letrec #f))
   (eval-no-hook
    (expand-macro code env))))

(define (ns-add! ns phase name val)
  (cond
   ((table? ns)
    (table-set! ns
                (cons phase name)
                val))

   ((box? ns)
    (set-box! ns
              (cons (cons (cons phase
                                name)
                          val)
                    (unbox ns))))

   ((pair? ns)
    (ns-add! (car ns) phase name val))

   ((procedure? ns)
    (error "Cannot modify procedure ns" ns))

   (else
    (error "Invalid ns" ns))))



(define (environment-add-def! env exported-name actual-name
                              #!key (phase (expansion-phase)))
  (ns-add! (env-ns env)
           phase
           exported-name
           (list 'def actual-name)))

(define (environment-add-mac! env exported-name fun m-env
                              #!key (phase (expansion-phase)))
  (ns-add! (env-ns env)
           phase
           exported-name
           (list 'mac fun m-env)))


(define-type syntactic-closure
  id: 0B9FFE46-B995-48C2-B5E5-B66FD3FD335C
  constructor: make-syntactic-closure-internal
  
  (env read-only:)
  (symbol read-only:))

(define (make-syntactic-closure env ids form)
  (cond
   ((symbol? form)
    (if (or (memq form ids)
            (string-contains (symbol->string form)
                             #\#))
        form
        (make-syntactic-closure-internal env form)))

   ((pair? form)
    (cons (make-syntactic-closure env ids (car form))
          (make-syntactic-closure env ids (cdr form))))

   ((vector? form)
    (vector-map (lambda (x)
                  (make-syntactic-closure env ids x))
                form))

   ((syntactic-capture? form)
    (cond
     ((eq? ids '())
      (make-syntactic-capture
       (lambda (inner-env)
         ((syntactic-capture-proc form) env))))
     
     (else
      (make-syntactic-capture
       (lambda (inner-env)
         (let ((created-env (make-environment env (box '()) #f)))
           (for-each (lambda (symbol)
                       (ns-add! (env-ns created-env)
                                (expansion-phase)
                                symbol
                                (environment-get inner-env symbol)))
                     ids)
           ((syntactic-capture-proc form) created-env)))))))
   
   ;; Forms that are not symbols, pairs, vectors or syntactic closures
   ;; (for instance null, numbers or strings) don't need to be wrapped
   ;; in a syntactic closure, because their meaning is independent of
   ;; lexical context.
   ;;
   ;; Also if form already is a fully closed syntactic closure, there
   ;; is no need to wrap it again.
   (else
    form)))

;; The structure that is returned by capture-syntactic-environment
(define-type syntactic-capture
  id: 268DA48C-6617-46FC-9DF9-F69233935278
  proc)

(define (expand-syncapture cap env)
  (if (syntactic-capture? cap)
      (expand-syncapture
       ((syntactic-capture-proc cap) env)
       env)
      cap))

(define (capture-syntactic-environment proc)
  (make-syntactic-capture proc))

(define (environment-add-macro-fun name fun env)
  (let ((fn
         (lambda (name env)
           (environment-add-mac!
            (env-scope-env env)
            name ;; The exported name
            (eval-in-next-phase fun env)
            env))))
    (cond
     ((symbol? name)
      (fn name env))

     ((syntactic-closure? name)
      (fn (syntactic-closure-symbol name)
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
    (environment-add-def! (env-scope-env env)
                          name
                          (gen-symbol ns name))
    ns))

;; Helper function for environment-add-defines and
;; environment-add-macros.
(define (environment-add-to-ns env
                               names
                               list->val
                               #!key
                               mutate
                               (introduces-scope? #t))
  (let* ((new-env
          (if mutate
              env
              (make-environment env
                                (box '())
                                introduces-scope?)))
         (box (env-ns new-env)))
    
    (if (not (box? box))
        (error "Environment must not be top-level" new-env))
    
    (let ((defined-names
            (map (lambda (n)
                   (list->val n new-env))
                 names)))
      (set-box!
       box
       (append defined-names
               (unbox box)))
      
      (values new-env defined-names))))

;; Names is a list of identifiers or pairs, where the car is the
;; identifier and the cdr is its namespace.
(define (environment-add-defines env names #!key mutate)
  (let ((phase (expansion-phase)))
    (environment-add-to-ns
     env
     names
     (lambda (n new-env)
       (if (pair? n)
           (list (cons phase (car n))
                 'def
                 (gen-symbol (cdr n)
                             (let ((sc (car n)))
                               (if (syntactic-closure? sc)
                                   (syntactic-closure-symbol sc)
                                   sc)))
                 new-env)
           (begin
             (scope-level (+ 1 (scope-level)))
             (list (cons phase n)
                   'def
                   (gen-symbol (generate-namespace)
                               (if (syntactic-closure? n)
                                   (syntactic-closure-symbol n)
                                   n))
                   new-env))))
     mutate: mutate)))

;; Macs is a list of lists where car is name, cadr is the sexp
;; of the macro transformer
(define (environment-add-macros env
                                macs
                                #!key
                                mutate
                                (mac-env env))
  (let ((phase (expansion-phase)))
    (environment-add-to-ns
     env
     macs
     (lambda (m new-env)
       (list (cons phase (car m))
             'mac
             (eval-in-next-phase (cadr m) env)
             mac-env))
     mutate: mutate
     introduces-scope?: #f)))

;; (inside-letrec) implies (not (top-level))
(define top-level (make-parameter #t))
(define inside-letrec (make-parameter #f))
(define expansion-phase (make-parameter 0))


(define transform-forms-to-triple-define-constant
  (gensym 'transform-forms-to-triple-define))

(define transform-forms-to-triple-define-syntax-constant
  (gensym 'transform-forms-to-triple-define-syntax))

;; The implementation of this function -forms-to-letrec is a
;; little bit odd. I made them this way because it's important
;; that no form gets macro-expanded more than once, since this
;; makes the entire macro expansion algorithm take exponential
;; time in respect to the nesting level of macros that use this
;; function (like lets, lambdas, syntax-rules macros)
(define (transform-forms-to-triple form parent-env)
  (let* ((exprs '())
         (defs '())
         (define-env (make-environment parent-env))
         (ieq? (lambda (a b env)
                 (identifier=? builtin-environment
                               a
                               define-env
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
                               rest))))))))
    
    (parameterize
     ((top-level #f)
      (inside-letrec #t))
     (call/cc
      (lambda (ret)
        (let loop ((form form) (env define-env))
          (inside-letrec
           (lambda (forms inner-env)
             (loop forms inner-env)
             (loop (cdr form) env)
             (ret #t)))

          (let* ((x (and (pair? form)
                         (expand-macro (car form) env)))
                 (x-hd-sym (and (pair? x)
                                (if (syntactic-closure? (car x))
                                    (syntactic-closure-symbol (car x))
                                    (car x)))))
            (cond
             ((null? form)
              #!void) ;; We're done
             
             ((eq? #!void x)
              (loop (cdr form) env))
             
             ((not (pair? x))
              (push-exprs-expand x (cdr form) env))
             
             ((eq? x-hd-sym
                   transform-forms-to-triple-define-constant)
              (let ((src (transform-to-lambda (cdr x))))
                (set! defs
                      (cons (cons env src)
                            defs))
                
                (environment-add-defines define-env
                                         (list (car src))
                                         mutate: #t)
                
                (loop (cdr form) env)))
             
             ((eq? x-hd-sym
                   transform-forms-to-triple-define-syntax-constant)
              (environment-add-macros define-env
                                      (list (cdr x))
                                      mutate: #t
                                      mac-env: env)
              
              (loop (cdr form) env))
             
             ((or (ieq? 'begin (car x) env)
                  (ieq? '##begin (car x) env))
              (loop (append (cdr x) (cdr form)) env))
             
             (else
              (push-exprs-expand x (cdr form) env))))))))

    (values exprs defs define-env)))

(define (transform-forms-to-letrec form env)
  (call-with-values
      (lambda ()
        (transform-forms-to-triple form env))
    (lambda (inner-exp defs new-env)
      (if (null? defs)
          inner-exp
          `((letrec
                ,(map (lambda (def)
                        (expand-macro (cdr def)
                                      (car def)))
                      defs)
              ,@inner-exp))))))

(define (let/letrec-helper rec code env)
  (parameterize
   ((inside-letrec #f)
    (scope-level (scope-level)))
   (apply
    (lambda (prefix params . body)
      (let* (;; If this is not a letrec, do the expansion of the parameter
             ;; initializer here. It has to be done before the call to
             ;; environment-add-defines, otherwise the let will leak if
             ;; it's given syntactic closures as parameter names and/or
             ;; initializers.
             (param-values
              (map (lambda (x)
                     (let ((x (expand-syncapture x env)))
                       (if (not
                            (and (list? x)
                                 (= 2 (length x))))
                           (error "Invalid binding" code))
                       (if rec
                           (cadr x)
                           (expand-macro (cadr x) env))))
                   params)))
        (call-with-values
            (lambda ()
              (environment-add-defines
               env
               (let ((ps (map (lambda (pair)
                                (if (list? pair)
                                    (car pair)
                                    (error "Invalid form: " code)))
                              params)))
                 (if prefix
                     (cons prefix ps)
                     ps))))
          (lambda (let-env defined-params)
            `(,(if rec
                   'letrec
                   'let)
              ,@(if prefix
                    `(,(expand-macro prefix let-env))
                    '())
              ,(map (lambda (p p-val dp)
                      (cons (caddr dp)
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
    (let ((code (expand-syncapture code env)))
      (if (and (not rec)
               (identifier? (cadr code)))
          (cdr code)
          (cons #f (cdr code)))))))

(define (lambda-helper code env)
  (parameterize
   ((inside-letrec #f)
    (scope-level (scope-level)))
   (apply
    (lambda (params . body)
      (call-with-values
          (lambda ()
            (environment-add-defines
             env
             (filter
              (lambda (x) x)
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
                         (if key (cons s "") s)))
                      
                      (else #f))))
                 params)))))
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
                        (let ((dp (let ((dp (car current-defined-params)))
                                    (set! current-defined-params
                                          (cdr current-defined-params))
                                    dp)))
                          (cond
                           ((identifier? p)
                            (caddr dp))
                           
                           ((pair? p)
                            (cons (caddr dp)
                                  (expand-macro (cdr p) env)))
                           
                           (else
                            (error "Invalid parameter list: "
                                   params)))))))
                    params))))
            `(lambda ,hygparams
               ,@(transform-forms-to-letrec body lambda-env))))))
   (cdr (expand-syncapture code env)))))

(define (let/letrec-syntax-helper rec form env thunk)
  (let ((form (expr*:strip-locationinfo form))
        (env (if rec (make-environment env) env)))
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

       (thunk body
              (call-with-values
                  (lambda ()
                    (environment-add-macros env
                                            bindings
                                            mutate: rec))
                (lambda (env _)
                  env))))
     (cdr form))))

;; Expansion functions

(define (hyg-expand-macro-quasiquote env source #!optional (level 1))
  (let ((code (expr*:value source)))
    (cond
     ((zero? level)
      (expand-macro source env))
     
     ((pair? code)
      (let ((hd (expand-syncapture
                 (expr*:value (car code))
                 env)))
        (cond
         ((or (identifier=? env hd
                            builtin-environment 'unquote)
              (identifier=? env hd
                            builtin-environment 'unquote-splicing))
          (expr*:value-set
           source
           (list (expr*:value-set (car code)
                                  (extract-synclosure-crawler hd))
                 (hyg-expand-macro-quasiquote
                  env
                  (cadr code)
                  (- level 1)))))
         
         ((identifier=? env hd
                        builtin-environment 'quasiquote)
          (expr*:value-set
           source
           (list (expr*:value-set (car code)
                                  'quasiquote)
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
                         (identifier=? env (expr*:value (car lst))
                                       builtin-environment 'unquote)
                         (pair? (expr*:value (cdr lst)))
                         (null? (expr*:value (cddr lst))))
                    (list
                     (cons 'unquote-splicing
                           (cdr (fn lst)))))
                   
                   ((pair? lst)
                    (cons (fn (car lst))
                          (m (expr*:value (cdr lst)))))
                   
                   ((null? lst) '())
                   
                   (else (fn lst))))))
            (expr*:value-set
             source
             (m code)))))))

     ((vector? code)
      (expr*:value-set
       source
       (vector-map (lambda (x)
                     (hyg-expand-macro-quasiquote env
                                                  x
                                                  level))
                   code)))

     ((syntactic-closure? code)
      (expr*:value-set source
                       (syntactic-closure-symbol code)))

     ((syntactic-capture? code)
      (hyg-expand-macro-quasiquote
       env
       (expr*:value-set source
                        (expand-syncapture code env))
       level))

     (else
      source))))

;; This function simply removes syntactic closures, without
;; taking the syntactic environments into account.
(define (extract-synclosure-crawler source)
  (let ((code (expr*:value source)))
     (cond
      ((pair? code)
       (expr*:value-set
        source
        (dotted-map (lambda (x)
                      (extract-synclosure-crawler x))
                    code)))

      ((vector? code)
       (expr*:value-set
        source
        (vector-map extract-synclosure-crawler
                    code)))

      ((syntactic-closure? code)
       (expr*:value-set
        source
        (syntactic-closure-symbol code)))
      
      (else
       source))))

;; TODO Remove this. It's used in one place though. And also in module.scm
(define (expand-synclosure sc env #!optional ns)
  (cond
   ((and ns (symbol? sc))
    (gen-symbol ns sc))
   
   ((not (syntactic-closure? sc))
    sc)
   
   ((environment-get
     env
     (syntactic-closure-symbol sc)
     sc-environment: (syntactic-closure-env sc)) =>
     (lambda (val)
       (if (eq? 'def (car val))
           (cadr val)
           (error "Macro name can't be used as a variable:"
                  (syntactic-closure-symbol sc)))))
   
   (else
    (gen-symbol (environment-namespace env)
                (syntactic-closure-symbol sc)))))

(define (expand-macro source
                      #!optional
                      (env (top-environment)))
  (let* ((source
          (expr*:value-set
           source
           (expand-syncapture
            (expr*:value source)
            env)))
         (code (expr*:value source)))
    (cond
     ((pair? code)
      ;; Expand the head if it is a synclosure; that closure might in
      ;; turn expand into a macro name.
      (let* ((hd-val (expand-syncapture
                      (expr*:value (car code))
                      env))
             (hd (if (syntactic-closure? hd-val)
                     (syntactic-closure-symbol hd-val)
                     hd-val))
             (search-env (if (syntactic-closure? hd-val)
                             (syntactic-closure-env hd-val)
                             env))
             (default-action
               (lambda ()
                 (parameterize
                  ((inside-letrec #f)
                   (top-level (and (eq? '##begin hd)
                                   (top-level))))
                  (expr*:dotted-map (lambda (x)
                                      (expand-macro x env))
                                    source)))))
        (if (symbol? hd) ;; This is equivalent to (identifier? hd-val)
            (cond
             ((environment-get env hd sc-environment: search-env) =>
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
                     (make-environment (caddr val))))))

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
                         ##declare
                         ##let
                         ##let*
                         ##letrec
                         ##lambda
                         ##define
                         ##cond
                         ##case
                         ##define-macro))
              (expr*:value-set code
                               (cons (expr*:value-set (car code)
                                                      hd)
                                     (cdr code))))
             
             (else
              (default-action)))
            
            (default-action))))

     ((syntactic-closure? code)
      (expr*:value-set
       source
       (cond
        ((environment-get
          env
          (syntactic-closure-symbol code)
          sc-environment: (syntactic-closure-env code)) =>
          (lambda (val)
            (if (eq? 'def (car val))
                (cadr val)
                (error "Macro name can't be used as a variable:" code))))

        (else
         (gen-symbol (environment-namespace env)
                     (syntactic-closure-symbol code))))))

     ((uninterned-symbol? code)
      source)
     
     ((symbol? code)
      (expr*:value-set
       source
       (cond
        ((string-contains (symbol->string code) #\#)
         code)
        
        ((environment-get env code) =>
         (lambda (val)
           (if (eq? 'def (car val))
               (cadr val)
               (error "Macro name can't be used as a variable:" code))))
        
        (else
         (gen-symbol (environment-namespace env)
                     code)))))
     
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
      (syntactic-closure? id)))

(define (identifier=? env1 id1 env2 id2)
  (define (strip-synclosure env id)
    (cond
     ((not (syntactic-closure? id))
      (cons id env))

     (else
      (cons (syntactic-closure-symbol id)
            (syntactic-closure-env id)))))
  
  ;; First, strip the identifiers to symbols then, check if the
  ;; symbols are equal. If they are not, then this is not the same
  ;; identifier.
  ;;
  ;; However, even if the symbols are the same, they could be
  ;; different identifiers. This can only happen if the symbols are of
  ;; the form [number]#name. If it is an absolute namespace like
  ;; module#, then the symbols must be the same.  This is an example:
  ;;
  ;; (let ()
  ;;   (let (x) x)
  ;;   (let (x) x))
  ;;
  ;; Both x variables would expand into something like 1#x, but they
  ;; are still not the same identifier.
  (let* ((strip1 (strip-synclosure env1 id1))
         (inner-id1 (car strip1))
         (inner-env1 (cdr strip1))
         
         (strip2 (strip-synclosure env2 id2))
         (inner-id2 (car strip2))
         (inner-env2 (cdr strip2)))
    (and (eq? inner-id1 inner-id2)
         (let ((one (environment-get env1
                                     inner-id1
                                     sc-environment: inner-env1
                                     ;; TODO I'm not sure this is the
                                     ;; correct place to have this. It
                                     ;; is here because syntax-rules
                                     ;; needs it for the literals part
                                     ;; to work.
                                     ignore-globals: #t))
               (two (environment-get env2
                                     inner-id2
                                     sc-environment: inner-env2
                                     ignore-globals: #t)))
           (or (and (not one)
                    (not two)
                    (eq? inner-id1 inner-id2))
               (and one
                    two
                    (eq? one two)))))))

;; Tools for defining macros

(define (sc-macro-transformer thunk)
  (lambda (form env mac-env)
    (expand-macro (make-syntactic-closure
                   mac-env
                   '()
                   (thunk (expr*:strip-locationinfo form)
                          env))
                  env)))

(define (rsc-macro-transformer thunk)
  (lambda (form env mac-env)
    (expand-macro (thunk (expr*:strip-locationinfo form)
                         mac-env)
                  env)))

(define (er-macro-transformer thunk)
  (lambda (form env mac-env)
    (expand-macro
     (thunk (expr*:strip-locationinfo form)
            (lambda (sym)
              (make-syntactic-closure mac-env '() sym))
            (lambda (a b)
              (identifier=? env a env b)))
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
                               `(cons #f ',(car mac))
                               ''mac
                               mac-name
                               name))
                       macs macro-names)))))
       (set! ,name
             (make-environment #f -ns-))))))

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
          (lambda (body inner-env)
            ((inside-letrec)
             body
             inner-env)))))

   (letrec-syntax
       (lambda (code env mac-env)
         (let/letrec-syntax-helper
          #t
          code
          env
          (lambda (body inner-env)
            ((inside-letrec)
             body
             inner-env)))))
   (begin
     (lambda (code env mac-env)
       ((inside-letrec)
        (cdr (expand-syncapture (expr*:value code)
                                env))
        env)))))

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
     (lambda (#!optional name)
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
                  (def-env (if (syntactic-closure? name-form)
                               (syntactic-closure-env name-form)
                               env))
                  (ns (environment-add-define! def-env
                                               (expr*:value name))))
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
      (eval-in-next-phase `(begin
                             ,@(cdr (expr*:value code)))
                          env)))
   
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
         (let/letrec-syntax-helper
          #f
          code
          env
          (lambda (body inner-env)
            `(begin
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
            `(begin
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
      (expr*:value-set
       source
       (let ((code (expr*:value source)))
         (cons
          (expr*:value-set (car code)
                           'cond-expand)
          (map (lambda (inner-source)
                 (let ((inner-code (expr*:value inner-source)))
                   (if (not (pair? inner-code))
                       (error "Invalid cond-expand form: "
                              (expr*:strip-locationinfo source)))
                   
                   (cons (extract-synclosure-crawler
                          (car inner-code))
                         (expand-macro (cdr inner-code) env))))
               (cdr code)))))))

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

   (c-lambda
    (lambda (source env mac-env)
      (extract-synclosure-crawler source)))

   (c-define
    (lambda (source env mac-env)
      (let ([src (cdr (expand-syncapture (expr*:value source) env))])
        (cond
          [(< (length src) 6)
           (error "Ill-formed c-define form"
                  (expr*:strip-locationinfo source))]
          [(top-level)
           (let* ([src    (expr*:value src)]
                  [var-fs (expr*:value (list-ref src 0))]
                  [types  (list-ref src 1)]
                  [result (list-ref src 2)]
                  [c-name (list-ref src 3)]
                  [scope  (list-ref src 4)]
                  [body   (list-tail src 5)]
                  [var-form (car var-fs)]
                  [var      (expand-syncapture var-form env)]
                  [formals  (cdr var-fs)]
                  [ns (environment-add-define! env (expr*:value var))])
             (expr*:value-set
              source
              `(c-define (,(expr*:value-set
                            var
                            (gen-symbol ns (expr*:value var)))
                          ,@(map (lambda (f-form)
                                   (let ([f (expand-syncapture f-form env)])
                                     (expr*:value-set
                                      f
                                      (gen-symbol ns (expr*:value f)))))
                                 formals))
                   ,(extract-synclosure-crawler types)
                   ,(extract-synclosure-crawler result)
                   ,(extract-synclosure-crawler c-name)
                   ,(extract-synclosure-crawler scope)
                 ,@(expand-macro body env))))]
          [else
           (error "Incorrectly placed c-define:"
                  (expr*:strip-locationinfo source))]))))

   (c-define-type
    (lambda (source env mac-env)
      (extract-synclosure-crawler source)))
   
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
        (expand 'define-type #f #f args))))

   (define-structure
     (nh-macro-transformer
      (lambda args
        (expand 'define-type #f #f args))))))
