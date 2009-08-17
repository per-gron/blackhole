;; TODO Things I should add support for:
;; Reuse gensyms over different scopes.
;; the <><=>= operators don't support multiple arguments
;; instanceof
;; /= *= += -= and so on
;; bitwise operators ... what are they called in scheme?
;; do..while
;; for..in
;; continue break return?
;; switch
;; labels
;; try..finally
;; implement the scheme error function
;; - doesn't work with only one argument
;; Implement vector?/vector-fill!
;; Make do work
;; string-camelize crashes on --
;; obj->query-string doesn't get converted to a js compatible thing


(import ../srfi/1
        ../string/util
        ../string/pregexp
        counter)

(syntax-begin
 (import ../string/util))

(export with-js-environment
        js-compile
        js-compile-inside
        js-lambda
        js-lambda?
        js-fun
        js-str
        
        make-js-module
        js-module?
        js-module-symbols
        js-module-symbols-set!
        js-module-dependencies
        js-module-dependencies-set!
        js-module-macros
        js-module-macros-set!
        js-module-scm-code
        js-module-scm-code-set!
        js-module-id
        js-module-id-set!
        js-module-code
        js-module-code-set!
        make-constant-js-module
        
        js-code
        jsc
        jsp
        js-module
        define-js-module-object)

(define-macro (push! var val)
  `(set! ,var (cons ,val ,var)))

(define (any pred lst) ; This is ridiculously non-optimized
  (fold (lambda (a b) (or a b)) #f (map pred lst)))

(define (append! lst rest)
  (cond
   ((null? lst) rest)
   ((null? (cdr lst)) (set-cdr! lst rest))
   (else (append! (cdr lst) rest))))

(define-macro (condp name . conds)
  `(cond ,@(map (lambda (x) `((,(car x) ,name) ,@(cdr x))) conds)))

(define (string-join between args)
  (apply string-append (join between args)))

(define (pairify lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) lst)
        (else (cons (cons (car lst) (cadr lst)) (pairify (cddr lst))))))

(define (transform-forms-to-pair form)
  (let* ((exprs '())
         (defs '())
         (push-expr (lambda (x)
                      (set! exprs (cons x exprs))))
         (push-def (lambda (x)
                     (set! defs (cons x defs)))))
    (for-each
     (lambda (x)
       (cond
        ((not (pair? x)) (push-expr x))

        ((eq? 'define (car x))
         (push-def x))

        ((eq? 'begin (car x))
         (let ((res (transform-forms-to-pair (cdr x))))
           (push-expr `(begin ,@(car res)))
           (for-each (lambda (x)
                       (push-def x))
                     (cdr res))))
        
        (else (push-expr x))))
     form)
    (cons (reverse exprs)
          (reverse defs))))

(define (transform-forms-to-letrec form)
  (let ((res (transform-forms-to-pair form)))
    (if (null? (cdr res))
        form
        `((letrec ,(map (lambda (x)
                          (let ((src (module#transform-to-lambda (cdr x))))
                            src))
                        (cdr res))
            ,@(car res))))))

(define *js-macro-env* (make-parameter '()))

(define *js-scope-env* (make-parameter '()))

(define *js-gensym-env* (make-parameter #f))

(define *js-gensym-symbols*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define *js-module-env* (make-counter
                         (counter-symbolizer
                          (counter-stringer
                           *js-gensym-symbols*))))

(define (js-gensym #!optional (env (*js-gensym-env*)))
  (env))

(define (js-add-macro name body)
  (*js-macro-env* (cons (cons name
                              body)
                        (*js-macro-env*))))

(define-macro (js-define-macro args . body)
  (if (symbol? args)
      `(js-add-macro (quote ,args) ,@body)
      (let ((name (car args))
            (args (cdr args)))
        `(js-add-macro (quote ,name) (lambda ,args ,@body)))))

(define-macro (js-add-alpha exp from to)
  (let ((from-gs (gensym))
        (to-gs (gensym)))
    `(let ((,from-gs ,from)
           (,to-gs ,to))
       (map (lambda (exp)
              `(js-alpha-convert
                ,,from-gs
                ,,to-gs
                ,exp))
            ,exp))))

(define (js-alpha-convert from to body)
  (cond
   ((and (symbol? body) (eq? body from))
    (js-expand-macro to))
   ((and (list? body) (not (null? body)))
    (if (and (eq? (car body) 'js-scope)
             (or (any (lambda (x) (eq? x from)) ; Search in the defined variables
                      (cdadr body))
                 (any (lambda (x) (eq? x from)) ; Search in the function's paramlist
                      (caadr body))))
        body
        (map (lambda (x) (js-alpha-convert from to x)) body)))
   (else body)))

(define (js-expand-macro expr)
  (if (list? expr)
      (let ((mac (assoc (car expr) (*js-macro-env*)))
            (hd (car expr)))
        (if mac
            (js-expand-macro (apply (cdr mac) (cdr expr)))
            (cond
             ((eq? hd 'js-syntax)
               `(js-syntax ,@(map js-expand-macro (cdr expr))))
             ((eq? hd 'js) expr)
             ((eq? hd 'js-scope)
              (parameterize
               ((*js-scope-env* (cadr expr))
                (*js-macro-env* (*js-macro-env*)))
               `(js-scope ,(cadr expr)
                          ,@(map js-expand-macro (cddr expr)))))
             ((eq? hd 'js-alpha-convert)
              (js-alpha-convert (cadr expr)
                                (caddr expr)
                                (js-expand-macro (cadddr expr))))
             (else
              `(js-syntax ,(js-expand-macro (car expr))
                          (js "(")
                          ,@(join '(js ",") (map js-expand-macro (cdr expr)))
                          (js ")"))))))
      expr))

(define (js-undotify body)
  (cond
   ((symbol? body)
    (let* ((str (symbol->string body))
           (spl (string-split #\. str))
           (spl (if (equal? (car spl) "")
                    (cons "this" (cdr spl))
                    spl)))
      (if (null? (cdr spl))
          body
          `(: ,@(map string->symbol spl)))))
   
   ((pair? body) (map js-undotify body))
   
   (else body)))

(define (with-js-environment thunk)
  (parameterize
   ((*js-gensym-env* (make-counter
                      (counter-symbolizer
                       (counter-stringer
                        *js-gensym-symbols*))))
    (*js-macro-env* (*js-macro-env*)))
   (thunk)))

(define (js-compile expr
                    #!optional
                    (env (cons
                          (make-counter
                           (counter-symbolizer
                            (counter-stringer
                             *js-gensym-symbols*)))
                          (*js-macro-env*))))
  (parameterize
   ((*js-gensym-env* (car env))
    (*js-macro-env* (cdr env)))
   (cons (js-compile-inside (js-undotify expr))
         (cons (*js-gensym-env*)
               (*js-macro-env*)))))

(define (js-compile-inside expr)
  (let ((expr (js-expand-macro expr)))
    (condp
     expr
     (string? (string-append
               "\""
               (pregexp-replace*
                "\n"
                (pregexp-replace*
                 "\""
                 expr
                 "\\\\\"")
                "\\\\\\n")
               "\""))
     (null? "null")
     (symbol? (string-camelize (symbol->string expr)))
     (number? (number->string expr))
     (boolean? (if expr "true" "false"))
     (list? (let ((h (car expr)))
              (cond
               ((eq? h 'js)
                (apply string-append (cdr expr)))
               ((eq? h 'js-syntax)
                (apply string-append (map js-compile-inside (cdr expr))))
               ((eq? h 'js-scope)
                (let ((vars (cdadr expr)))
                  (string-append
                   (if (null? vars)
                       ""
                       (string-append
                        "var "
                        (string-join ","
                                     (map symbol->string
                                          (cdadr expr)))
                        ";"))
                   (apply string-append
                          (map js-compile-inside
                               (cddr expr))))))
               (else
                (error "Invalid expression" expr))))))))

; Compiler utility macros

(define-macro (jsc expr)
  `(car (js-compile (quote ,expr))))

(define-macro (jsp expr)
  `(print (jsc ,expr) "\n"))

;; Javascript lambdas

(define-type js-fun
  id: 3B52B8C0-FFF0-43AF-8CA3-FD3AD60848A5
  str
  fun)

(define-syntax js-lambda
  (syntax-rules ()
    ((js-lambda args body ...)
     (make-js-fnu
      (js-compile '(lambda args body ...))
      (lambda args body ...)))))

(define js-lambda? js-fun?)

(define (js-fun fun)
  (js-fun-fun fun))

(define (js-str fun)
  (js-fun-str fun))

;; Javascript modules

(define *current-module* (make-parameter #f))

(define-type js-module
  id: B2010B92-674C-459C-B20F-AD73B4EEA30D
  symbols
  dependencies
  macros
  scm-code
  id
  code)

(let ((orig-fun make-js-module))
  (set! make-js-module
        (lambda (syms deps code)
          (let ((c (make-counter
                    (counter-symbolizer
                     (counter-stringer
                      *js-gensym-symbols*)))))
            (let ((exports (map (lambda (x)
                                  (cons x (c)))
                                syms))
                  (mod-id (*js-module-env*)))
              (orig-fun
               exports
               deps
               '()
               code
               mod-id
               (parameterize
                ((*current-module* (cons mod-id
                                         exports)))
                (string-append
                 (car
                  (js-compile
                   `(with-modules
                     ,deps
                     (begin
                       (set! _
                             (if (equal? (typeof _) "undefined")
                                 (obj)
                                 _))
                       (set!
                        (: _ ,mod-id)
                        (let ((mod (obj)))
                          ,@code
                          ,@(map (lambda (x)
                                   `(set! (: mod ,(cdr x)) ,(car x)))
                                 exports)
                          mod))))))
                 ";"))))))))

(define-macro (js-module syms deps . code)
  `(make-js-module ',syms (list ,@deps) ',code))

(define-syntax define-js-module-object
  (sc-macro-transformer
   (lambda (form env)
     (apply
      (lambda (name mod)
        (let* ((name-sc (make-syntactic-closure env '() name))
               (mod-sc (make-syntactic-closure env '() mod))
               (old-id (with-exception-catcher
                        (lambda (e)
                          #f)
                        (lambda ()
                          (js-module-id (eval name-sc))))))
          (if old-id
              `(begin
                 (set! ,name-sc
                       ;; This will cause needless double compilation
                       (let ((var ,mod-sc))
                         (js-module-id-set! var ',old-id))))
              `(define ,name-sc ,mod-sc))))
      (cdr form)))))

(js-define-macro (with-module mod body)
  (let ((mod-id (js-module-id mod)))
    (for-each (lambda (id)
                (set! body
                      `(js-alpha-convert
                        ,(car id)
                        (: _ ,mod-id ,(cdr id))
                        ,body)))
              (js-module-symbols mod)))
  body)

(js-define-macro (js-mangle-name sym)
  (string-append
   "_."
   (symbol->string (car (*current-module*)))
   "."
   (let ((p (assq sym
                  (cdr (*current-module*)))))
     (symbol->string
      (if p
          (cdr p)
          sym)))))

(js-define-macro (with-modules mods body)
  (for-each (lambda (mod)
              (set! body
                    `(with-module ,mod
                                  ,body)))
            mods)
  body)

(define (js-code deps code)
  (car
   (js-compile
    `(with-modules
      ,deps
      ,code))))


;; Core language macros

(js-define-macro (define . args)
  `(set! ,@(module#transform-to-lambda args)))

(js-define-macro (define-macro . args)
  (let* ((v (module#transform-to-lambda args))
         (name (car v))
         (val (cadr v)))
    (js-add-macro name (eval val))
    `(begin)))

; This macro is for creating javascript functions. There are some quirks
; regarding functions and scoping, so the use of this HIGHLY recommended
; in favor of creating own function(){} syntax expressions.
;
; See lambda and the non-optimized version of let.
;
; There are two js-scope expressions in this macro. The first is just to
; "protect" the parameters from alpha conversion, the second is both to
; protect from alpha conversion and to add the appropriate var ...;
; declarations on the right place. (ie, within the function definition)
(js-define-macro (statement-wrapper vars . body)
  (let* ((gs (map (lambda (x) (js-gensym)) vars))
         (body ; For each expression in the body, add alpha conversion
          (map (lambda (res)
                 (for-each (lambda (v g)
                             (set! res
                                   `(js-alpha-convert ,v ,g ,res)))
                           vars
                           gs)
                 res)
               body)))
    `(js-syntax (js-scope ,(list vars)
                          (js "(function(")
                          ,@(join '(js ",") gs)
                          (js "){"))
                (js-scope ,(list vars)
                          ,@body
                          (js "})")))))

(js-define-macro (lambda vars . body)
  `(statement-wrapper
    ,vars
    (js "return ")
    (begin ,@(transform-forms-to-letrec body))))

(js-define-macro (trylet a #!optional name block)
  (let ((gs (js-gensym)))
    `(js-syntax
      (statement-wrapper
       ()
       (js "try{return ")
       ,a
       (js "}catch(")
       ,gs
       (js"){")
       ,@(if name
             `((js "return ")
               (js-alpha-convert
                ,name
                ,gs
                ,block))
             '())
       (js "}"))
      (js "()"))))

(js-define-macro (try a b)
  (let ((sym (js-gensym)))
    `(trylet ,a
             ,sym
             (js-syntax ,b (js "(" ,(symbol->string sym) ")")))))

(js-define-macro (throw a)
  `((statement-wrapper () (js "throw ") ,a)))

;; Non-optimized let-macro. I keep it for reference and/or debugging
(js-define-macro (let vars . body)
  `((lambda ,(map car vars) (begin ,@body))
    ,@(map cadr vars)))

; Let-macro implemented with alpha-conversion
(js-define-macro (let vars . body)
  (if (symbol? vars) ; Let loop
      `(letrec ((,vars (lambda ,(map car (car body))
                         ,@(cdr body))))
         (,vars ,@(map cadr (car body))))
      (if (null? (*js-scope-env*))
          `((lambda ,(map car vars) (begin ,@body))
            ,@(map cadr vars))
          (let ((syms (map (lambda (x) (js-gensym)) vars)))
            `(begin
               ,@(map (lambda (v gs)
                        (set! body
                              (js-add-alpha body (car v) gs))
                        (append! (*js-scope-env*) (list gs))
                        `(set! ,gs ,(cadr v)))
                      vars syms)
               ,@(transform-forms-to-letrec body))))))

(js-define-macro (letrec vars . body)
  (if (null? (*js-scope-env*))
      ;; This is to hinder the letrec from leaking scope.
      `((lambda ()
          (letrec ,vars ,@body)))
      (let* ((alphas '())
             (syms (map (lambda (v)
                          (let ((gs (js-gensym)))
                            (set! alphas
                                  (cons (cons (car v) gs)
                                        alphas))
                            gs))
                        vars))
             (ret
              `(begin
                 ,@(map (lambda (v gs)
                          (set! body
                                (js-add-alpha body (car v) gs))
                          (append! (*js-scope-env*) (list gs))
                          (let ((expr (cadr v)))
                            (for-each (lambda (a)
                                        (set! expr
                                              `(js-alpha-convert
                                                ,(car a)
                                                ,(cdr a)
                                                ,expr)))
                                      alphas)
                            `(set! ,gs ,expr)))
                        vars syms)
                 ,@body)))
        ret)))

(js-define-macro (let* vars . body)
  (if (null? vars)
      `(begin ,@body)
      `(let (,(car vars))
         (let* ,(cdr vars)
           ,@body))))

(js-define-macro (ref var . args)
  `(js-syntax ,var
              ,@(map (lambda (p)
                       `(js-syntax (js "[") ,p (js "]")))
                     args)))

(js-define-macro (quote expr)
  (cond
   ((symbol? expr) `(js-syntax ,(symbol->string expr)))
   ((null? expr) '(js "null"))
   (else (error "Invalid argument supplied to quote" expr))))

(js-define-macro (obj . vars)
  `(js-syntax (js "{")
              ,@(join '(js ",")
                      (map (lambda (p)
                             `(js-syntax ,(symbol->string (car p))
                                         (js ":")
                                         ,(cdr p)))
                           (pairify vars)))
              (js "}")))

(js-define-macro (begin . exprs)
  (cond
   ((null? exprs) '(void 0))
   ((null? (cdr exprs))
    (car exprs))
   (else
    `(js-syntax (js "(")
                ,@(join '(js ",") exprs)
                (js ")")))))

(js-define-macro (if cond t #!optional (f '(void 0)))
  `(js-syntax (js "((")
              ,cond
              (js ")?")
              ,t
              (js ":")
              ,f
              (js ")")))

(js-define-macro (cond . clauses)
  (let* ((hd (car clauses))
         (tl (cdr clauses))
         (hd-test (car hd))
         (hd-body (cdr hd)))
    (cond
     ((and (null? tl)
           (eq? hd-test 'else))
      `(begin ,@hd-body))
     
     ((null? tl)
      `(if ,hd-test (begin ,@hd-body)))

     (else
      `(if ,hd-test
           (begin ,@hd-body)
           (cond ,@tl))))))

(js-define-macro (typeof obj)
  `(js-syntax (js "(typeof ")
              ,obj
              (js ")")))

(js-define-macro (instanceof obj type)
  `(js-syntax (js "(")
              ,obj
              (js " instanceof ")
              ,type
              (js ")")))

(js-define-macro (new obj . args)
  `(js-syntax (js "new ")
              ,obj
              (js "(")
              ,@(join '(js ",") args)
              (js ")")))

(js-define-macro (delete obj)
  `((statement-wrapper
     ()
     (js-syntax
      (js "delete ")
      ,obj))))

(js-define-macro (not b)
  `(js-syntax (js "(!(") ,b (js "))")))

; The first argument is expanded to a symbol, the rest to strings.
; This is because the first symbol is actually a variable that should be able
; to be effected by alpha conversion for instance, but the rest shouldn't be
; changed.
(js-define-macro (: first . names)
  `(js-syntax ,first
              ,@(map (lambda (n)
                       `(js "." ,(symbol->string n)))
                     names)))

(js-define-macro (regexp str #!optional (args '||))
  `(js-syntax (js "/")
              (js ,(pregexp-replace*
                    "/"
                    str
                    "\\\\/"))
              (js "/")
              (js ,(symbol->string args))))

(js-define-macro (for-in var obj . body)
  (let ((var-gs (js-gensym)))
    `((statement-wrapper
       ()
       (js-syntax
        (js "for(var ")
        ,var-gs
        (js " in ")
        ,obj
        (js ")")
        (js-alpha-convert ,var
                          ,var-gs
                          (begin ,@body)))))))

(js-define-macro (do vars conds . body)
  (let ((syms (map (lambda (x) (js-gensym)) vars))
        (updates (map (lambda (var)
                        (if (null? (cddr var))
                            `(begin)
                            `(set! ,(car var)
                                   ,(caddr var))))
                      vars))
        (res-gs (js-gensym)))
    `((statement-wrapper
       ()
       (js-syntax
        (js "for(")
        ,@(if (null? vars)
              '()
              '((js "var ")))
        ,@(join '(js ",")
                (map (lambda (var sym)
                       (set! body
                             (js-add-alpha body (car var) sym))
                       (set! conds
                             (js-add-alpha conds (car var) sym))
                       (set! updates
                             (js-add-alpha updates (car var) sym))
                       `(js-syntax
                         (js ,(symbol->string
                               sym))
                         (js "=")
                         ,(cadr var)))
                     vars syms))
        (js ";")
        (js ";")
        (begin
          ,@updates)
        (js "){var ")
        ,res-gs
        (js "=")
        (or ,@conds)
        (js ";if(")
        ,res-gs
        (js ")return ")
        ,res-gs
        (js ";")
        (begin ,@body)
        (js "}"))))))

(js-define-macro (while cond . body)
  `((statement-wrapper
     ()
     (js-syntax
      (js "while(")
      ,cond
      (js ")")
      (begin ,@body)))))

(js-define-macro (do-while cond . body)
  `((statement-wrapper
     ()
     (js-syntax
      (js "do{")
      (begin ,@body)
      (js "}while(")
      ,cond
      (js ")")))))

;; Vectors

;; (js-define-macro (vector-fill! vec o) (TODO))

(define no-fill (gensym))

(js-define-macro (make-vector len #!optional (fill no-fill))
  (if (eq? fill no-fill)
      `(new Array ,len)
      `(let ((arr (new Array len)))
         (vector-fill! arr ,fill)
         arr)))

(js-define-macro (vector . expr)
  `(js-syntax (js "[")
              ,@(join '(js ",")
                      expr)
              (js "]")))

(js-define-macro (vector-length vec)
  `(: ,vec length))

(js-define-macro (vector-ref vec idx)
  `(ref ,vec ,idx))

(js-define-macro (vector-set! vec idx obj)
  `(set! (ref ,vec ,idx) ,obj))

(js-define-macro (vector? vec)
  (let ((gs (js-gensym)))
    `(let ((,gs ,vec))
       (and ,gs (=== (: ,gs constructor) Array)))))


;; Integer increment/decrement

(js-define-macro (incr! o)
  `(js-syntax (js "++") ,o))

(js-define-macro (decr! o)
  `(js-syntax (js "--") ,o))

(js-define-macro (incr-after! o)
  `(js-syntax ,o (js "++")))

(js-define-macro (decr-after! o)
  `(js-syntax ,o (js "--")))

;; Type checks

(js-define-macro (null? v)
  `(=== ,v '()))

(js-define-macro (procedure? p)
  `(equal? (typeof ,p) "function"))

(js-define-macro (number? n)
  `(equal? (typeof ,n) "number"))

(js-define-macro (undefined? o)
  `(equal? (typeof ,o) "undefined"))

;; Strings

(js-define-macro (string? s)
  `(equal? (typeof ,s) "string"))

(js-define-macro (string-append . args)
  `(+ ,@args))

;; Pairs

(js-define-macro (cons a b)
  `(vector ,a ,b))

(js-define-macro (car a)
  `(ref ,a 0))

(js-define-macro (cdr a)
  `(ref ,a 1))

(js-define-macro (set-car! cons val)
  `(set! (: ,cons a) ,val))

(js-define-macro (set-cdr! cons val)
  `(set! (: ,cons b) ,val))

;; Standard libary functions

(js-define-macro (abs v)
  `(Math.abs ,v))

(js-define-macro (cos v)
  `(Math.cos ,v))

(js-define-macro (sin v)
  `(Math.sin ,v))

(js-define-macro (tan v)
  `(Math.tan ,v))

(js-define-macro (acos v)
  `(Math.acos ,v))

(js-define-macro (asin v)
  `(Math.asin ,v))

(js-define-macro (atan v)
  `(Math.asin ,v))

(js-define-macro (exp v)
  `(Math.exp ,v))

(js-define-macro (ceiling v)
  `(Math.ceil ,v))

(js-define-macro (floor v)
  `(Math.floor ,v))

(js-define-macro (log v)
  `(Math.log ,v))

(js-define-macro (sqrt v)
  `(Math.sqrt ,v))

(js-define-macro (min . args)
  `(Math.min ,@args))

(js-define-macro (max . args)
  `(Math.max ,@args))

(js-define-macro (round v)
  `(Math.round ,v))

(js-define-macro (negative? v)
  `(< ,v 0))

(js-define-macro (positive? v)
  `(> ,v 0))

(js-define-macro (zero? v)
  `(=== 0 ,v))

(js-define-macro (odd? v)
  `(modulo ,v 2))

(js-define-macro (even? v)
  `(not (odd? ,v)))

(js-define-macro (apply fun args)
  `((: ,fun apply) this ,args))

(define (js-define-op-with-name op name)
  (js-add-macro name
                (lambda args
                  `(js-syntax (js "(")
                              ,@(join `(js ,(symbol->string op)) args)
                              (js ")")))))

(define (js-define-op op)
  (js-define-op-with-name op op))

(js-define-op '+)
(js-define-op '-)
(js-define-op '*)
(js-define-op '/)
(js-define-op-with-name '% 'modulo)
(js-define-op '=)
(js-define-op '===)
(js-define-op '!==)
(js-define-op '<)
(js-define-op '>)
(js-define-op '<=)
(js-define-op '>=)

(js-define-op-with-name '= 'set!)
(js-define-op-with-name '== 'eq?) ; This is not correct
(js-define-op-with-name '== 'equal?) ; This is not correct
(js-define-op-with-name '== '=)
(js-define-op-with-name '!= 'neq?) ; This is not correct
(js-define-op-with-name '|\|\|| 'or)
(js-define-op-with-name '&& 'and)



;; Utility macros and functions

(define-macro (js-define-utility-with-name scheme-name name)
  (let ((obj (gensym))
        (arg (gensym)))
    `(let ((macro-name ',name))
       (js-define-macro (,scheme-name ,obj . ,arg)
                        `((: ,,obj ,macro-name) ,@,arg)))))

(define-macro (js-define-utility name)
  `(js-define-utility-with-name ,name ,(string-camelize name)))

(js-define-utility join)

;;(js-define-utility each)
;;(js-define-utility all)
;;(js-define-utility map)
;;(js-define-utility find)
;;(js-define-utility each-slice)
;;(js-define-utility-with-name filter findAll)
;;(js-define-utility-with-name remove reject)
;;(js-define-utility grep)
;;(js-define-utility in-groups-of)
;;(js-define-utility any)
;;(js-define-utility include)
;;(js-define-utility-with-name fold inject)
;;(js-define-utility invoke)
;;(js-define-utility max)
;;(js-define-utility min)
;;(js-define-utility partition)
;;(js-define-utility pluck)
;;(js-define-utility size)
;;(js-define-utility sort)
;;(js-define-utility sort-by)

;; Tests

;;(jsp (arr.sort-by size))
;;(jsp (arr.any even?))
;;(jsp (arr.each-slice 4))
;;(jsp (arr.each (lambda (x) (alert x))))
;;(jsp (arr.all even?))
;;(jsp (arr.map even?))
;;(jsp (arr.find even?))
;;(jsp (neq? 5 4))
;; 
;;(jsp (try (alert "hej") (lambda (aa) (alert aa))))
;;(jsp (trylet e (alert "Hej") (alert "aa")))
;; 
;;(jsp (let ((e 5))
;;       (trylet e
;;               (throw 4)
;;               (alert e))))
;; 
;;(jsp (or json.elementName "span"))
;;(jsp (and (eq? 4 3) #f))
;; 
;;(jsp 'hello)
;;(jsp '(a b (c)))
;;(jsp ,(a 5 c 6))
;;(jsp (ref document "all"))
;; 
;;(jsp (alert "Whoah"))
;;(jsp (: ($ "yay") hej))
;;(jsp (alert (if (eq? 5 5) "yes" "noo")))
;;(jsp (not #t))
;;(jsp (begin (alert "Hej") (alert "Då")))
;;(jsp (+ 1 2 (* (+ 1 3) 2)))
;;(jsp (lambda (be ba hje kill) (* be.kill 2)))
;; 
;;(jsp (let ((a 5)) a))
;;(js-expand-macro '(let ((a 5)) a))
;; 
;;(jsp ((lambda (b)
;;       (let ((a (+ 5 b)))
;;         (alert a)
;;         (lambda (a) a))) 5))
;; 
;;(jsp (let ((a 6)) (alert (lambda (a) 5))))
;;(js-expand-macro '(let ((a 6)) (alert (lambda (a) a))))
;; 
;;(jsp (set! a 5))
;;(jsp (define test 8))
;; 
;;(jsp (alert (if (eq? 5 5) "ja!" "neeej")))
;; 
;;(jsp (Widget.create
;;      "label"
;;      (lambda (json)
;;        (.text.tieInnerText
;;         (.createElement (or json.elementName "span") "wgt_label")))
;;      '("text")))
