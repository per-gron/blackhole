;; Basic tests
(eq? (expand-macro 4) 4)

(eq? ((eval '(lambda (a b) (+ a b))) 4 5)
     9)

(expand-macro '(define a #t))

(eq? (eval '(let ((a 1)
                  (b 2)
                  (c 3))
              (+ a b c)))
     6)

(eval '(let ((a #f))
         (let ((a #t))
           a)))

;; Test nested scope usage
(eq? (eval
      '(let ((a 1))
         (let ((b 2))
           (+ a b))))
     3)

;; Test scope priority (is the macro or the define more important?)
;; This should return #t
(eval
 '(let-syntax
      ((test (syntax-rules ()
               ((test) #f))))
    (let ((test (lambda () #t)))
      (test))))

;; Test macro invocations with syntactic closures as the name
(eval
 (expand-macro
  `(let-syntax ((mac (syntax-rules () ((mac) #t))))
     ,(capture-syntactic-environment
       (lambda (env)
         (let ((mac (make-syntactic-closure env '() 'mac)))
           `(,mac)))))))

;; Test let's independence
(eq? (eval
      '(let ((a 2))
         (let ((a 1)
               (b a))
           (+ a b))))
     3)

;; Test let's independence with syntactic closures as arguments
(eval
 `(let ((a #t))
    ,(capture-syntactic-environment
      (lambda (env)
        (let ((a (make-syntactic-closure env '() 'a)))
          `(let ((a #f)
                 (,a ,a))
             ,a))))))

;; Test let loops
(eval '(let loop ((a #f))
         (if a
             (loop a)
             #t)))

;; Test letrec's non-independence
(eq? (eval '(letrec ((a 1)
                     (b (lambda () a)))
              (+ a (b))))
     2)

;; Test letrec's non-independence with syntactic closures as arguments
(eval
 `(let ((a (lambda () #f)))
    ,(capture-syntactic-environment
      (lambda (env)
        (let ((a (make-syntactic-closure env '() 'a)))
          `(letrec ((,a #t)
                    (fun (lambda () ,a)))
             (fun)))))))

;; Test syntactic closures in lets. I seriously don't know what the
;; result of this should be. The hot spot in this test is the
;; syntactic closure that is most deeply nested; should it refer to
;; the a that is 3, or should it refer to the a in the top that is #t?
;;
;; The code that decides which one it should be is expand-synclosure,
;; the part of the cond that takes care of syntactic capture. If that
;; function clones the environment, the answer is the top level a,
;; otherwise the inner a.
(expand-macro
 `(let ((a #f))
    ,(capture-syntactic-environment
      (lambda (e)
        (capture-syntactic-environment
         (lambda (env)
           (let ((a (make-syntactic-closure env '() 'a)))
             `(begin
                ,a
                (let ((a 4)
                      (,a 3))
                  ,(make-syntactic-closure e '() 'a)
                  a
                  ,a)
                ,a))))))))

;; The same test, but for lambdas
(expand-macro
 `(let ((a #f))
    ,(capture-syntactic-environment
      (lambda (e)
        (capture-syntactic-environment
         (lambda (env)
           (let ((a (make-syntactic-closure env '() 'a)))
             `(begin
                ,a
                (lambda (a ,a)
                  ,(make-syntactic-closure e '() 'a)
                  a
                  ,a)
                ,a))))))))

;; Non-DSSSL rest parameters in lambda
(equal? ((eval '(lambda x x)) 1 2 3)
        '(1 2 3))

;; DSSSL rest parameters in lambda
(equal? ((eval '(lambda (x #!rest rest) rest)) 1 2 3)
        '(2 3))

;; Optional parameters in lambda
(eq? ((eval '(lambda (x #!optional opt) opt)) 1 2)
     2)

;; Same as above
(eq? ((eval '(lambda (x #!optional opt) opt)) 1)
     #f)

;; Key parameters in lambda
(eq? ((eval '(lambda (x #!key key) key)) 1 key: 2)
     2)

;; Non-DSSSL rest parameter as syntactic closures in lambda
(car ((eval
       (capture-syntactic-environment
        (lambda (env)
          `(lambda ,(make-syntactic-closure env '() 'a)
             a))))
      #t))

;; Optional parameters as syntactic closures in lambda
(eq? ((eval
       (capture-syntactic-environment
        (lambda (env)
          (let ((sc (make-syntactic-closure env '() 'a)))
            `(lambda (a #!optional ,sc)
               (+ a ,sc))))))
      2 3)
     5)

;; Rest parameters as syntactic closures in lambda
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    `(lambda (a #!rest ,(make-syntactic-closure env '() 'a))
       a))))

;; Key parameters as syntactic closures in lambda
(expand-macro
 `(let ((a #f))
    ,(capture-syntactic-environment
      (lambda (env)
        (let ((a (make-syntactic-closure env '() 'a)))
          `(lambda (a #!key ,a)
             ,a))))))

(expand-macro
 `(let ((ifa 3))
    ,(make-syntactic-closure
      build#empty-environment
      '()
      'ifa)))

(expand-macro
 (capture-syntactic-environment
  (lambda (e)
    `(let ((a 3))
       ,(make-syntactic-closure e '() 'a)))))

(expand-macro
 `(let ((a 3))
    ,(capture-syntactic-environment
      (lambda (e)
        `(let ((a 4))
           ,(make-syntactic-closure e '() 'a))))))

(expand-macro
 '(let-syntax ((when (sc-macro-transformer
                      (lambda (form env)
                        `(if ,(make-syntactic-closure env '() (cadr form))
                             (begin
                               ,@(cddr
                                  (map (lambda (x)
                                         (make-syntactic-closure env '() x))
                                       form))))))))
    (let ((if #t))
      (when if (set! if 'now))
      if)))

(expand-macro
 '(let ()
    (define (a) 4)
    (a)))

(expand-macro
 '(letrec ()
    (define (a) 4)
    (a)))

(expand-macro
 '(define (x)
    (define (a) 4)
    (a)))

(expand-macro
 '(let* ((a 4)
         (b 5))
    (define (c) 4)
    (c)))

(expand-macro
 '(let-syntax ((test-a (sc-macro-transformer
                        (lambda (form env)
                          `(if (text-a) "Yay"))))
               (if (sc-macro-transformer
                    (lambda (form env)
                      `(nono))))
               (test-b (sc-macro-transformer
                        (lambda (form env)
                          `(if (test-a) "Yay")))))
    (test-a)
    (test-b)))

(expand-macro
 '(let ((x 'outer))
    (let-syntax ((m (sc-macro-transformer
                     (lambda (form env)
                       'x))))
      (let ((x 'inner))
        (m)))))

(expand-macro
 '(let ((=> #f))
    (cond (#t => 'ok))))

;; Test macro recursion with letrec-syntax
(eval
 '(letrec-syntax ((test
                   (sc-macro-transformer
                    (lambda (form env)
                      (if (null? (cdr form))
                          #t
                          `(test))))))
    (test 'test)))


;; Basic define-macro
(eval
 '(let ()
    (define-macro (hej) #t)
    (hej)))


(eq? 3
     (eval
      (expand-macro
       '(let ()
          (define-syntax hej (sc-macro-transformer
                              (lambda _ 3)))
          (hej)))))

;; Test define-syntax within a transform-to-letrec scope
;; where a define uses a macro.
(eq? 4
     (eval
      (expand-macro
       '(let ()
          (define-syntax test
            (syntax-rules ()
              ((test) 4)))
          
          (define hej (test))
          
          hej))))

;; Pretty much same as above, but with some more quirks
;; to test for double-colorings.
(eq? 5 ((eval
         (expand-macro
          '(let ()
             (define-syntax test
               (syntax-rules ()
                 ((test) 3)
                 
                 ((test var) (lambda (x) (+ x var)))))
             
             (define (hej var) (test var))
             
             (hej (test)))))
        2))

;; Basic test for identifier=?
(let ((e build#empty-environment))
  (identifier=? e 'a e 'a))

;; Basic test for identifier=?
(not (let ((e build#empty-environment))
       (identifier=? e 'a e 'b)))

(equal? '(#t #f)
        (let-syntax
            ((foo
              (sc-macro-transformer
               (lambda (form env)
                 (capture-syntactic-environment
                  (lambda (transformer-env)
                    (identifier=? transformer-env 'x
                                  env 'x)))))))
          (list (foo)
                (let ((x 3))
                  (foo)))))



(eq? 1
     (eval
      (expand-macro
       '(let-syntax ((swap
                      (syntax-rules ()
                        ((swap a b) (let ((tmp b))
                                      (set! b a)
                                      (set! a tmp))))))
          (let ((one 1)
                (two 2))
            (swap one two)
            two)))))

(expand-macro
 '(let-syntax ((test (syntax-rules (a)
                       ((test a) 'a!)
                       ((test 5) 'five!)
                       ((test 5 var) var)
                       ((test) 'Hoo)
                       ((test other ...) 'x))))
    (test a)
    (test 5 'hej)
    (let ((a #f))
      (test a)
      (test 5))))

(eq? 'YeY
     (eval
      (expand-macro
       '(let-syntax ((test (syntax-rules ()
                             ((test) 'YeY))))
          (test)))))

(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test var)
                   (haha var)))))
    (test 4)))

(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test (a b) ...)
                   (begin (+ a b) ...)))))
    (test (1 2) (3 4))))

(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test var hej ...)
                   (begin (var hej) ... end)))))
    (test 1 2 3 4 5)))

;; Test that syntax-rules ... rules can take empty parameters
(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test a ...)
                   (begin a ...)))))
    (test)
    #f))

(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test (a . b) c)
                   b))))
    (test (hej alla idioter) 44)))

(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test (a ...) (b ...))
                   (begin (+ a b) ...)))))
    (test (1 2) (3 4))))

;; This really should give an error. At least it doesn't crash.
(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test (a ...) (b ...))
                   (begin (+ a b) ...)))))
    (test (1 2) (3 4 5))))

(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test (a ...) (b ...))
                   (+ a ... b ...)))))
    (test (1 2) (3 4 5))))

(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test (a ...) ...)
                   '((a ...) ...)))))
    (test (a) (b c d))))

(expand-macro
 '(let-syntax ((test
                (syntax-rules ()
                  ((test (_ val ...) ...)
                   '((val ...) ...)))))
    (test (1 2 3) (4 5))))

;; Test letrec-syntax
(expand-macro
 '(letrec-syntax ((test
                   (sc-macro-transformer
                    (lambda (form env)
                      (if (eq? (cadr form) 1)
                          ''YaY
                          `(test ,(- (cadr form) 1)))))))
    (test 10)))

;; Test letrec-syntax and syntax-rules together
(expand-macro
 '(letrec-syntax ((test
                   (syntax-rules ()
                     ((test 3)
                      (test 2))
                     ((test 2)
                      (test 1))
                     ((test 1)
                      'YaY))))
    (test 3)))


;; Test syntactic closures within quasiquotes. (Previously there was a
;; bug that made this return h1#x, because code inside quasiquotes
;; that was within syntactic closures would get macroexpanded like
;; normal code and not as if it was inside a quote.
(eq? 'x
     (eval
      (expand-macro
       `(let ((x #f))
          ,(capture-syntactic-environment
            (lambda (e)
              (list 'quasiquote
                    (make-syntactic-closure e '() 'x))))))))


;; This is REALLY a corner case
;; http://groups.google.com/group/comp.lang.scheme/msg/eb6cc6e11775b619
;; says that this should return 2, but this system and the system of
;; SISC returns 1.
(let ((x 1))
  (let-syntax
      ((foo (syntax-rules ()
              ((_ y)
               (let-syntax
                   ((bar (syntax-rules ()
                           ((_) (let ((x 2)) y)))))
                 (bar))))))
    (foo x)))

;; This is REALLY a corner case
;; http://groups.google.com/group/comp.lang.scheme/msg/eb6cc6e11775b619
;; says that this should return 2, but this system and the system of
;; SISC returns 1.
(let ((x 1))
  (let-syntax
      ((foo (syntax-rules ()
              ((_ y)
               (let-syntax
                   ((bar (syntax-rules ()
                           ((_ x) y))))
                 (bar 2))))))
    (foo x)))

;; SISC supports this syntax. My interpretation of R5RS
;; supports this syntax. This implementation does.
;; Gauche does not, however.
(expand-macro
 '(let-syntax
      ((test (syntax-rules ()
               ((test (a ...) ...)
                (+ a ... ...)))))
    (test (1 2) (3 4))))

;; Test syntax-rules that leaks scope
(expand-macro
 '(let-syntax
      ((test (syntax-rules ()
               ((test a form)
                (let ((a 1))
                  form)))))
    (test c (+ c c))))

;; Same as above, but with lambda
(expand-macro
 '(let-syntax
      ((test (syntax-rules ()
               ((test a form)
                (lambda (a)
                  form)))))
    (test c (+ c c))))

;; As a counter-test to the two above, test
;; syntax-rules that doesn't leak scope.
(expand-macro
 '(let-syntax
      ((test (syntax-rules ()
                ((test form)
                (let ((a 5))
                  (+ a form))))))
    (let ((a 1))
      (test (+ a a)))))

;; Test mutually recursive functions
;; defined with define within a scope
(expand-macro
 '(let ()
    (define (a) (b))
    (define (b) (a))
    (a)))

;; Test syntax-rules macros that take
;; syntactic closures as parameters where
;; the pattern has to parse it.
(expand-macro
 '(letrec-syntax ((one (syntax-rules ()
                         ((one (a b))
                          (+ a b))))
                  (two (syntax-rules ()
                         ((two x)
                          (one x)))))
    (two (1 2))))

;; Test let with the parameter list as syntactic closure
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    `(let ,(make-syntactic-closure env '() '((a 3)))
       #f))))

;; Test let* with the parameter list as syntactic closure
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    `(let* ,(make-syntactic-closure env '() '((a 3)))
       #f))))

;; Test let loop with name as syntactic closure
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    (let ((loop (make-syntactic-closure env '() 'loop)))
      `(let ,loop ()
            loop
            ,loop)))))

;; Test let with one parameter as syntactic closure
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    `(let (,(make-syntactic-closure env '() '(a 3)))
       #f))))

;; Test lambda with the parameter list as syntactic closure
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    `(lambda ,(make-syntactic-closure env '() '(a))
       a
       ,(make-syntactic-closure env '() 'a)))))

;; Test lambda with defaulting parameter name as syntactic closure
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    `(lambda (#!optional (,(make-syntactic-closure env '() 'a) 5))
       #f))))

;; Test lambda with a defaulting parameter as a syntactic closure
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    `(lambda (#!optional ,(make-syntactic-closure env '() '(a 5)))
       #f))))

;; Test lambda with a defaulting parameter
;; that macro-expands
(expand-macro
 '(lambda (#!optional (x (let ((a 4)) (+ a a)))) x))

;; Test transform-to-lambda with a syntactic closure
;; as parameter
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    `(define ,(make-syntactic-closure env '() '(fun args))
       #f))))

;; Test define with a syntactic closure as name
(expand-macro
 (capture-syntactic-environment
  (lambda (env)
    (let ((x (make-syntactic-closure env '() 'x)))
    `(let ((x #t))
       (define ,x #f)
       x
       ,x)))))

;; Test macros that defines macros
(eval
 '(let ()
    (define-macro (one)
      `(define-macro (two) #t))
    (one)
    (two)))

;; Test defining macros inside of begins
(eval
 '(let ()
    (begin
      (define-syntax one
        (syntax-rules ()
          ((one)
           #t)))
      (one))))

;; Test macros that define macros and use them
(eval
 '(let ()
    (define-syntax one
      (syntax-rules ()
        ((one)
         (begin
           (define-syntax two
             (syntax-rules ()
               ((two) #t)))
           (two)))))
    (one)))

;; Same as above, but with define-macro
(eval
 '(let ()
    (define-macro (one)
      `(begin
         (define-macro (two) #t)
         (two)))
    (one)))

;; I had problems with this snippet of code; the expression
;; (one test) got expanded to (one h1#test) before the invocation
;; of the macro, which yielded an incorrect return value of
;; 'h1#test.
(expand-macro
 '(let ((test #f))
    (define-macro (one x)
      `',x)
    (one test)))


;; Test macros that define variables
;; TODO This doesn't work
(expand-macro
 '(let ((test #f))
    (let-syntax
        ((one
          (nh-macro-transformer
           (lambda ()
             `(define test #t)))))
      (one)
      test)))

;; Test macros that define variables that shouldn't leak
;; TODO This doesn't work
(expand-macro
 '(let ((test #t))
    (let-syntax
        ((one
          (syntax-rules ()
            ((one) (begin
                     (define test #f)
                     #f)))))
      (one)
      test)))

;; A more intricate test of mutually recursive macros
(eval
 '(let ()
    (define a #t)
    (define (b) (two #f))
    (define-macro (one n) (if n 'a '(two #t)))
    (define-macro (two n) `(one ,n))
    (b)))

;; Test to define top-level stuff inside a scope
(expand-macro
 '(let-syntax
      ((one (syntax-rules ()
              ((one) (define x #f)))))
    (let ((x #t))
      (one)
      x)))

;; Both references to x should be to the later one.
(expand-macro
 `(let ((x #f))
    (begin
      (define x #t)
      x)
    x))

;; This should return #t
(eval
 `(let ((x #f))
    ,(capture-syntactic-environment
      (lambda (inner-env)
        `(define ,(make-syntactic-closure inner-env '() 'x)
           #t)))
    x))

;; Test begins not within the top level and inside
;; something else than a let/letrec block
(let-syntax
    ((one
      (syntax-rules ()
        ((one body1 ...)
         (if #t (begin body1 ...))))))
  (one #t))

;; Test syntactic closures in head position on macro
;; invocations. (This expression should return #t)
(eval
 (capture-syntactic-environment
  (lambda (env)
    `(let ((define (lambda args #f))
           (a #f))
       (,(make-syntactic-closure env '() 'define) a #t)
       a))))

;; Test define-syntax that overrides another variable within a
;; scope. (This expression should return #t)
(eval
 `(let ((test (lambda () #f)))
    (define-syntax test
      (syntax-rules ()
        ((test) #t)))
    (test)))

;; Test to define a macro using define-syntax with a syntactic closure
;; where define-syntax is shadowed. (This expression should return #t)
(eval
 `(let ((define-syntax (lambda args #f))
        (test (lambda () #f)))
    (,(make-syntactic-closure build#builtin-environment
                              '()
                              'define-syntax)
     test
     (sc-macro-transformer
      (lambda (form env)
        #t)))
    (test)))

;; Test that inner ns macros that expand into #f actually get
;; expanded. (Yes, I had problems with this once)
(not
 (eval
  '(let-syntax ((test (syntax-rules ()
                        ((test) #f))))
     (test))))

;; Don't remember what this test was for..
(expand-macro
 '(let ()
    (define-macro (one)
      `(define (fun) 'x))
    (one)
    (define (two) 'two)
    (two)))

;; This should expand into a non-macro expanded form
(equal? ''(lambda (x) #f)
        (expand-macro
         (capture-syntactic-environment
          (lambda (env)
            `',(make-syntactic-closure env '() '(lambda (x) #f))))))

;; This should expand into a non-macro expanded form
(equal? ''(lambda (x) #f)
        (expand-macro
         (capture-syntactic-environment
          (lambda (env)
            (make-syntactic-closure env '() ''(lambda (x) #f))))))

;; Test that quotes' contents don't get expanded
(equal? '(##begin '(lambda (x) #t))
        (expand-macro
         '(letrec-syntax
              ((one
                (syntax-rules ()
                  ((one a) 'a)))
               (two
                (syntax-rules ()
                  ((two a) (one a)))))
            
            (two (lambda (x) #t)))))

;; Test that define-syntax can take syntactic closures as the first
;; parameter.
(begin
  (expand-macro
   `(define-syntax
      ,(make-syntactic-closure (build#top-environment)
                               '()
                               'xx)
      (syntax-rules ()
        ((xx) #t))))
  ;; If this test fails, it will already have generated an error by
  ;; now.
  #t)

;; This once resulted in an infinite loop. It is a more intricate test
;; for define-syntax' ability to take syntactic closures as first
;; parameter.
(let-syntax
    ((mac
      (syntax-rules ()
        ((mac name)
         (define-syntax name
           (syntax-rules ()
             ((name) 'name)))))))
  (mac xx)
  (eq? (xx) 'xx))

;; Test that let-syntax can take syntactic closures as names of the
;; macros.
(eval
 (expand-macro
  `(let ((xx (lambda () #t)))
     (let-syntax
         ((,(make-syntactic-closure (build#top-environment)
                                    '()
                                    'xx)
           (syntax-rules ()
             ((xx) #f))))
       (xx)))))





;; This currently doesn't work; The xx inside the syntax-rules macro
;; leaks.
(let-syntax
    ((mac
      (syntax-rules ()
        ((mac)
         (define-syntax xx
           (syntax-rules ()
             ((xx) #f)))))))
  (define-syntax xx
    (syntax-rules ()
      ((xx) #t)))
  (mac)
  (xx))

;; When evaluating this, whoa gets exported as macro to the top
;; level. (See above) I think this is related to the fact that top
;; level stuff is stored with a hashtable that is not cloned when
;; making sub-environments. That is: An incorrent assumption that
;; there is only one top level per environment tree.
(expand-macro
 (make-syntactic-closure
  (build#make-environment (build#top-environment))
  '()
  '(define-syntax whoa
     (syntax-rules ()
       ((whoa) #t)))))
