(begin ;; Helper functions. Enclosed in a begin to make it return #t
       ;; and thus not result in a "failed test"
  (define (expect-exception thunk)
    (with-exception-catcher
     (lambda (e)
       #t)
     (lambda ()
       (thunk)
       #f)))
  
  #t)

;; Basic tests
(eq? (expand-macro 4) 4)

(eq? ((eval '(lambda (a b) (+ a b))) 4 5)
     9)

(or #t (expand-macro '(define a #t)))

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

;; Test unquote after dot
(eq? 10
     (let ((a (eval (expand-macro '`(5 . ,5)))))
       (+ (car a) (cdr a))))

;; Test scope priority (is the macro or the define more important?)
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
 `(let ((a #f))
    ,(capture-syntactic-environment
      (lambda (env)
        (let ((a (make-syntactic-closure env '() 'a)))
          `(letrec ((,a #t)
                    (fun (lambda () ,a)))
             (fun)))))))

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
((eval
  (capture-syntactic-environment
   (lambda (env)
     `(lambda (a #!rest ,(make-syntactic-closure env '() 'a))
        a))))
 #t
 #f)

;; Key parameters as syntactic closures in lambda
((eval
  `(let ((a #f))
     ,(capture-syntactic-environment
       (lambda (env)
         (let ((a (make-syntactic-closure env '() 'a)))
           `(lambda (a #!key ,a)
              ,a))))))
 #f
 a: #t)

(expect-exception
 (lambda ()
   (eval
    `(let ((ifa 3))
       ,(make-syntactic-closure
         bh#empty-environment
         '()
         'ifa)))))

(eq? 5
     (eval
      `(let ((a 5))
         ,(capture-syntactic-environment
           (lambda (e)
             `(let ((a #f))
                ,(make-syntactic-closure e '() 'a)))))))

;; Basic syntactic closure test
(eval
 `(let ((a #t))
    ,(capture-syntactic-environment
      (lambda (env)
        (let ((a (make-syntactic-closure env '() 'a)))
          `(let ((a #f))
             ,a))))))


(eq? 'now
     (eval
      '(let-syntax
           ((when (sc-macro-transformer
                   (lambda (form env)
                     `(if ,(make-syntactic-closure env '() (cadr form))
                          (begin
                            ,@(cddr
                               (map (lambda (x)
                                      (make-syntactic-closure env '() x))
                                    form))))))))
         (let ((if #t))
           (when if (set! if 'now))
           if))))

(let ()
  (define (a) #t)
  (a))

(letrec ()
  (define (a) #t)
  (a))

(begin
  (define (x)
    (define (a) #t)
    (a))
  (x))

(let* ((a 4)
       (b 5))
  (define (c) #t)
  (c))

(let-syntax ((test-a (sc-macro-transformer
                      (lambda (form env)
                        `(if #t #t))))
             (if (sc-macro-transformer
                  (lambda (form env)
                    #f)))
             (test-b (sc-macro-transformer
                      (lambda (form env)
                        `(if #t #t)))))
  (and (test-a) (test-b)))

(eq? 'outer
     (let ((x 'outer))
       (let-syntax ((m (sc-macro-transformer
                        (lambda (form env)
                          'x))))
         (let ((x 'inner))
           (m)))))

(eq? 'ok
     (let ((=> #f))
       (cond (#t => 'ok))))

;; Test that let-syntax bindings don't interfere with each other.
(let ((a (lambda () #t)))
  (let-syntax
      ((a (syntax-rules () ((_) #f)))
       (b (syntax-rules () ((_) (a)))))
    (b)))

;; Test that letrec-syntax bindings do interfere with each other.
(letrec-syntax
    ((a (syntax-rules () ((_) #f)))
     (b (syntax-rules () ((_) (a)))))
  (not (b)))

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
      '(let ()
         (define-syntax hej (sc-macro-transformer
                             (lambda _ 3)))
         (hej))))

;; Test define-syntax within a transform-to-letrec scope
;; where a define uses a macro.
(eq? 4
     (eval
      '(let ()
         (define-syntax test
           (syntax-rules ()
             ((test) 4)))
         
         (define hej (test))
         
         hej)))

;; Pretty much same as above, but with some more quirks
;; to test for double-colorings.
(eq? 5 ((eval
         '(let ()
            (define-syntax test
              (syntax-rules ()
                ((test) 3)
                
                ((test var) (lambda (x) (+ x var)))))
            
            (define (hej var) (test var))
            
            (hej (test))))
        2))

;; Basic test for identifier=?
(let ((e bh#empty-environment))
  (identifier=? e 'a e 'a))

;; Basic test for identifier=?
(not (let ((e bh#empty-environment))
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

;; Test of identifier=?
(let-syntax ((test (syntax-rules (a)
                     ((test a) #t)
                     ((test x) #f))))
  (test a))



(eq? 1
     (eval
      '(let-syntax ((swap
                     (syntax-rules ()
                       ((swap a b) (let ((tmp b))
                                     (set! b a)
                                     (set! a tmp))))))
         (let ((one 1)
               (two 2))
           (swap one two)
           two))))

(equal? '(a! hej (x . five!))
        (let-syntax ((test (syntax-rules (a)
                             ((test a) 'a!)
                             ((test 5) 'five!)
                             ((test 5 var) var)
                             ((test) 'Hoo)
                             ((test other ...) 'x))))
          (list (test a)
                (test 5 'hej)
                (let ((a #f))
                  (cons (test a)
                        (test 5))))))

(eq? 'YeY
     (eval
      (expand-macro
       '(let-syntax ((test (syntax-rules ()
                             ((test) 'YeY))))
          (test)))))

(let ((haha (lambda (x) (eq? x 4))))
  (let-syntax ((test
                (syntax-rules ()
                  ((test var)
                   (haha var)))))
    (test 4)))

(equal? '(3 7)
        (let-syntax ((test
                      (syntax-rules ()
                        ((test (a b) ...)
                         (list (+ a b) ...)))))
          (test (1 2) (3 4))))

(equal? '(3 4 5 6 end)
        (let-syntax ((test
                      (syntax-rules ()
                        ((test var hej ...)
                         (list (+ var hej) ... 'end)))))
          (test 1 2 3 4 5)))

;; Test syntax-rules vector patterns
(letrec-syntax
    ((test (syntax-rules ()
             ((test #(1 1)) #f)
             ((test #(1 2)) #t)
             ((test _) #f))))
  (test #(1 2)))

;; Test syntax-rules vector patterns
(equal? '#(1 #(1 2))
          (letrec-syntax
              ((test (syntax-rules ()
                       ((test a) '#(1 a)))))
            (test #(1 2))))

;; Test syntax-rules vector patterns
(eq? 2
     (letrec-syntax
         ((test (syntax-rules ()
                  ((test #(1 a)) (test a))
                  ((test 2) 2)
                  ((test _) #f))))
       (test #(1 2))))

;; Test that syntax-rules ... rules can take empty parameters
(let-syntax ((test
              (syntax-rules ()
                ((test a ...)
                 (begin a ...)))))
  (test)
  #t)

(equal? '(alla idioter)
        (let-syntax ((test
                      (syntax-rules ()
                        ((test (a . b) c)
                         'b))))
          (test (hej alla idioter) 44)))

(equal? '(4 6)
        (let-syntax ((test
                      (syntax-rules ()
                        ((test (a ...) (b ...))
                         (list (+ a b) ...)))))
          (test (1 2) (3 4))))

;; This really should give an error. At least it doesn't crash.
(equal? '(4 6)
        (let-syntax ((test
                      (syntax-rules ()
                        ((test (a ...) (b ...))
                         (list (+ a b) ...)))))
          (test (1 2) (3 4 5))))

(equal? '(1 2 3 4 5)
        (let-syntax ((test
                      (syntax-rules ()
                        ((test (a ...) (b ...))
                         (list a ... b ...)))))
          (test (1 2) (3 4 5))))

(equal? '((a) (b c d))
        (let-syntax ((test
                      (syntax-rules ()
                        ((test (a ...) ...)
                         '((a ...) ...)))))
          (test (a) (b c d))))

(equal? '((2 3) (5))
        (let-syntax ((test
                      (syntax-rules ()
                        ((test (_ val ...) ...)
                         '((val ...) ...)))))
          (test (1 2 3) (4 5))))

;; Test do
(equal? 25
        (let ((x '(1 3 5 7 9)))
          (do ((x x (cdr x))
               (sum 0 (+ sum (car x))))
              ((null? x) sum))))

(equal? '#(0 1 2 3 4)
        (do ((vec (make-vector 5))
             (i 0 (+ i 1)))
            ((= i 5) vec)
          (vector-set! vec i i)))

;; Test letrec-syntax
(eq? 'YaY
     (letrec-syntax ((test
                      (sc-macro-transformer
                       (lambda (form env)
                         (if (eq? (cadr form) 1)
                             ''YaY
                             `(test ,(- (cadr form) 1)))))))
       (test 10)))

;; Test letrec-syntax and syntax-rules together
(eq? 'YaY
     (letrec-syntax
         ((test
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
;; says that this should return 1, and SISC returns 1.
(eq? 1
     (let ((x 1))
       (let-syntax
           ((foo (syntax-rules ()
                   ((_ y)
                    (let-syntax
                        ((bar (syntax-rules ()
                                ((_) (let ((x 2)) y)))))
                      (bar))))))
       (foo x))))

;; This is REALLY a corner case
;; http://groups.google.com/group/comp.lang.scheme/msg/eb6cc6e11775b619
;; says that this should return 2, but SISC and Gauche returns 1.
(eq? 2
     (let ((x 1))
       (let-syntax
           ((foo (syntax-rules ()
                   ((_ y)
                    (let-syntax
                        ((bar (syntax-rules ()
                                ((_ x) y))))
                      (bar 2))))))
         (foo x))))

;; SISC supports this syntax. My interpretation of R5RS
;; supports this syntax. This implementation does.
;; Gauche does not, however.
(eq? 10
     (let-syntax
         ((test (syntax-rules ()
                  ((test (a ...) ...)
                   (+ a ... ...)))))
       (test (1 2) (3 4))))

;; Test syntax-rules that leaks scope
(eq? 2
     (let-syntax
         ((test (syntax-rules ()
                  ((test a form)
                   (let ((a 1))
                     form)))))
       (test c (+ c c))))

;; Same as above, but with lambda
(eq? 4
     ((let-syntax
          ((test (syntax-rules ()
                   ((test a form)
                    (lambda (a)
                      form)))))
        (test c (+ c c)))
      2))

;; As a counter-test to the two above, test
;; syntax-rules that doesn't leak scope.
(eq? 7
     (let-syntax
         ((test (syntax-rules ()
                  ((test form)
                   (let ((a 5))
                     (+ a form))))))
       (let ((a 1))
         (test (+ a a)))))

;; Test mutually recursive functions
;; defined with define within a scope
(negative?
 (let ()
   (define (a x)
     (b (- x 1)))
   (define (b x)
     (if (negative? x)
         x
         (a (- x 1))))
   (a 12)))

;; Test syntax-rules macros that take
;; syntactic closures as parameters where
;; the pattern has to parse it.
(eq? 3
     (letrec-syntax ((one (syntax-rules ()
                            ((one (a b))
                             (+ a b))))
                     (two (syntax-rules ()
                            ((two x)
                             (one x)))))
       (two (1 2))))

;; Test let with the parameter list as syntactic closure
(eval
 (expand-macro
  (capture-syntactic-environment
   (lambda (env)
     `(let ,(make-syntactic-closure env '() '((a 3)))
        #t)))))

;; Test let* with the parameter list as syntactic closure
(eval
 (expand-macro
  (capture-syntactic-environment
   (lambda (env)
     `(let* ,(make-syntactic-closure env '() '((a 3)))
        #t)))))

;; Test let loop with name as syntactic closure
(procedure?
 (eval
  (expand-macro
   (capture-syntactic-environment
    (lambda (env)
      (let ((loop (make-syntactic-closure env '() 'loop)))
        `(let ,loop ()
              loop
              ,loop)))))))

;; Test let with one parameter as syntactic closure
(eval
 (capture-syntactic-environment
  (lambda (env)
    `(let (,(make-syntactic-closure env '() '(a 3)))
       (eq? a 3)))))

;; Test lambda with the parameter list as syntactic closure
((eval
  (capture-syntactic-environment
   (lambda (env)
     `(lambda ,(make-syntactic-closure env '() '(a))
        (and a
             ,(make-syntactic-closure env '() 'a))))))
  #t)

;; Test lambda with defaulting parameter name as syntactic closure
((eval
  (expand-macro
   (capture-syntactic-environment
    (lambda (env)
      `(lambda (#!optional (,(make-syntactic-closure env '() 'a) 5))
         #t))))))

;; Test lambda with a defaulting parameter as a syntactic closure
((eval
  (expand-macro
   (capture-syntactic-environment
    (lambda (env)
      `(lambda (#!optional ,(make-syntactic-closure env '() '(a 5)))
         #t))))))

;; Test lambda with a defaulting parameter
;; that macro-expands
(eq? 8
     ((eval
       '(lambda (#!optional (x (let ((a 4)) (+ a a)))) x))))

;; Test transform-to-lambda with a syntactic closure as parameter. If
;; this test fails, it ought to result in an error, not an incorrect
;; result.
(eval
 `(let ()
    ,(capture-syntactic-environment
      (lambda (env)
        `(define ,(make-syntactic-closure env '() '(fun args))
           args)))
    (fun #t)))

;; Test define with a syntactic closure as name
(equal? '(#t . #f)
        (eval
         (capture-syntactic-environment
          (lambda (env)
            (let ((x (make-syntactic-closure env '() 'x)))
              `(let ((x #t))
                 (define ,x #f)
                 (cons x
                       ,x)))))))

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
(eq? 'test
     (eval
      (expand-macro
       '(let ((test #f))
          (define-macro (one x)
            `',x)
          (one test)))))


;; Test macros that define variables
(let ((test #f))
  (let-syntax
      ((one
        (nh-macro-transformer
         (lambda ()
           `(define test #t)))))
    (one)
    test))

;; Another corner case. I don't really know what this should be. SISC
;; returns #f
(let ((test #t))
  (let-syntax
      ((one
        (syntax-rules ()
          ((one) (begin
                   (define test #f)
                   #!void)))))
    (one)
    test))

;; A more intricate test of mutually recursive macros
(eval
 '(let ()
    (define a #t)
    (define (b) (two #f))
    (define-macro (one n) (if n 'a '(two #t)))
    (define-macro (two n) `(one ,n))
    (b)))

;; Test to define top-level stuff inside a scope
(let-syntax
    ((one (syntax-rules ()
            ((one) (define x #f)))))
  (let ((x #t))
    (one)
    x))

;; Both references to x should be to the later one.
(call/cc
 (lambda (k)
   (let ((x (lambda () (k #f))))
     (begin
       (define (x) #t)
       (x))
     (x))))

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
;; invocations.
(eval
 (capture-syntactic-environment
  (lambda (env)
    `(let ((define (lambda args #f))
           (a #f))
       (,(make-syntactic-closure env '() 'define) a #t)
       a))))

;; Test define-syntax that overrides another variable within a
;; scope.
(eval
 `(let ((test (lambda () #f)))
    (define-syntax test
      (syntax-rules ()
        ((test) #t)))
    (test)))

;; Test to define a macro using define-syntax with a syntactic closure
;; where define-syntax is shadowed.
(eval
 (expand-macro
  (capture-syntactic-environment
   (lambda (env)
     `(let ((define-syntax (lambda args #f))
            (test (lambda () #f)))
        (,(make-syntactic-closure env
                                  '()
                                  'define-syntax)
         test
         (sc-macro-transformer
          (lambda (form env)
            #t)))
        (test))))))

;; Test that inner ns macros that expand into #f actually get
;; expanded. (Yes, I had problems with this once)
(not
 (eval
  '(let-syntax ((test (syntax-rules ()
                        ((test) #f))))
     (test))))

;; Test defining a macro that defines a function, and then after that
;; continuing with defining things.
(eq? 'two
     (eval
      (expand-macro
       '(let ()
          (define-macro (one)
            `(define (fun) 'x))
          (one)
          (define (two) 'two)
          (two)))))

;; This should expand into a non-macro expanded form
(equal? ''(lambda (x) #f)
        (expand-macro
         (capture-syntactic-environment
          (lambda (env)
            `',(make-syntactic-closure env '() '(lambda (x) #f))))))

;; This should expand into a non-macro expanded form
(equal? '(lambda (x) #f)
        (eval
         (expand-macro
          (capture-syntactic-environment
           (lambda (env)
             (make-syntactic-closure env '() ''(lambda (x) #f)))))))

;; Test that quotes' contents don't get expanded
(equal? '(lambda (x) #t)
        (eval
         (expand-macro
          '(letrec-syntax
               ((one
                 (syntax-rules ()
                   ((one a) 'a)))
                (two
                 (syntax-rules ()
                   ((two a) (one a)))))
             
             (two (lambda (x) #t))))))

;; Test that define-syntax can take syntactic closures as the first
;; parameter.
(begin
  (expand-macro
   `(define-syntax
      ,(make-syntactic-closure (bh#*top-environment*)
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
         ((,(make-syntactic-closure (bh#*top-environment*)
                                    '()
                                    'xx)
           (syntax-rules ()
             ((xx) #f))))
       (xx)))))

;; This is a quirky case. I don't know exactly what the results of
;; this one should be. SISC returns #t on this one. Gauche gives an
;; error.
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

;; Test macros that generate define-syntax forms that should not leak.
(let-syntax
    ((mac
      (syntax-rules ()
        ((mac)
         (define-syntax xx
           (syntax-rules ()
             ((_) #f)))))))
  (let ((xx (lambda () #t)))
    (mac)
    (xx)))

;; Test macros that generate define forms that should not leak.
(let-syntax
    ((mac
      (syntax-rules ()
        ((mac)
         (define (xx) #f)))))
  (let ((xx (lambda () #t)))
    (mac)
    (xx)))


;; Test that begins get properly expanded always. This is actually a
;; test to make sure that the inside-letrec parameter is handled
;; correctly.
(let ((x #t))
  x
  (begin x))

;; Another inside-letrec related test
(let ()
  (let ((x (begin expand-macro)))
    (eq? x expand-macro)))

;; One more test aimed at inside-letrec correctness
(let ()
  (define-macro (a) #t)
  (a))

;; Test that transform-forms-to-triple doesn't bail on #!void
(let ()
  #!void
  (define a #t)
  a)

;; Basic test for begins in transform-forms-to-triple.
(eq? 6
     (let ((a 2) (b 2))
       (begin
         (define a 3))
       (define b 3)
       (+ a b)))

;; Test that defines work even when they are inside let-syntax forms.
(let ()
  (let-syntax ()
    (define (b) #t))
  (b))

;; Make sure transform-forms-to-triple doesn't use shadowed defines.
(let ((hej #t)
      (define (lambda _ #t)))
  (define hej #f)
  hej)

;; Test syntactic capture in the head position
(eval
 `(,(capture-syntactic-environment
     (lambda (env)
       'eq?))
   1 1))

;; Test syntactic capture with macro in head position
(eval
 `(let-syntax
      ((mac
        (syntax-rules ()
          ((_) #t))))
    (,(capture-syntactic-environment
       (lambda (env)
         'mac)))))

;; A somewhat esoteric test with synclosures and syncaptures
(eq? 0
     (eval
      `(let ((a 0))
         ,(capture-syntactic-environment
           (lambda (env-1)
             `(let ((b 1))
                ,(capture-syntactic-environment
                  (lambda (env-2)
                    (let ((a-1 (make-syntactic-closure env-1 '() 'a))
                          (a-2 (make-syntactic-closure env-2 '() 'a)))
                      `(let ((,a-2 2))
                         ,a-1))))))))))

;; Check that child environments within a transform-forms-to-triple
;; are preserved even on defines.
(let ()
  (let-syntax
      ((test (syntax-rules ()
               ((test) #t))))
    (define (test-fun) (test)))
  (test-fun))

;; Check that it's possible to have defines inside syncaptures within
;; scopes.
(equal?
 4
 (eval
  `(let ()
     ,(capture-syntactic-environment
       (lambda (_)
         `(define a 4)))
     a)))

;; Test that let-syntax within a let can contain a define-syntax that
;; uses macros defined by the previously mentioned let-syntax.
(let ()
  (let-syntax
      ((test-mac2
        (syntax-rules ()
          ((test-mac)
           #t))))
    
    (define-syntax test-mac
      (syntax-rules ()
        ((test-mac) (test-mac2)))))
  (test-mac))


;; Test that top-level let-syntax can contain a define-syntax that
;; uses macros defined by the previously mentioned let-syntax.
(begin
  (define-syntax test-mac2
    (syntax-rules ()
      ((test-mac2)
       #f)))
  
  (let-syntax
      ((test-mac2
        (syntax-rules ()
          ((test-mac2)
           #t))))
    
    (define-syntax test-mac
      (syntax-rules ()
        ((test-mac) (test-mac2)))))
  
  (test-mac))

;; Test macros that define thigns
(let ((test #f))
  (let-syntax
      ((one
        (syntax-rules ()
          ((one xx)
           (define xx #t)))))
    (one test)
    test))

;; Test macros that define macros
(let ((test (lambda () #f)))
  (let-syntax
      ((one
        (syntax-rules ()
          ((one xx)
           (define-macro (xx) #t)))))
    (one test)
    (test)))

;; Test that we don't leak scope over expansion phase borders
#;(begin ;; TODO syntax-begin is removed; reimplement this test without it.
  (syntax-begin (define ---test-variable #t))
  (eval
   `(let ((---test-variable #f))
      ,(capture-syntactic-environment
        (lambda (env)
          (expand-macro
           `(syntax-begin
             ,(make-syntactic-closure env '() '---test-variable))))))))

(letrec-syntax
    ((when
      (syntax-rules ()
        ((when exp block ...)
         (if exp
             (begin block ...)))))
     (foo
      (syntax-rules ()
        ((foo id)
         (let ((id #t))
           (when id
                 #t))))))
  (foo bar))


;; A test for environment-get, to see if it finds the right binding
(eval
 `(let ((a (lambda () #t)))
    ,(capture-syntactic-environment
      (lambda (env)
        `(let-syntax ((a (syntax-rules () ((_) #f))))
           ,(make-syntactic-closure env '() '(a)))))))


;; A test to see whether make-syntactic-closure can take the return
;; value of capture-syntactic-environment properly.
(eval
 `(let ((a #t))
    ,(capture-syntactic-environment
      (lambda (env)
        `(let ((a #f))
           ,(make-syntactic-closure
             env
             '()
             (capture-syntactic-environment
              (lambda (should-not-be-inner-env)
                (make-syntactic-closure
                 should-not-be-inner-env
                 '()
                 'a)))))))))

;; Another test to see whether make-syntactic-closure can take the
;; return value of capture-syntactic-environment properly.
(eval
 `(let ((a #f))
    ,(capture-syntactic-environment
      (lambda (env)
        `(let ((a #t))
           ,(make-syntactic-closure
             env
             '(a)
             (capture-syntactic-environment
              (lambda (should-not-be-inner-env)
                (make-syntactic-closure
                 should-not-be-inner-env
                 '()
                 'a)))))))))

;; Test syntax-rules macros that use the same binding name that refer
;; to distinct bindings in nested macro-expansions
(equal? '(1 2)
        (letrec-syntax
            ((let-values
              (syntax-rules ()
                ((let-values () ?args ?tmps ?body)
                 ((lambda ?args
                    (let ?tmps ?body))
                  1 2))
              
              ((let-values (?a . ?b) (?arg ...) (?tmp ...) ?body)
               (let-values ?b (?arg ... x) (?tmp ... (?a x)) ?body)))))
          (let-values (a b) () () (list a b))))

;; Test default values of DSSSL parameters whose expressions use other
;; values in the parameter list
(let ((a #f))
  ((lambda (a #!key (b a))
     b) #t))

;; Test default values of DSSSL parameters whose expressions use other
;; values in the parameter list
(let ((b #t))
    ((lambda (#!key (a b) (c #f))
       b)))

;; Test the distinction between literal identifiers and keywords
(eq? (let-syntax ((test-fail
                   (syntax-rules (else)
                     ((_ else (quote p)) 
                      (cond (#f #f) (else (quote p)))))))
       (test-fail else 'a))
     'a)

;; Test that quasiquote interacts well with begin
(equal? ((lambda (num) `(+ ,(begin num) 1)) 3)
        `(+ 3 1))


;; Test that let-syntax works with define-syntax, within a let
(let ()
  (let-syntax
      ((test-mac
        (syntax-rules ()
          ((test-mac) #t))))
    (define-syntax test
      (syntax-rules ()
        ((test) (test-mac)))))
  (test))

;; Test that let-syntax works with define-syntax, top level
(begin
  (let-syntax
      ((test-mac
        (syntax-rules ()
          ((test-mac) #t))))
    (define-syntax test
      (syntax-rules ()
        ((test) (test-mac)))))
  (test))

;; Test receive form
(equal? '(a b)
        (receive (a b) (values 'a 'b) (list a b)))



;;; Problematic things:

;; This should produce an error
(expect-exception
 (lambda ()
   (expand-macro '(let ((a 5) (a 6)) a))))



;; Test syntactic closures in lets. I seriously don't know what the
;; result of this should be. The hot spot in this test is the
;; syntactic closure that is most deeply nested; should it refer to
;; the a that is 3, or should it refer to the a in the top that is #t?
;;
;; The code that decides which one it should be is expand-synclosure,
;; the part of the cond that takes care of syntactic capture. If that
;; function clones the environment, the answer is the top level a,
;; otherwise the inner a.
(or #t
    (expand-macro
     `(let ((a #t))
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
                    ,a)))))))))

;; The same test, but for lambdas
(or #t
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
                    ,a)))))))))


;; calcing is never set back to #f. It probably should, at least in
;; load-once.


;;(expand-macro
;; '(letrec-syntax
;;      ((mac-1
;;        (lambda (form env mac-env)
;;          (let ((arg (cadr form)))
;;            (pp (list env: env mac-env: mac-env arg: arg))
;;            (expand-macro (make-syntactic-closure mac-env '() arg) env))))
;;       (mac
;;        (syntax-rules ()
;;          ((_ rest)
;;           (begin
;;             (mac-1 (let ((rest 0))
;;                      rest
;;                      (mac-1 rest)))
;;             (let ((rest 0))
;;               rest))))))
;;    (mac aa)))
;; 
;; 
;;(expand-macro
;; '(letrec-syntax
;;      ((mac-1
;;        (lambda (form env mac-env)
;;          (let ((arg (cadr form)))
;;            (pp (list env: env
;;                      mac-env: mac-env
;;                      arg: arg
;;                      (expand-macro arg env)
;;                      (expand-macro arg mac-env)))
;;            (expand-macro (make-syntactic-closure mac-env '() arg) env))))
;;       (mac
;;        (syntax-rules ()
;;          ((_ rest)
;;           (begin
;;             (mac-1 (let ((rest 0))
;;                      rest
;;                      (mac-1 rest)))
;;             (let ((rest 0))
;;               rest))))))
;;    (mac aa)))
;; 
;; 
;;(expand-macro
;; '(letrec-syntax
;;      ((mac-1
;;        (syntax-rules ()
;;          ((_ a) a)))
;;       (mac
;;        (syntax-rules ()
;;          ((_ rest)
;;           (let ((rest #t))
;;             rest
;;             (mac-1 rest))))))
;;    (mac aa)))
