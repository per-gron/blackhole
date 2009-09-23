;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                  Syntax-rules implementation                     ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define (substitution-append a b)
  (let* ((...s '())
         (res '())
         (add-fun (lambda (pair)
                    (if (eq? '... (car pair))
                        (set! ...s
                              (cons pair
                                    ...s))
                        (set! res
                              (cons pair
                                    res))))))
    (for-each add-fun a)
    (for-each add-fun b)
    (cond
     ((null? ...s) #f)
     
     ((null? (cdr ...s))
      (set! res
            (cons (car ...s)
                  res)))
     
     ;; There should never be more than two ...s
     (else
      (set!
       res
       (cons
        (cons
         '...
         (let loop ((al (cdar ...s))
                    (bl (cdadr ...s)))
           (cond
            ((null? al)
             bl)
            
            ((null? bl)
             al)
            
            (else
             (cons
              (substitution-append
               (car al)
               (car bl))
              (loop (cdr al)
                    (cdr bl)))))))
        res))))
    res))

(define (sc-memq env id ids)
  (if (pair? ids)
      (or (and (identifier=? env id
                             env (car ids))
               ids)
          (sc-memq env id (cdr ids)))
      #f))

(define (sc-assq env id ids)
  (if (pair? ids)
      (let ((hd (car ids)))
        (or (and (identifier=? env id
                               env (car hd))
                 hd)
            (sc-assq env id (cdr ids))))
      #f))

(define (pattern-match literals env pattern form)
  (let ((form (expand-syncapture form env)))
    (cond
     ((null? pattern)
      (and (null? form) '()))
     
     ((identifier? pattern)
      (if (sc-memq env pattern literals)
          (and (identifier=? empty-environment pattern
                             env form)
               '())
          (list (cons pattern
                      (make-syntactic-closure env '() form)))))
     
     ((and (pair? pattern)
           (pair? (cdr pattern))
           (identifier=? env '...
                         env (cadr pattern))
           (null? (cddr pattern)))
      (let* ((inner-pattern (car pattern)))
        (let loop ((matches '())
                   (current-form form))
          (cond
           ((null? current-form)
            (list (cons '... '())))
           
           ((not (pair? current-form))
            #f)
           
           (else
            (let ((pm (pattern-match literals
                                     env
                                     inner-pattern
                                     (car current-form))))
              (cond
               ;; Something didn't match.
               ((not pm)
                #f)
               
               ;; We got one match, recurse
               ((pair? (cdr current-form))
                (loop (cons pm
                            matches)
                      (cdr current-form)))
               
               ;; We got our final match. Return a substitution
               ;; match with the special variable name ...
               ((null? (cdr current-form))
                (list
                 (cons
                  '...
                  (reverse
                   (cons pm
                         matches)))))
               
               (else
                #f))))))))
     
     ((pair? pattern)
      (and (pair? form)
           (let ((a (pattern-match literals
                                   env
                                   (car pattern)
                                   (car form)))
                 (b (pattern-match literals
                                   env
                                   (cdr pattern)
                                   (cdr form))))
             (and a b (substitution-append a b)))))
     
     ((vector? pattern) ;; TODO This isn't tested
      (and (vector? form)
           (pattern-match literals
                          env
                          (vector->list pattern)
                          (vector->list form))))
     
     (else
      (and (equal? pattern form) '())))))

;; Transforms (+ a ... ...) into (+ (... (... a)))
(define (transform-ellipsis-to-prefix form)
  (cond
   ((pair? form)
    (if (and (pair? (cdr form))
             (eq? '... (cadr form)))
        (transform-ellipsis-to-prefix
         (cons (list '... (car form))
               (cddr form)))
        (cons (transform-ellipsis-to-prefix (car form))
              (transform-ellipsis-to-prefix (cdr form)))))

   (else form)))
  

(define (substitute env pattern-vars subs form)
  (cond
   ((identifier? form)
    (if (sc-memq env form pattern-vars)
        (let ((pair
               (let loop ((s subs))
                 (and (not (null? s))
                      (or (sc-assq env form (car s))
                          (loop (cdr s)))))))
          (if pair
              (cdr pair)
              (error "Unbound pattern variable" form)))
        form))
   
   ((pair? form)
    (if (and (pair? (car form))
             (eq? '... (caar form)))
        (let ((inner-form (cdar form))
              (inner-subs
               ;; It's safe to use assq here, because the '... comes
               ;; directly from pattern-match.
               (let ((p (assq '... (car subs))))
                 (if p
                     (cdr p)
                     (error "Using ellipsis when no ellipsis is in pattern: "
                            form)))))
          (let loop ((inner-subs inner-subs))
            (cond
             ((pair? inner-subs)
              (with-exception-catcher
               (lambda (e)
                 ;; This happens when using an unbound pattern variable
                 ;; Note that this code makes this syntax-rules
                 ;; implementation non-standards-compliant; things like
                 ;; 
                 ;; (let-syntax ((test
                 ;;               (syntax-rules ()
                 ;;                 ((test (a ...) (b ...))
                 ;;                  (begin (+ a b) ...)))))
                 ;;   (test (1 2) (3 4 5)))
                 ;; 
                 ;; should result in an error, but it doesn't. However,
                 ;; you can't just remove this code, because then it
                 ;; would reject some valid cases, like
                 ;;
                 ;; (let-syntax ((test
                 ;;               (syntax-rules ()
                 ;;                 ((test (a ...) (b ...))
                 ;;                  (begin (+ a) ... (+ b) ...)))))
                 ;;   (test (1 2) (3 4 5)))
                 (loop '()))
               (lambda ()
                 (append
                  (substitute env
                              pattern-vars
                              (cons (car inner-subs)
                                    subs)
                              inner-form)
                  (loop (cdr inner-subs))))))
             
             ((null? inner-subs)
              (substitute env pattern-vars subs (cdr form))))))
        (cons (substitute env pattern-vars subs (car form))
              (substitute env pattern-vars subs (cdr form)))))
   
   ;; TODO Vectors?
   
   (else form)))

(define (syntax-rules-proc literals . rules)
  ;; Precalculate the ellipsis prefix transformation and
  ;; do some validation
  (define prefix-ellipsis
    (map (lambda (rule)
           (if (not
                (and (list? rule)
                     (= 2 (length rule))))
               (error "Invalid syntax rule"
                      `(syntax-rules ,literals ,@rules)))
           (transform-ellipsis-to-prefix (cadr rule)))
         rules))

  ;; Return the actual macro transformer function
  (lambda (source env mac-env)
    (let ((form (expr*:strip-locationinfo source)))
      (call/cc
       (lambda (ret)
         (let* ((form (expand-syncapture form env))
                (form-args (cdr form)))
           (for-each
            (lambda (rule pe)
              (let* ((pattern (cdar rule))
                     (pattern-vars
                      (let loop ((pattern pattern))
                        (cond
                         ((eq? pattern '...)
                          '())
                         
                         ((identifier? pattern)
                          (list pattern))
                         
                         ((pair? pattern)
                          (append (loop (car pattern))
                                  (loop (cdr pattern))))
                         
                         (else '())))))
                (cond
                 ((pattern-match
                   literals
                   env
                   pattern
                   form-args) =>
                   (lambda (subs)
                     (let ((res
                            (expand-macro
                             (substitute env
                                         pattern-vars
                                         (list subs)
                                         pe)
                             mac-env)))
                       (ret res)))))))
            rules prefix-ellipsis)
           (error "Ill-formed special form: "
                  (extract-synclosure-crawler form))))))))

