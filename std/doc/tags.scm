(use /misc/splice
     /srfi/1)

(private)

;; This function is incredibly ugly. I can't think of a nicer way to
;; write it right now. Maybe I should use my queue module..
(define (paragraphify forms)
  (letrec
      (;; List where we accumulate the current paragraph while we
       ;; build it
       (current-para '())
       ;; Where we are in the head string we're parsing
       (pos 0)
       ;; Temporary buffer where we save string output
       (out-buf (open-output-string))
       ;; This is a boolean that is true as long as there only has
       ;; been whitespace in the current paragraph. It gets ignored.
       (new-para? #t)
       ;; Yet another state boolean. This should be true iff the last
       ;; non-whitespace char (excluding newlines) was a newline char.
       (last-char-was-newline #f)
       ;; Adds one more thing to the current paragraph
       (push
        (lambda (x)
          (let ((object? (car x))
                (val (cdr x)))
            (if object?
                (let ((out-str (get-output-string out-buf)))
                  (if (positive?
                       (string-length out-str))
                      (push (cons #t out-str)))
                  (set! new-para? #f)
                  (set! current-para
                        (cons val
                              current-para)))
                (cond
                 ((and new-para?
                       (or (eq? #\newline val)
                           (eq? #\space val)))
                  #f)
                 
                 ((and (eq? #\newline val)
                       last-char-was-newline)
                  (new-para))
                 
                 (else
                  (cond
                   ((eq? #\newline val)
                    (set! last-char-was-newline #t))
                   
                   ((not (eq? #\space val))
                    (set! last-char-was-newline #f)))
                  
                  (write-char val out-buf)
                  (set! new-para? #f)))))))
       ;; List where we accumulate the paragraphs we've collected
       (paras '())
       (new-para
        (lambda ()
          ;; Flush the output buffer
          (push '(#t))
          (let ((p (cdr current-para)))
            (if (not (null? p))
                (set! paras
                      (cons (reverse p)
                            paras))))
          (set! new-para? #t)
          (set! last-char-was-newline #f)
          (set! current-para '())))
       (skip
        (lambda ()
          (if (and (pair? forms)
                   (string? (car forms))
                   (< pos (string-length
                           (car forms))))
              (set! pos (+ 1 pos))
              (begin
                (set! pos 0)
                (set! forms (and (pair? forms)
                                 (cdr forms)))))))
       (peek
        (lambda ()
          (cond
           ((null? forms)
            #!eof)
           
           ((not (string? (car forms)))
            (cons #t (car forms)))
           
           (else
            (if (>= pos (string-length
                         (car forms)))
                (begin
                  (skip)
                  (peek))
                (cons #f
                      (string-ref (car forms) pos)))))))
       (read
        (lambda ()
          (let ((res (peek)))
            (skip)
            res))))
    
    (let loop ()
      (let ((res (read)))
        (if (not (eq? #!eof res))
            (begin
              (push res)
              (loop)))))
    
    (new-para)
    (reverse paras)))

(define (string-just-whitespace? str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (let ((chr (string-ref str i)))
            (and (or (eq? #\linefeed chr)
                     (eq? #\space chr)
                     (eq? #\return chr)
                     (eq? #\tab chr)
                     (eq? #\page chr))
                 (loop (+ i 1))))
          #t))))

(define (paraify forms element block-elements)
  (apply
   append
   (map
    (lambda (x)
      (define (make-para accum)
        (cond
         ((null? accum)
          '())

         ((and (null? (cdr accum))
               (string? (car accum))
               (string-just-whitespace? (car accum)))
          '())

         (else
          (list (cons element
                      (reverse accum))))))
      
      (let loop ((x x) (accum '()))
        (cond
         ((null? x)
          (make-para accum))
         
         ((not (pair? x))
          (error "Invalid input (must be proper list)" forms))
         
         ((and (pair? (car x))
               (memq (caar x) block-elements))
          
          `(,@(make-para accum)
            ,(car x)
            ,@(loop (cdr x) '())))
         
         (else
          (loop (cdr x)
                (cons (car x) accum))))))
    (paragraphify forms))))




(define (filter-strings lst)
  (filter (lambda (x) (not (string? x)))
          lst))

(define-syntax define-element-args
  (syntax-rules ()
    ((_) '())
    
    ((_ name)
     `((name ,name)))

    ((_ name names ...)
     (cons `(name ,name)
           (define-element-args names ...)))))

(define-syntax define-element
  (syntax-rules ()
    ((_ type name)
     (define (name . body)
       `(name ,@(type body))))
    
    ((_ type name args ...)
     (define (name args ... . body)
       (cons
        'name
        (cons
         `(@ ,@(define-element-args args ...))
         (type body)))))))

(/private)

(define scmd-inline unsplice-list)

(define (scmd-block contents)
  (paraify
   (unsplice-list contents)
   'p
   '(title section proc arguments example)))

(define (scmd-just-block contents)
  (filter-strings
   (unsplice-list contents)))

;;; Inline elements

(define-element scmd-inline abbr title)
(define-element scmd-inline acronym title)
(define-element scmd-inline em)
(define-element scmd-inline cite)
(define-element scmd-inline strong)
(define-element scmd-inline s)
(define-element scmd-inline o)
(define-element scmd-inline q)
(define-element scmd-inline dfn)
(define-element scmd-inline kbd)
(define-element scmd-inline sub)
(define-element scmd-inline sup)
(define-element scmd-inline var)
(define-element scmd-inline filepath)
(define-element scmd-inline footnote)

(define-syntax ref
  (syntax-rules ()
    ((ref var)
     (list 'ref
           (symbol->string 'var)))))

;;; Block level elements

(define-element scmd-block section title)
(define-element scmd-just-block repl)
(define-element scmd-just-block dl)
;;x(define-element scmd-just-block ul)
(define-element scmd-just-block ol)
(define-element scmd-block li)
(define-element scmd-block dd)
(define-element scmd-block dt)
(define-element scmd-inline block)
(define-element scmd-inline h1)
(define-element scmd-inline h2)
(define-element scmd-inline h3)
(define-element scmd-inline h4)
(define-element scmd-inline h5)
(define-element scmd-inline h6)
(define-element scmd-block note)
(define-element scmd-block alert)
(define-element scmd-block tip)
(define-element scmd-block source)
(define-element scmd-block output)
(define-element scmd-block blockquote)

(private)
(define-syntax ul-inner
  (syntax-rules ()
    ((_)
     '())

    ((_ (contents ...) rest ...)
     (cons (cons 'li
                 (scmd-block '(contents ...)))
           (ul-inner rest ...)))

    ((_ x rest ...)
     (ul-inner rest ...))))
(/private)

(define-syntax ul
  (syntax-rules ()
    ((ul args ...)
     (cons 'ul
           (ul-inner args ...)))))

