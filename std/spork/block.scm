(import js)

(export block-name
        block-attributes
        block-children
        block-transaction?
        block-transaction
        block-transaction!
        block-name-set!
        block-child-append!
        block-child-prepend!
        block-child-remove!
        block-clear-children!
        block-attribute-set!
        block-text-content
        block-text-content-set!
        block-attribute
        block-has-attribute?
        block-has-child-nodes?
        block-child-ref
        block-attribute-remove!
        block)

(define-type block-t
  name
  attributes
  children)

(define (make-block var-fun name rest)
  (let* ((maybe-atts (car rest))
         (args (cdr rest))
         (maybe-attrs-is-attrs (and (pair? maybe-attrs)
                                    (eq? '@ (car maybe-attrs))))
         (block (var-fun
                 (make-block-t name
                               (if maybe-attrs-is-attrs
                                   (cdr maybe-attrs)
                                   '())
                               (if maybe-attrs-is-attrs
                                   args
                                   (cons maybe-attrs args))))))
    (lambda (#!optional (action 'render) value)
      (cond
       ((eq? action 'render)
        (let ((b (block)))
          `(,(block-t-name b)
            ,@(let ((attrs (block-t-attributes b)))
                (if (null? attrs)
                    '()
                    `((@ ,@attrs))))
            ,@(block-t-children b))))
       
       ((eq? action 'get)
        (block))
       
       ((eq? action 'set!)
        (block value))
       
       (else
        (error "Invalid block action" action))))))

(define (block-get-block-t block)
  (block 'get))

(define-type transaction-cons
  car
  cdr)

(define-type transaction-t
  block
  fun
  args)

(define null-transaction (list 'null-transaction))

(define current-transaction (make-parameter #f))

(define-syntax block
  (syntax-rules ()
    ((block name args ...)
     (make-block make-variable
                 `name
                 `(args ...)))))

(define (block-name block)
  (block-t-name (block-get-block-t block)))

(define (block-attributes block)
  (block-t-attributes (block-get-block-t block)))

(define (block-children block)
  (block-t-children (block-get-block-t block)))

(define (block-transaction? obj)
  (or (transaction-t? obj)
      (eq? obj null-transaction)
      (transaction-cons? obj)))

(define (block-transaction thunk)
  (parameterize
   ((current-transaction null-transaction))
   (thunk)
   (current-transaction)))

(define (block-add-transaction-action! transaction)
  (current-transaction
   (make-transaction-cons transaction
                          (current-transaction))))

(define (block-transaction! thunk)
  (block-add-transaction-action!
   (block-transaction thunk)))


;; Utility function for the letrec-syntax macros below.
;; functions is an a-list of the form (element-name . javascript-function-sexp)
(define (make-block-js-module functions)
  (println
   (js-module-code
    (make-js-module
     '(process-block-element)
     (list core-module)
     `((define process-block-functions
         (obj
          ,@(apply
             append
             (map (lambda (pair)
                    (list (car pair)
                          (cdr pair)))
                  functions))))
       
       (define (process-block-element element)
         (let ((block (document.getElementById
                       (element.getAttribute "bid"))))
           (for-each
            (lambda (elm)
              ((ref process-block-functions elm.nodeName) block elm))
            element.childNodes))))))))

(letrec-syntax
    ((action-function
      (syntax-rules ()
        ((action fun-name _ action-fun __)
         (define (fun-name block . args)
           (block-add-transaction-action!
            (make-transaction-t block action-fun args))))))
     
     (action-functions
      (syntax-rules ()
        ((action-functions (action ...))
         (action-function action ...))
        
        ((action-functions (action ...) rest ...)
         (begin
           (action-function action ...)
           (action-functions rest ...)))))
     
     (action-js-function
      (syntax-rules ()
        ((action-js-function) '())
        
        ((action-js-function (_ elm-name __ action-fun) rest ...)
         (cons (cons 'elm-name 'action-fun)
               (action-js-function rest ...)))))
     
     (action-js
      (syntax-rules ()
        ((action-js action ...)
         (make-block-js-module
          (action-js-function action ...)))))
     
     (actions
      (syntax-rules ()
        ((actions action ...)
         (begin
           (action-js action ...)
           (action-functions action ...))))))
  
  (actions
   (block-name-set!
    nn
    (lambda (name attrs children new-name)
      (values new-name
              attrs
              children
              `(nn (@ (v ,new-name)))))
    (lambda (block node)
      (set! block.nodeName (node.getAttribute "v"))))
   
   (block-child-append!
    ca
    (lambda (name attrs children . val)
      (values name
              attrs
              (append children
                      (list val))
              `(ca ,@val)))
    (lambda (block node)
      (for-each (lambda (n)
                  (block.appendChild n))
                node.childNodes)))
   
   (block-child-prepend!
    cp
    (lambda (name attrs children . val)
      (values name
              attrs
              (cons val children)
              `(cp ,@val)))
    (lambda (block node)
      (let ((first-child block.firstChild))
        (for-each (lambda (n)
                    (block.insertBefore n first-child))
                  node.childNodes))))
   
   (block-child-remove!
    cr
    (lambda (name attrs children val)
      (values name
              attrs
              (filter (lambda (x)
                        (not (eq? x val)))
                      children)
              `(cr (@ (i ,TODO)))))
    (lambda (block node)
      (block.removeChild
       (ref block.childNodes
            (node.getAttribute "i")))))
   
   (block-clear-children!
    cc
    (lambda (name attrs children)
      (values name
              attrs
              '()
              `(cc)))
    (lambda (block node)
      (while block.childNodes.length
             (block.removeChild block.firstChild))))
   
   (block-attribute-set!
    as
    (lambda (n attrs c name val)
      (let ((old-attrs
             (filter (lambda (x)
                       (not (eq? x name)))
                     attrs)))
        (values n
                (if val
                    (cons (list name val)
                          old-attrs)
                    old-attrs)
                c
                `(as (@ (n ,name) (v ,val))))))
    (lambda (block node)
      (block.setAttribute (node.getAttribute "n")
                          (node.getAttribute "v"))))))

(define (block-text-content block)
  'TODO)

(define (block-text-content-set! block text)
  (block-transaction!
   (lambda ()
     (block-clear-children! block)
     (block-child-append! text))))

(define (block-attribute block name)
  (al-get (block-attributes block) name))

(define (block-has-attribute? block name)
  (assoc (block-attributes block) name))

(define (block-has-child-nodes? block)
  (null? (block-children block)))

(define (block-child-ref block idx)
  (list-ref (block-children block) idx))

(define (block-attribute-remove! block name)
  (block-attribute-set! block name #f))
