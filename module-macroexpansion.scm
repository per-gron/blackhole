;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;               Utilities for module macroexpansion                ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define (transform-to-define source)
  (let ((code
         (expr*:value source))
        (default-action
         (lambda ()
           `(define ,(gensym)
              ,source))))
    (cond
     ((pair? code)
      (let ((code-car (expr*:value (car code))))
        (case code-car
         ((begin ##begin)
          (expr*:value-set
           source
           (cons (car code)
                 (map transform-to-define
                      (cdr code)))))

         ((c-define)
          ;; TODO
          (error "c-define is not implemented"))

         ((c-define-type)
          ;; TODO
          (error "c-define-type is not implemented"))

         ((c-initialize)
          ;; TODO
          (error "c-define-type is not implemented"))

         ((c-declare)
          ;; TODO
          (error "c-declare is not implemented"))

         ((let-syntax letrec-syntax cond-expand)
          ;; This shouldn't happen
          (error "Internal error in transform-to-define"))

         ((declare
           ##define
           define
           ;; The macro forms are rarely or never here, but we check
           ;; for them just in case.
           ##define-macro
           ##define-syntax
           define-macro
           define-syntax)
          source)

         (else
          (default-action)))))

     (else
      (default-action)))))

(define loaded-module-sym (gensym 'loaded-module))
(define expansion-phase-sym (gensym 'syntactic-tower))
(define name-sym (gensym 'name))
(define val-sym (gensym 'val))

(define (generate-compiletime-code namespace-string
                                   expanded-code
                                   definitions
                                   syntax-dependencies)
  (let ((names (map (lambda (x)
                      (if (eq? 'def (cadr x))
                          (cons (car x)
                                (gen-symbol namespace-string
                                            (car x)))
                          (cons (car x) (caddr x))))
                    definitions)))
    `(lambda (,loaded-module-sym ,expansion-phase-sym)
       (let ((module-instances #f))
         ,(transform-to-define expanded-code)
         
         (values
          (lambda (,name-sym)
            (case ,name-sym
              ,@(map (lambda (name)
                       `((,(car name))
                         ,(cdr name)))
                  names)
              (else (error "Unbound variable" ,name-sym))))
          (lambda (,name-sym ,val-sym)
            (case ,name-sym
              ,@(map (lambda (name)
                       `((,(car name))
                         (set! ,(cdr name) ,val-sym)))
                  names)
              (else (error "Unbound variable" ,name-sym)))))))))

(define (module-macroexpand module-reference
                            sexpr
                            #!optional (tower (make-syntactic-tower)))
  (let ((definitions '())
        (imports '())
        (imports-for-syntax '())
        (exports '())
        (options- '())
        (cc-options- "")
        (ld-options-prelude- "")
        (ld-options- "")
        (force-compile- #f))
    (parameterize
     ((*module-macroexpansion-import*
       (lambda (pkgs)
         (set! imports
               (cons imports pkgs))))

      (*module-macroexpansion-import-for-syntax*
       (lambda (pkgs)
         (set! imports-for-syntax
               (append pkgs imports))))
      
      (*module-macroexpansion-export*
       (lambda (e)
         (set! exports (cons e exports))))
      
      (*module-macroexpansion-define*
       (lambda (name)
         (set! definitions
               (cons (list name 'def)
                     definitions))))
      
      (*module-macroexpansion-define-syntax*
       (lambda (name proc-sexp env)
         (set! definitions
               (cons (list name 'mac proc-sexp env)
                     definitions))))
      
      (*module-macroexpansion-force-compile*
       (lambda ()
         (set! force-compile #t)))
      
      (*module-macroexpansion-compile-options*
       (lambda (#!key options
                      cc-options
                      ld-options-prelude
                      ld-options
                      force-compile)
         (if options
             (set! options- options))
         (if cc-options
             (set! cc-options- cc-options))
         (if ld-options-prelude
             (set! ld-options-prelude- ld-options-prelude))
         (if ld-options
             (set! ld-options- ld-options))
         (if force-compile
             (set! force-compile- force-compile)))))

     (call-with-values
         (lambda ()
           (parameterize
               ((*top-environment*
                 (make-top-environment module-reference))
                (*expansion-phase*
                 (syntactic-tower-first-phase
                  (make-syntactic-tower))))
             (values (expand-macro sexpr)
                     (*top-environment*))))
       (lambda (expanded-code env)
         ;; TODO Add something to check for duplicate imports and
         ;; exports.

         (let ((syntax-dependencies
                (remove-duplicates
                 (call-with-values
                     (lambda ()
                       (resolve-imports imports-for-syntax module-reference))
                   (lambda (defines modules)
                     modules)))))
           (values expanded-code
                   (generate-compiletime-code (environment-namespace env)
                                              expanded-code
                                              definitions
                                              syntax-dependencies)
                   `',(object->u8vector
                       `((definitions . ,definitions)
                         (imports . ,imports)
                         (imports-for-syntax . ,imports-for-syntax)
                         (exports . ,exports)
                         (namespace-string . ,(environment-namespace env))
                         (options . ,options-)
                         (cc-options . ,cc-options-)
                         (ld-options-prelude . ,ld-options-prelude-)
                         (ld-options . ,ld-options-)
                         (force-compile . ,force-compile-))))))))))
