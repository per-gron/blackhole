;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;               Utilities for module macroexpansion                ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;;;; ---------- Module info utilities ----------

;; TODO Right now, this isn't used. (I think)
(define (macroexpansion-symbol-defs symbols env)
  (let ((ns (module-namespace
             (environment-module env))))
    (map (lambda (pair)
           (let ((name (car pair))
                 (type (cdr pair)))
             (if (eq? 'def type)
                 (list name 'def (gen-symbol ns name))
                 (let ((mac (environment-get env name)))
                   (if (or (not mac)
                           (not (eq? 'mac (car mac))))
                       (error "Internal error in macroexpansion-symbol-defs:"
                              mac))
                   (list name ;; exported name
                         'mac
                         (cadr mac) ;; macro procedure
                         (caddr mac)))))) ;; macro environment
         symbols)))

;;;; ---------- Module macroexpansion utilities ----------

(define phase-sym (gensym 'phase))
(define loaded-module-sym (gensym 'loaded-module))
(define syntactic-tower-sym (gensym 'syntactic-tower))
(define name-sym (gensym 'name))
(define val-sym (gensym 'val))

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
               (cons imports pkgs))))
      
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
            ((top-environment (make-top-environment module-reference)))
            (values (expand-macro sexpr)
                    (top-environment))))
       (lambda (expanded-code env)
         ;; TODO Add something to check for duplicate imports and
         ;; exports.
         
         (values expanded-code
                 `(lambda (,phase-sym ,loaded-module-sym ,syntactic-tower-sym)
                    (let ((module-instances #f))
                      ,expanded-code

                      (values
                       (lambda (,name-sym)
                         (case ,name-sym
                           ((name) name)))
                       (lambda (,name-sym ,val-sym)
                         (case ,name-sym
                           ((name) (set! name ,val-sym)))))))
                 `((definitions . ,definitions)
                   (imports . ,imports)
                   (imports-for-syntax . ,imports-for-syntax)
                   (exports . ,exports)
                   (namespace-string . ,(environment-namespace env))
                   (options . ,options-)
                   (cc-options . ,cc-options-)
                   (ld-options-prelude . ,ld-options-prelude-)
                   (ld-options . ,ld-options-)
                   (force-compile . ,force-compile-))))))))
