;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;               Utilities for module macroexpansion                ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;;;; ---------- Module info utilities ----------

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

(define calc-mode (make-parameter 'repl)) ;; one of 'repl, 'calc, 'load

(define (module-info-calculate module-reference #!optional filename)
  (let ((symbols '())
        (exports #f)
        (imports '())
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
      
      (*module-macroexpansion-export*
       (lambda (e)
         (set! exports
               (append e (or e '())))))
      
      (*module-macroexpansion-define*
       (lambda (name)
         (set! symbols
               (cons (cons name 'def)
                     symbols))))
      
      (*module-macroexpansion-define-syntax*
       (lambda (name proc-sexp env)
         (set! symbols
               (cons (cons name 'mac)
                     symbols))))
      
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


     (let ((env #f))
       (if filename
           (parameterize
            ((calc-mode 'calc)
             (top-environment (make-top-environment
                               (resolve-one-module module-reference))))
            (expand-macro
             (expr*:strip-locationinfo
              (file-read-as-expr filename)))
            (set! env (top-environment))))

       (call-with-values
           (lambda ()
             (if exports
                 (resolve-exports exports env)
                 (values
                  (macroexpansion-symbol-defs symbols env)
                  '())))
         (lambda (exports export-uses)
           ;; TODO Add something to check for duplicate imports and
           ;; exports.
           `("list of exported names and macros"
             (namespace-string . ,(TODO module-reference))
             (options . ,options)
             (cc-options . ,cc-options)
             (ld-options-prelude . ,ld-options-prelude)
             (ld-options . ,ld-options)
             (force-compile . ,force-compile)
             (imports . ,imports)
             "list of defined names and macros whether they are exported or not"
             "list of the modules that the module's macros depend on")
           (make-module-info
            (reverse (*module-macroexpansion-symbols*))
            exports
            (*module-macroexpansion-imports*)
            (remove-duplicates ;; TODO This is an n^2 algorithm = sloow
             (append export-uses
                     (*module-macroexpansion-uses*))
             equal?)
            (*module-macroexpansion-options*)
            (*module-macroexpansion-cc-options*)
            (*module-macroexpansion-ld-options-prelude*)
            (*module-macroexpansion-ld-options*)
            (*module-macroexpansion-force-compile*)
            env)))))))

;;;; ---------- Module macroexpansion utilities ----------

(define (module-macroexpand module sexpr #!optional (tower (make-syntactic-tower)))
  (error "Not implemented")
  (values runtime-code
          compiletime-code
          info-code))
