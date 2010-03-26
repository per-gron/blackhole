;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       The actual system                          ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; I have tried to organize this file so that there is no code that
;; depends on code that is below itself as much as possible/convenient.

;; This file only contains the neccesary functions. Utilities are
;; supposed to be in a separare module in the standard library.

;;;; ---------- Module info utilities ----------

(define empty-module-info
  (make-module-info
   '() '() '() '() '() "" "" "" #f builtin-environment))

;; The placement of this code is a little counterintuitive. It is
;; here, because this is the code that actually does the
;; calc-info.
(define-env load-environment
  "module-loadenv#"
  ((import
    (lambda (source env mac-env)
      (let ((pkgs (extract-synclosure-crawler
                   (cdr (expr*:strip-locationinfo source)))))
        (with-module-cache
         (lambda ()
           (call-with-values
               (lambda () (resolve-imports
                           (extract-synclosure-crawler pkgs)))
             (lambda (def mod)
               (*module-loadenv-uses*
                (append (*module-loadenv-uses*)
                        mod))
               (*module-loadenv-imports*
                (append (*module-loadenv-imports*)
                        def))
               (module-add-defs-to-env def env))))))))
   
   (module
    (lambda (code env mac-env)
      (error "Ill-placed module form" code)))
   
   (define
     (lambda (source env mac-env)
       (let* ((code (expr*:value source))
              (src (transform-to-lambda (cdr code)))

              (name-form (car (expr*:value src)))
              (name (expand-synclosure name-form env))
              (def-env (if (syntactic-closure? name-form)
                           (syntactic-closure-env name-form)
                           env))
              (sym (if (syntactic-closure? name-form)
                       (syntactic-closure-symbol name-form)
                       name-form)))
         
         (environment-add-define! def-env
                                  (expr*:value name))
         
         (*module-loadenv-symbols*
          (cons (cons sym 'def)
                (*module-loadenv-symbols*)))
         (void))))
   
   (module#define-macro-register
     (lambda (form env mac-env)
       (let ((src (transform-to-lambda (cdr form))))
         (*module-loadenv-symbols*
          (cons (cons (car src) 'mac)
                (*module-loadenv-symbols*))))
       (void)))
   
   (let
       (lambda (code env mac-env)
         (void)))
   
   (letrec
       (lambda (code env mac-env)
         (void)))
   
   (lambda
       (lambda (code env mac-env)
         (void)))
   
   (export
    (lambda (code env mac-env)
      (*module-loadenv-exports*
       (append (cdr (extract-synclosure-crawler code))
               (or (*module-loadenv-exports*) '())))))
   
   (compile-options
    (nh-macro-transformer
     (lambda (#!key options
                    cc-options
                    ld-options-prelude
                    ld-options
                    force-compile)
       (if options
           (*module-loadenv-options*
            options))
       (if cc-options
           (*module-loadenv-cc-options*
            cc-options))
       (if ld-options-prelude
           (*module-loadenv-ld-options-prelude*
            ld-options-prelude))
       (if ld-options
           (*module-loadenv-ld-options*
            ld-options))
       (if force-compile
           (*module-loadenv-force-compile*
            force-compile))
       (void))))
   
   (c-declare
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))
   
   (c-initialize
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))
   
   (c-define-type
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))
   
   (c-lambda
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))
   
   (c-define
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))))

(define-macro (make-loadenv-vars . vars)
  (let ((syms (map (lambda (var)
                     (string->symbol
                      (string-append
                       "*module-loadenv-"
                       (symbol->string (car var))
                       "*")))
                   vars))
        (defaults (map cadr vars)))
    `(begin
       ,@(map (lambda (sym)
                `(define ,sym (make-parameter #f)))
              syms)
       
       (define (with-module-loadenv thunk)
         (parameterize
          ,(map (lambda (sym default)
                  `(,sym ,default))
                syms
                defaults)
          (thunk))))))

(make-loadenv-vars (symbols '())
                   (exports #f)
                   (imports '())
                   (uses '())
                   (options '())
                   (cc-options "")
                   (ld-options-prelude "")
                   (ld-options "")
                   (force-compile #f))

(define (loadenv-symbol-defs symbols env)
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
                       (error "Internal error in loadenv-symbol-defs:"
                              mac))
                   (list name ;; exported name
                         'mac
                         (cadr mac) ;; macro procedure
                         (caddr mac)))))) ;; macro environment
         symbols)))

;; This function makes use of the module-loadenv dynamic environment.
(define (interpret-loadenv-exports env)
  (let* ((exps (*module-loadenv-exports*)))
    (if exps
        (resolve-exports exps env)
        (values
         (loadenv-symbol-defs (*module-loadenv-symbols*)
                              env)
         '()))))

(define calc-mode (make-parameter 'repl)) ;; one of 'repl, 'calc, 'load

(define (module-info-calculate module #!optional filename)
  (with-module-loadenv
   (lambda ()
     (let ((env #f))
       (if filename
           (parameterize
            ((calc-mode 'calc)
             (top-environment (make-top-environment
                               (resolve-one-module module))))
            (expand-macro
             (expr*:strip-locationinfo
              (file-read-as-expr filename)))
            (set! env (top-environment))))

       (call-with-values
           (lambda () (interpret-loadenv-exports env))
         (lambda (exports export-uses)
           ;; TODO Add something to check for duplicate imports and
           ;; exports.
           (make-module-info
            (reverse (*module-loadenv-symbols*))
            exports
            (*module-loadenv-imports*)
            (remove-duplicates ;; TODO This is an n^2 algorithm = sloow
             (append export-uses
                     (*module-loadenv-uses*))
             equal?)
            (*module-loadenv-options*)
            (*module-loadenv-cc-options*)
            (*module-loadenv-ld-options-prelude*)
            (*module-loadenv-ld-options*)
            (*module-loadenv-force-compile*)
            env)))))))


;;;; ---------- Module utility functions ----------

(define (current-module)
  (environment-module (top-environment)))

(define (current-loader)
  (let ((cm (current-module)))
    (if cm
        (module-loader (current-module))
        local-loader)))


(define *calc-info-cache* (make-parameter #f))

(define (with-module-cache thunk)
  (if (*calc-info-cache*)
      (thunk)
      (parameterize
       ((*calc-info-cache* (make-table)))
       (suspend-ns-table-changes thunk))))

(define (make-module-util-function fn)
  (lambda (mod)
    (with-module-cache
     (lambda ()
       (fn (resolve-one-module mod))))))

(define module-info
  (make-module-util-function
   (lambda (mod)
     (let ((mp (module-path mod)))
       (or (table-ref (*calc-info-cache*) mp #f)
           (let ((ret ((loader-calculate-info
                        (module-loader mod))
                       mod)))
             (table-set! (*calc-info-cache*) mp ret)
             ret))))))

(define module-needs-compile?
  (make-module-util-function
   (lambda (mod)
     (let* ((path (module-file mod))
            (of (last-object-file path)))
       (if of
           (not (file-newer? of path))
           'not-compiled)))))

(define (module-compile! mod
                         #!key
                         continue-on-error
                         to-c)
  (let ((mod (resolve-one-module mod)))
    (with-module-cache
     (lambda ()
       (with-exception-catcher
        (lambda (e)
          (if continue-on-error
              (begin
                (display "Warning: Compilation failed: ")
                (display-exception e)
                #f)
              (raise e)))
        (lambda ()
          (let ((info (module-info mod)))
            (with-module-loadenv ;; For *module-loadenv-uses*
             (lambda ()
               (let ((result (compile-with-options
                              mod
                              (module-file mod)
                              to-c: to-c
                              options: (module-info-options info)
                              cc-options: (module-info-cc-options info)
                              ld-options-prelude: (module-info-ld-options-prelude
                                                   info)
                              ld-options: (module-info-ld-options info))))
                 (if (not result)
                     (error "Compilation failed"))))))))))))

(define module-clean!
  (make-module-util-function
   (lambda (mod)
     (let ((fn (module-file mod)))
       (and fn (clean-file (module-file mod)))))))

(define module-namespace
  (let ((fn
         (make-module-util-function
          (lambda (mod)
            (string-append
             (let ((loader (module-loader mod)))
               (if (eq? loader module-module-loader)
                   "module"
                   (namespace-choose-unique mod)))
             "#")))))
    (lambda (mod)
      ;; This function might be called with #f as argument
      (if mod (fn mod) "~#"))))


(define (module-add-defs-to-env defs
                                #!optional (te (top-environment))
                                #!key (phase (expansion-phase)))
  (with-module-cache
   (lambda ()
     (for-each
      (lambda (def)
        (if (eq? 'def (cadr def))
            ;; Regular define
            (environment-add-def!
             te
             (car def) ;; The name it's imported as
             (caddr def) ;; The name it's imported from
             phase: phase)
            ;; Macro
            (environment-add-mac!
             te
             ;; The name it's imported as
             (car def)
             ;; The macro procedure
             (caddr def)
             ;; The macro's environment
             (cadddr def)
             phase: phase)))
      defs))))

(define (module-load/deps modules)
  (with-module-cache
   (lambda ()
     (let ((load-table (make-table)))
       (for-each
        (lambda (module)
          (let rec ((module module))
            (cond
             ((not (table-ref load-table
                              (module-path module)
                              #f))
              (table-set! load-table
                          (module-path module)
                          #t)
              
              (for-each rec
                        (module-info-uses
                         (module-info module)))

              (let ((fn (module-file module)))
                (if fn
                    (load-once fn module)))))))
        modules)))))


(define (module-import modules #!optional (env (top-environment)))
  (with-module-cache
   (lambda ()
     (call-with-values
         (lambda () (resolve-imports modules))
       (lambda (defs mods)
         (if (or (eq? (calc-mode) 'repl)
                 (> (expansion-phase) 0))
             (module-load/deps mods))
         
         (module-add-defs-to-env defs env))))))

(define module-module
  (let* ((repl-environment #f)
         (fn (make-module-util-function
              (lambda (mod)
                (if (not (environment-module (top-environment)))
                    (set! repl-environment (top-environment)))

                (top-environment (make-top-environment mod))
                (module-load/deps (list mod))
                
                (let ((info (module-info mod)))
                  (module-add-defs-to-env (module-info-imports info))
                  (module-add-defs-to-env
                   (loadenv-symbol-defs (module-info-symbols info)
                                        (module-info-environment info))))
                (void)))))
    (lambda (mod)
      ;; This function might be called with #f as argument
      (if mod
          (fn mod)
          (begin
            (top-environment repl-environment)
            (void))))))



;;;; ---------- Some environment creation stuff ----------

;; This code is here because it depends on the module-info machinery.

(define module-env-table
  (let* ((ns (make-table))
         (env (make-environment #f ns)))
    (module-add-defs-to-env
     (module-info-exports
      ((loader-calculate-info module-module-loader) #f))
     env
     phase: #f)
    ns))


(define builtin-ns
  (let* ((builtin-pair
          (cons (env-ns builtin-environment)
                gambit-builtin-table))
         (inside-letrec-table
          (env-ns inside-letrec-environment))
         (calcing-table
          (cons (env-ns load-environment)
                builtin-pair))
         (inside-letrec-pair
          (cons inside-letrec-table
                builtin-pair)))
    (lambda ()
      (let ((ns
             (cond
              ((eq? (calc-mode) 'calc)
               calcing-table)
              
              ((inside-letrec)
               inside-letrec-pair)
              
              (else
               builtin-pair))))
      (values (if (or (not (zero? (expansion-phase)))
                      (not (environment-module (top-environment))))
                  (cons module-env-table ns)
                  ns)
              #f)))))

(define (make-top-environment module)
  (let ((ns (cons (make-table)
                  builtin-ns)))
    (make-environment module ns)))

(define empty-environment
  (make-top-environment #f))

(define top-environment
  (make-parameter (make-top-environment #f)))
