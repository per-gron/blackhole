;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       The actual system                          ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; I have tried to organize this file so that there is no code that
;; depends on code that is below itself as much as possible/convenient.

;; This file only contains the neccesary functions. Utilities are
;; supposed to be in a separare module in the standard library.


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
            (with-module-macroexpansion ;; For *module-macroexpansion-uses*
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
                   (macroexpansion-symbol-defs (module-info-symbols info)
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
