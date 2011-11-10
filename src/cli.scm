(define (die/error . args)
  (let ((err (current-error-port)))
    (display "Error: " err)
    (for-each
        (lambda (arg)
          (display arg err)
          (display " " err))
      args)
    (display "\n") err)
  (exit 1))

(define short-opts
  '((#\b 1 "bunch") ;; 1 means that bunch takes an argument ...
    (#\c 0 "compile") ;; ... compile doesn't (hence 0)
    (#\e 1 "eval")
    (#\f 0 "force")
    (#\k 0 "continue")
    (#\o 1 "output")
    (#\q 0 "quiet")
    (#\r 0 "recursive")))

(define (parse-opts args kont)
  (define (opt? str)
    (and (> (string-length str) 1)
         (char=? #\- (string-ref str 0))))
  
  (define (long-opt? str)
    (and (> (string-length str) 2)
         (char=? #\- (string-ref str 0))
         (char=? #\- (string-ref str 1))))

  (define (short-opt? str)
    (and (opt? str)
         (not (long-opt? str))))
  
  (let loop ((args args)
             (args-sans-opts '())
             (opts '()))
    
    (define (consume-next-argument!)
      (if (or (null? (cdr args))
              (equal? "--" (cadr args)))
          (die/error "Expected an argument to" (car args)))
      (let ((val (cadr args)))
        (set-cdr! args (cddr args))
        val))
    
    (cond
     ((null? args)
      (kont (reverse args-sans-opts)
            (reverse opts)))

     ((equal? "--" (car args))
      (kont (append (reverse args-sans-opts)
                    (cdr args))
            (reverse opts)))

     ((long-opt? (car args))
      (let* ((=-pos (string-contains (car args) #\=))
             (opt-name
              (substring (car args)
                         2
                         (or =-pos
                             (string-length (car args)))))
             (opt-val
              (and =-pos
                   (substring (car args)
                              (+ 1 =-pos)
                              (string-length (car args))))))
        (loop (cdr args)
              args-sans-opts
              (cons (list opt-name
                          (string-append "-" opt-name)
                          opt-val)
                    opts))))

     ((short-opt? (car args))
      (let* ((str (car args))
             (len (string-length str)))
        (let inner-loop ((idx 1) (opts opts))
          (cond
           ((= len idx)
            (loop (cdr args)
                  args-sans-opts
                  opts))

           (else
            (let* ((opt-chr (string-ref str idx))
                   (opt (assq opt-chr short-opts)))
              (if (not opt)
                  (die/error "Unrecognized option" (car args)))

              (let ((val
                     (cond
                      ((zero? (cadr opt))
                       #f)

                      ((not (= 2 len))
                       (die/error "Option that takes an argument must not be grouped"
                              (car args)))

                      (else
                       (consume-next-argument!)))))
                (inner-loop (+ 1 idx)
                            (cons (list (caddr opt)
                                        (string #\- opt-chr)
                                        val)
                                  opts)))))))))

     (else
      (loop (cdr args)
            (cons (car args) args-sans-opts)
            opts)))))

(define (handle-opts! opts handlers)
  (for-each
      (lambda (opt)
        (let ((handler (assoc (car opt) handlers)))
          (if handler
              ((cdr handler) (caddr opt))
              (die/error "Option is not valid in this context:"
                         (cadr opt)))))
    opts))

(define (ensure-no-args! args)
  (if (not (null? args))
      (apply
       die/error
       (cons "Did not expect arguments:" args))))

(define (ensure-args! args)
  (if (null? args)
      (apply
       die/error
       (cons "At least one argument is required"))))

(define (ensure-one-arg! args)
  (if (not (and (list? args)
                (= 1 (length args))))
      (apply
       die/error
       (cons "Expected exactly one argument:" args))))

(define (exe-cmd cmd opts args)
  (define output-fn #f)
  (define quiet #f)
  
  (handle-opts!
   opts
   `(("output"
      ,@(lambda (val)
          (set! output-fn val)))
     ("quiet"
      ,@(lambda (val)
          (set! quiet
                (not (equal? val "no")))))))
  (ensure-one-arg! args)

  (if (not output-fn)
      (set! output-fn
            (path-strip-extension (car args))))

  (module-compile-to-standalone
   output-fn
   (module-reference-from-file (car args))
   port: (if quiet
             (open-string "")
             (current-output-port))))

(define (compile-cmd cmd opts args)
  (define recursive #f)
  (define bunch #f)
  (define quiet #f)
  (define continue #f)
  (define force #f)

  (handle-opts!
   opts
   `(("recursive"
      ,@(lambda (val)
          (set! recursive (not (equal? val "no")))))
     ("bunch"
      ,@(lambda (val)
          (set! bunch val)))
     ("quiet"
      ,@(lambda (val)
          (set! quiet (not (equal? val "no")))))
     ("continue"
      ,@(lambda (val)
          (set! continue (not (equal? val "no")))))
     ("force"
      ,@(lambda (val)
          (set! force (not (equal? val "no")))))))
  
  (ensure-args! args)

  (let* ((mod-refs
          (map (lambda (fn)
                 (if (not (file-exists? fn))
                     (die/error "File does not exist:" fn))
                 (module-reference-from-file fn))
            args))
         (mods-and-deps
          (if recursive
              (modules-deps mod-refs)
              mod-refs))
         (port
          (if quiet
              (open-string "")
              (current-output-port))))
    (if bunch
        (module-compile-bunch 'link
                              bunch
                              (map module-reference-path mods-and-deps)
                              modules: mods-and-deps
                              port: port
                              verbose: #f)
        (modules-compile! mods-and-deps
                          continue-on-error?: continue
                          port: port
                          force?: force))))

(define (clean-cmd cmd opts args)
  (define recursive #f)
  (define quiet #f)

  (handle-opts!
   opts
   `(("recursive"
      ,@(lambda (val)
          (set! recursive (not (equal? val "no")))))
     ("quiet"
      ,@(lambda (val)
          (set! quiet (not (equal? val "no")))))))
  
  (ensure-args! args)

  (let* ((mod-refs
          (map (lambda (fn)
                 (if (not (file-exists? fn))
                     (die/error "File does not exist:" fn))
                 (module-reference-from-file fn))
            args))
         (mods-and-deps
          (if recursive
              (modules-deps mod-refs)
              mod-refs))
         (port
          (if quiet
              (open-string "")
              (current-output-port))))
    
    (display "Cleaning modules...\n" port)
    (for-each
        (lambda (mod)
          (display " * " port)
          (display (path-normalize (module-reference-path mod)
                                   'shortest)
                   port)
          (module-clean! mod)
          (newline port))
      mods-and-deps)))

(define (install-cmd cmd opts args)
  (println "INSTALL! (Not implemented)"))

(define (uninstall-cmd cmd opts args)
  (println "UNINSTALL! (Not implemented)"))

(define (list-cmd cmd opts args)
  (ensure-no-args! args)
  (handle-opts! opts '())

  (println "Installed packages:")
  (for-each
      (lambda (pkg)
        (println " * "
                 (package-name pkg)
                 " ("
                 (version->symbol
                  (package-version pkg))
                 ")"))
    (installed-packages)))

(define (search-cmd cmd opts args)
  (ensure-no-args! args)
  (handle-opts! opts '())

  (println "Available packages:")
  (for-each
      (lambda (pkg)
        (println " * "
                 (package-name pkg)
                 " ("
                 (version->symbol
                  (package-version pkg))
                 ")"))
    (remote-packages)))

(define (deps-cmd cmd opts args)
  (define quiet #f)
  (define recursive #f)
  
  (handle-opts!
   opts
   `(("recursive"
      ,@(lambda (val)
          (set! recursive (not (equal? val "no")))))
     ("quiet"
      ,@(lambda (val)
          (set! quiet (not (equal? val "no")))))))
  
  (ensure-args! args)

  (for-each
      (lambda (arg)
        (if (not quiet)
            (println "Dependencies for " arg ":"))
        (let ((deps (module-deps (module-reference-from-file arg)
                                 recursive)))
          (for-each
              (lambda (dep)
                (if (not quiet)
                    (print " * "))
                (print (loader-prettify-path
                        (module-reference-loader dep)
                        (module-reference-path dep)))
                (newline))
            deps)))
    args))

(define (exported-names-cmd cmd opts args)
  (define quiet #f)
  
  (handle-opts!
   opts
   `(("quiet"
      ,@(lambda (val)
          (set! quiet (not (equal? val "no")))))))
  
  (ensure-args! args)

  (for-each
      (lambda (arg)
        (if (not quiet)
            (println "Exported names from " arg ":"))
        (for-each
            (lambda (name)
              (if (not quiet)
                  (print " * "))
              (println name))
          (module-exported-names
           (module-reference-from-file arg))))
    args))

(define (help-cmd cmd opts args)
  (println "HELP! (Not implemented)"))

(define (repl-cmd cmd opts args)
  (define quiet #f)
  
  (ensure-no-args! args)
  
  (handle-opts!
   opts
   `(("version"
      ,@(lambda (val)
          (println "Black Hole for Gambit Scheme, version [not yet determined]")
          (exit 0)))
     ("eval"
      ,@(lambda (val)
          (eval
           (with-input-from-string val
             (lambda () (read))))))
     ("quiet"
      ,@(lambda (val)
          (set! quiet
                (not (equal? val "no")))))))

  (if (not quiet)
      (println "Gambit Scheme w/ Black Hole"))
  (##repl-debug #f #t))

(define (unknown-cmd cmd opts args-sans-opts)
  (die/error "Unknown command:"
             cmd
             "To get a list of options, type 'bh help'"))

(define (cli actual-arguments)
  (let ((arguments (if (null? actual-arguments)
                       '("repl")
                       actual-arguments))

        (commands
         `(("exe" ,@exe-cmd)
           ("compile" ,@compile-cmd)
           ("clean" ,@clean-cmd)

           ("install" ,@install-cmd)
           ("uninstall" ,@uninstall-cmd)
           ("list" ,@list-cmd)
           ("search" ,@search-cmd)

           ("deps" ,@deps-cmd)
           ("exported-names" ,@exported-names-cmd)

           ("help" ,@help-cmd)
           ("repl" ,@repl-cmd)

           ("unknown-command" ,@unknown-cmd))))
    (let* ((cmd-pair (assoc (car arguments) commands))
           (cmd (if cmd-pair
                    (cdr cmd-pair)
                    (cdr (assoc "unknown-command" commands)))))
      (parse-opts
       (cdr arguments)
       (lambda (args-sans-opts opts)
         (cmd (car arguments)
              opts
              args-sans-opts))))))
