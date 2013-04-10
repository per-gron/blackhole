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
    (#\D 0 "ignore-dependencies")
    (#\e 1 "eval")
    (#\f 0 "force")
    (#\h 0 "help")
    (#\k 0 "continue")
    (#\o 1 "output")
    (#\p 0 "pretend")
    (#\q 0 "quiet")
    (#\r 0 "recursive")
    (#\v 0 "verbose")))

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
                          (string-append "--" opt-name)
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

(define (display-pkgs pkgs port)
  (for-each
      (lambda (pkg)
        (display " * " port)
        (display (package-name&version pkg) port)
        (newline port))
    pkgs))

(define (exe-cmd cmd opts args)
  (define output-fn #f)
  (define quiet #f)
  (define verbose #f)
  
  (handle-opts!
   opts
   `(("output"
      ,@(lambda (val)
          (set! output-fn val)))
     ("quiet"
      ,@(lambda (val)
          (set! quiet (not (equal? val "no")))))
     ("verbose"
      ,@(lambda (val)
          (set! verbose (not (equal? val "no")))))))
  (ensure-one-arg! args)

  (if (not output-fn)
      (set! output-fn
            (path-strip-extension (car args))))

  (module-compile-to-standalone
   output-fn
   (module-reference-from-file (car args))
   port: (if quiet
             (open-string "")
             (current-output-port))
   verbose?: verbose))

(define (compile-cmd cmd opts args)
  (define recursive #f)
  (define bunch #f)
  (define quiet #f)
  (define continue #f)
  (define force #f)
  (define verbose #f)

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
          (set! force (not (equal? val "no")))))
     ("verbose"
      ,@(lambda (val)
          (set! verbose (not (equal? val "no")))))))
  
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
                              verbose?: verbose)
        (modules-compile! mods-and-deps
                          continue-on-error?: continue
                          port: port
                          force?: force
                          verbose?: verbose))))

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
  (define version #t)
  (define compile #t)
  (define quiet #f)
  (define pretend #f)
  (define verbose #f)
  (define ignore-dependencies #f)

  (handle-opts!
   opts
   `(("version"
      ,@(lambda (val)
          (set! version
                (with-input-from-string val read))))
     ("compile"
      ,@(lambda (val)
          (set! compile (or (not val)
                            (not (equal? val "no"))))))
     ("quiet"
      ,@(lambda (val)
          (set! quiet (not (equal? val "no")))))
     ("pretend"
      ,@(lambda (val)
          (set! pretend (not (equal? val "no")))))
     ("verbose"
      ,@(lambda (val)
          (set! verbose (not (equal? val "no")))))
     ("ignore-dependencies"
      ,@(lambda (val)
          (set! ignore-dependencies (not (equal? val "no")))))))
  
  (ensure-args! args)

  (if (and (not (eqv? #t version))
           (not (= 1 (length args))))
      (die/error "When specifying a version, only one package can be installed at a time:" args))

  (let ((pkgs-to-be-installed
         (or (find-packages-for-installation
              args
              version: version
              ignore-dependencies?: ignore-dependencies
              throw-error?: #f)
             (apply
              die/error
              (cons "No package with the specified version was found:"
                    args))))
        (port (if quiet
                  (open-string "")
                  (current-output-port))))
    (display (if pretend
                 "Would install the following packages:\n"
                 "Installing the following packages:\n")
             port)
    (display-pkgs pkgs-to-be-installed port)
    (newline port)
    
    (if (not pretend)
        (for-each (lambda (pkg)
                    (package-install!
                     pkg
                     compile?: compile
                     port: port
                     verbose?: verbose))
          pkgs-to-be-installed))))

(define (uninstall-cmd cmd opts args)
  (define version #t)
  (define quiet #f)
  (define pretend #f)
  (define verbose #f)
  (define ignore-dependencies #f)
  (define force #f)

  (handle-opts!
   opts
   `(("version"
      ,@(lambda (val)
          (set! version
                (with-input-from-string val read))))
     ("quiet"
      ,@(lambda (val)
          (set! quiet (not (equal? val "no")))))
     ("pretend"
      ,@(lambda (val)
          (set! pretend (not (equal? val "no")))))
     ("verbose"
      ,@(lambda (val)
          (set! verbose (not (equal? val "no")))))
     ("ignore-dependencies"
      ,@(lambda (val)
          (set! ignore-dependencies (not (equal? val "no")))))
     ("force"
      ,@(lambda (val)
          (set! force (not (equal? val "no")))))))
  
  (ensure-args! args)

  (if (and (not (eqv? #t version))
           (not (= 1 (length args))))
      (die/error "When specifying a version, only one package can be uninstalled at a time:" args))

  (let* ((pkgs (get-installed-packages))
         (after-pkgs
          (foldr
           (lambda (pkg-name accum)
             (let ((pkg-found
                    (find-suitable-package
                     pkgs
                     pkg-name
                     version: version
                     throw-error?: #f)))
               (if pkg-found
                   (tree-delete pkgs
                                pkg-found
                                package<?)
                   (die/error "Package could not be found:"
                              pkg-name
                              version))))
           pkgs
           args))
         (orphans
          (packages-find-orphans after-pkgs))
         (port (if quiet
                   (open-string "")
                   (current-output-port))))

    (if (not (null? orphans))
        (cond
         (ignore-dependencies
          (display "Warning: The following packages depend on the packages you are requesting to remove:\n" port)
          (for-each
              (lambda (pkg)
                (display " * " port)
                (display (package-name&version pkg) port)
                (newline port))
            orphans))
         (force
          (set! after-pkgs
                (tree-difference after-pkgs
                                 (list->tree orphans
                                             package<?)
                                 package<?)))
         (else
          (apply
           die/error
           (cons "The following packages depend on the packages you are requesting to remove:\n"
                 (map package-name&version
                   orphans))))))

    (let ((to-be-uninstalled
           (tree->list
            (tree-difference pkgs
                             after-pkgs
                             package<?))))
      (display (if pretend
                   "Would uninstall the following packages:\n"
                   "Uninstalling the following packages:\n")
               port)
      (display-pkgs to-be-uninstalled port)

      (if (and verbose (not pretend))
          (display "\nUninstalling:\n" port))
      (if (not pretend)
          (for-each
              (lambda (pkg)
                (if verbose
                    (begin
                      (display " * " port)
                      (display (package-name&version pkg) port)
                      (display " ." port)))
                (package-uninstall! pkg)
                (if verbose
                    (display ".\n" port)))
            to-be-uninstalled)))))

(define (list-cmd cmd opts args)
  (define quiet #f)

  (handle-opts!
   opts
   `(("quiet"
      ,@(lambda (val)
          (set! quiet (not (equal? val "no")))))))
  
  (ensure-no-args! args)

  (if (not quiet)
      (println "Installed packages:"))
  (for-each
      (lambda (pkg)
        (if (not quiet)
            (print " * "))
        (println (package-name&version pkg)))
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
  (define help-topics
    `(("modules" ,@help-modules)
      ("packages" ,@help-packages)
      
      ("commands" ,@help-commands)

      ("exe" ,@help-exe)
      ("compile" ,@help-compile)
      ("clean" ,@help-clean)

      ("install" ,@help-install)
      ("uninstall" ,@help-uninstall)
      ("list" ,@help-list)
      ("search" ,@help-search)

      ("deps" ,@help-deps)
      ("exported-names" ,@help-exported-names)

      ("repl" ,@help-repl)))
  
  (define num-args (length args))
  
  (handle-opts! opts `(("help" ,@(lambda (val) #t))))

  (cond
   ((zero? num-args)
    (println help-string))

   ((= 1 num-args)
    (let* ((arg (car args))
           (res (assoc arg help-topics)))
      (if res
          (println (cdr res))
          (die/error "Unknown help topic:" arg))))
   
   (else
    (die/error "Invalid arguments passed to help:" args))))

(define (repl-cmd cmd opts args)
  (define quiet #f)
  (define help #f)
  
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
           (with-input-from-string val read))))
     ("quiet"
      ,@(lambda (val)
          (set! quiet (not (equal? val "no")))))
     ("help"
      ,@(lambda (val)
          (set! help (not (equal? val "no")))))))

  (cond
   (help
    (help-cmd cmd opts args))
   (else
    (if (not quiet)
        (println "Gambit Scheme w/ Black Hole"))
    (##repl-debug #f #t))))

(define (unknown-cmd cmd opts args-sans-opts)
  (die/error "Unknown command:"
             cmd
             "To get a list of options, type 'bh help'"))

(define (cli arguments)
  (let ((commands
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
    (parse-opts
     arguments
     (lambda (actual-args-sans-opts opts)
       (let* ((args-sans-opts (if (null? actual-args-sans-opts)
                                  '("repl")
                                  actual-args-sans-opts))
              (cmd-pair (assoc (car args-sans-opts) commands))
              (cmd (if cmd-pair
                       (cdr cmd-pair)
                       (cdr (assoc "unknown-command" commands)))))
         (cmd (car args-sans-opts)
              opts
              (cdr args-sans-opts)))))))
