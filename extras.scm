;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Utility functions                          ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;


(define (modules-compile! mods #!optional continue-on-error port)
  (let* ((mods (resolve-modules mods))
         (mods-sorted
          (filter (lambda (x)
                    (and (memq x mods)
                         (module-needs-compile? x)))
                  (remove-duplicates
                   (reverse
                    (apply
                     append
                     (map (lambda (x)
                            (cons x
                                  (module-info-uses
                                   (loaded-module-info
                                    (module-reference-ref x)))))
                          mods))))))
         (nmods (length mods-sorted))
         (port (or port (current-output-port)))
         (file-number 1))
    
    (display "Compiling " port)
    (write nmods port)
    (display " modules\n" port)
    
    (for-each
     (lambda (mod)
       (write (module-path mod) port)
       (display " (" port)
       (write file-number port)
       (display "/" port)
       (write nmods port)
       (display ")\n" port)
       ;; The module might have been compiled by load-once earlier.
       (if (module-needs-compile? mod)
           (module-compile! mod continue-on-error: continue-on-error))
       (set! file-number (+ file-number 1)))
     mods-sorted)))

(define (modules-clean! mods)
  (for-each module-clean! mods))

(define (module-files-in-dir top-dir)
  (let ((mod-list '()))
    (let loop ((files (directory-files top-dir))
               (dir top-dir))
      (for-each
       (lambda (file)
         (let ((path (path-expand file dir)))
           (cond
            ((is-directory? path)
             (loop (directory-files path)
                   path))

            (else
             (if (string-ends-with path ".scm")
                 (set! mod-list
                       (cons path
                             mod-list)))))))
       files))
    mod-list))

(define (module-from-file file)
  (make-module local-loader
               (path-normalize file)))

(define (modules-in-dir top-dir)
  (map module-from-file
       (module-files-in-dir top-dir)))

(define (module-deps mod #!optional recursive)
  (if recursive
      (let ((mods
             (flatten
              (let loop ((mod mod))
                (let ((uses (module-info-uses
                             (loaded-module-info
                              (module-reference-ref x)))))
                  (cons uses
                        (map loop uses)))))))
        ;; Sort the modules in the order of which they depend on
        ;; each other. This algorithm is ridiculously inefficient.
        (remove-duplicates
         (reverse
          (apply
           append
           (map (lambda (x)
                  (cons x
                        (module-info-uses
                         (loaded-module-info
                          (module-reference-ref x)))))
                mods)))
         equal?))
      (module-info-uses
       (loaded-module-info
        (module-reference-ref mod)))))

(define (module-compile/deps! mod #!optional continue-on-error port)
  (modules-compile! (cons mod (module-deps mod #t)) continue-on-error port))

(define (module-clean/deps! mod #!optional continue-on-error port)
  (modules-clean! (cons mod (module-deps mod #t))))

(define (module-generate-export-list mod)
  (cons 'export
        (append
         (map car (module-info-symbols
                   (loaded-module-info
                    (module-reference-ref mod)))))))
  
(define (generate-tmp-dir thunk)
  (let ((compile-tmp-dir "~~/lib/modules/work/compile-tmp/"))
    (create-dir-unless-exists compile-tmp-dir)
    (let ((fn (let loop ((i 0))
                (let ((fn (path-expand (number->string i)
                                       compile-tmp-dir)))
                  (if (file-exists? fn)
                      (loop (+ i 1))
                      fn)))))
      (dynamic-wind
          (lambda ()
            (if (not fn)
                (error "generate-tmp-dir: Can't re-enter"))
            (create-directory fn))
          (lambda ()
            (thunk fn))
          (lambda ()
            (map (lambda (f)
                   (delete-file (path-expand f fn)))
                 (directory-files (list path: fn
                                        ignore-hidden: 'dot-and-dot-dot)))
            (delete-directory fn)
            (set! fn #f))))))

(define (module-compile-c-file-to-o c-filename
                                    #!key
                                    output
                                    (cc-options "")
                                    verbose)
  (if (not (and (or (not output)
                    (string? output))
                (string? cc-options)))
      (error "Invalid parameters"))

  (if (not output)
      (set! output (path-normalize
                    (string-append (path-strip-extension
                                    c-filename)
                                   ".o")
                    #f)))
  
  (##gambc-cc 'obj
              (path-directory output)
              (list c-filename)
              output
              cc-options
              "" ;; ld-options-prelude
              "" ;; ld-options
              verbose))

(define (module-link-files o-files
                           o1-file
                           #!key
                           standalone
                           (ld-options-prelude "")
                           (ld-options "")
                           verbose)
  (if (not (and (string? o1-file)
                (string? ld-options-prelude)
                (string? ld-options)))
      (error "Invalid parameters"))

  (for-each (lambda (x)
              (if (not (string? x))
                  (error "Invalid parameters")))
            o-files)
  
  (##gambc-cc (if standalone 'exe 'dyn)
              (current-directory)
              o-files
              o1-file
              "" ;; cc-options
              ld-options-prelude
              ld-options
              verbose))

(define (module-compile-bunch mode
                              to-file
                              files
                              #!key
                              (port (current-output-port))
                              verbose)
  (generate-tmp-dir
   (lambda (dir)
     (let* ((mods (map module-from-file files))
            (c-files
             (let loop ((mods mods) (i 0))
               (cond
                ((null? mods) '())
                
                (else
                 (let ((mod (car mods)))
                   (cons (path-expand
                          (string-append
                           (let ((ns (module-reference-namespace mod)))
                             (substring ns
                                        0
                                        (- (string-length ns) 1)))
                           ".c")
                          dir)
                         (loop (cdr mods)
                               (+ 1 i))))))))

            (standalone #f)
            (save-links #f))

       (case mode
         ((exe)
          (set! standalone #t))
         ((dyn)
          #!void) ;; Nothing to be done
         ((link)
          (set! save-links #t))
         (else
          (error "Unknown module-compile-bunch mode" mode)))
       
       (display "Compiling " port)
       (display (length files) port)
       (display " files...\n" port)
       
       (for-each
        (lambda (mod c-file file)
          (display file port)
          (display " ." port)
          (compile-with-options mod
                                file
                                to-c: c-file)
          (display "." port)
          (module-compile-c-file-to-o c-file verbose: verbose)
          (display "." port)
          (newline))
        mods c-files files)
       
       (let ((link-c-file
              (let ((file-name (path-strip-directory
                                to-file)))
                (let loop ((attempt 0))
                  (let ((try
                         (path-expand (string-append file-name
                                                     (if (zero? attempt)
                                                         ""
                                                         (number->string attempt))
                                                     ".c")
                                      dir)))
                    (if (file-exists? try)
                        (loop (+ 1 attempt))
                        try))))))
         
         (display "Creating link file..\n" port)
         (parameterize
          ;; Suppress warning messages from link-flat
          ((current-output-port
            (open-output-u8vector)))
          ((if standalone
               link-incremental
               link-flat)
           (map path-strip-extension
                c-files)
           output: link-c-file))
         
         (display "Compiling link file..\n" port)
         (module-compile-c-file-to-o link-c-file verbose: verbose)
         
         (display "Linking files..\n" port)
         (module-link-files
          (map (lambda (fn)
                 (string-append (path-strip-extension fn)
                                ".o"))
               (cons link-c-file c-files))
          (path-expand to-file (current-directory))
          standalone: standalone
          verbose: verbose))
       
       (if save-links
           (for-each (lambda (file)
                       (with-output-to-file
                           (string-append (path-strip-extension file)
                                          ".ol")
                         (lambda ()
                           (println (path-normalize to-file)))))
                     files))
       
       to-file))))

(define (module-compile-to-standalone name mod
                                      #!key
                                      verbose
                                      (port (current-output-port)))
  (let ((mod (resolve-one-module mod)))
    (module-compile-bunch
     'exe
     name
     (map module-path
          (append (module-deps mod #t)
                  (list mod)))
     verbose: verbose
     port: port)))

;;(module-compile-bunch 'link
;;                      "std/build.ob"
;;                      (module-files-in-dir
;;                       "~~/lib/modules/std"))

