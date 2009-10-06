;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Utility functions                          ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;


(define (modules-compile! mods #!optional continue-on-error port)
  (with-module-cache
   (lambda ()
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
                                      (module-info x))))
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
              (module-compile! mod continue-on-error))
          (set! file-number (+ file-number 1)))
        mods-sorted)))))

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
      (remove-duplicates
       (flatten
        (let loop ((mod mod))
          (let ((uses (module-deps mod)))
            (cons uses
                  (map loop uses)))))
       equal?)
      (module-info-uses (module-info mod))))

(define (module-compile/deps! mod #!optional continue-on-error port)
  (modules-compile! (cons mod (module-deps mod #t)) continue-on-error port))

(define (module-clean/deps! mod #!optional continue-on-error port)
  (modules-clean! (cons mod (module-deps mod #t))))

(define (module-generate-export-list mod)
  (cons 'export
        (append
         (map car (module-info-symbols
                   (module-info mod))))))
  
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

;; More or less copied from _gsclib.scm
(define (module-compile-c-file c-filename
                               #!key
                               output
                               directory
                               (cc-options "")
                               (ld-options-prelude "")
                               (ld-options ""))
  
  (define (arg name val)
    (##string-append "GSC_CC_O_" name "=" val))
  
  (define (install-dir path)
    (parameterize
     ((##current-directory
       (##path-expand path)))
     (##current-directory)))
  
  (if (not (and (string? c-filename)
                (string? cc-options)
                (string? ld-options-prelude)
                (string? ld-options)
                (or (not output)
                    (string? output))))
      (error "Invalid parameters"))
  
  (if (not output)
      (set! output (path-normalize
                    (string-append (path-strip-extension
                                    c-filename)
                                   ".o")
                    #f)))
  
  (let* ((gambcdir-bin
          (install-dir "~~bin"))
         (gambcdir-include
          (install-dir "~~include"))
         (gambcdir-lib
          (install-dir "~~lib"))
         (c-filename-dir
          (or directory
              (parameterize
               ((##current-directory (##path-directory c-filename)))
               (##current-directory))))
         (c-filename-base
          (if directory
              c-filename
              (##path-strip-directory c-filename))))
    (##open-process-generic
     3 ;; (macro-direction-inout)
     #t
     (lambda (port)
       (let ((status (##process-status port)))
         (##close-port port)
         status))
     open-process
     (##list path:
             (##string-append gambcdir-bin "gsc-cc-o.bat")
             arguments:
             '()
             environment:
             (##append
              (let ((env (##os-environ)))
                (if (##fixnum? env) '() env))
              (##list (arg "GAMBCDIR_BIN"
                           (##path-strip-trailing-directory-separator
                            gambcdir-bin))
                      (arg "GAMBCDIR_INCLUDE"
                           (##path-strip-trailing-directory-separator
                            gambcdir-include))
                      (arg "GAMBCDIR_LIB"
                           (##path-strip-trailing-directory-separator
                            gambcdir-lib))
                      (arg "OBJ_FILENAME" output)
                      (arg "C_FILENAME_DIR"
                           (##path-strip-trailing-directory-separator
                            c-filename-dir))
                      (arg "C_FILENAME_BASE" c-filename-base)
                      (arg "CC_OPTIONS" cc-options)
                      (arg "LD_OPTIONS_PRELUDE" ld-options-prelude)
                      (arg "LD_OPTIONS" ld-options)))
             stdin-redirection: #f
             stdout-redirection: #f
             stderr-redirection: #f))))

(define (module-compile-c-file-to-o c-filename
                                    #!key
                                    output
                                    (cc-options ""))
  (module-compile-c-file c-filename
                         output: output
                         cc-options: (string-append "-c "
                                                    cc-options)))

(define (module-link-files o-files
                           o1-file
                           #!key
                           (ld-options-prelude "")
                           (ld-options ""))
  ;; TODO Does this support spaces in filenames?
  (module-compile-c-file ""
                         output: o1-file
                         directory: (current-directory)
                         ld-options-prelude: ld-options-prelude
                         ld-options:
                         (string-append
                          (apply
                           string-append
                           (map (lambda (fn)
                                  (string-append (path-normalize fn)
                                                 " "))
                                o-files))
                          ld-options)))

(define (module-compile-bunch to-file
                              files
                              #!key
                              (save-links #t)
                              (port (current-output-port)))
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
                           (let ((ns (module-namespace mod)))
                             (substring ns
                                        0
                                        (- (string-length ns) 1)))
                           ".c")
                          dir)
                         (loop (cdr mods)
                               (+ 1 i))))))))
            (link-c-file
             (path-expand (string-append
                           (path-strip-directory
                            to-file)
                           ".c")
                          dir)))
       
       (with-module-cache
        (lambda ()
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
             (module-compile-c-file-to-o c-file)
             (display "." port)
             (newline))
           mods c-files files)

          (display "Creating link file..\n" port)
          (parameterize
           ;; Suppress warning messages from link-flat
           ((current-output-port
             (open-output-u8vector)))
           (link-flat (map path-strip-extension
                           c-files)
                      output: link-c-file))

          (display "Compiling link file..\n" port)
          (module-compile-c-file-to-o link-c-file)

          (display "Linking files..\n" port)
          (module-link-files
           (map (lambda (fn)
                  (string-append (path-strip-extension fn)
                                 ".o"))
                (cons link-c-file c-files))
           (path-expand to-file (current-directory)))

          (if save-links
              (for-each (lambda (file)
                          (with-output-to-file
                              (string-append (path-strip-extension file)
                                             ".ol")
                            (lambda ()
                              (println (path-normalize to-file)))))
                        files))

          to-file))))))


;;(module-compile-bunch "std/build.ob"
;;                      (module-files-in-dir
;;                       "~~/lib/modules/std"))











