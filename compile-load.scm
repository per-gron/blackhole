;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;               Utilities for compiling and loading                ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;;;; ---------- File utilities ----------

(define (object-files path)
  (let* ((dir (path-directory path))
         (begin-str (string-append (path-strip-directory
                                    (path-strip-extension path))
                                   ".o")))
    (map (lambda (fn)
           (path-expand fn dir))
         (filter (lambda (fn) (string-begins-with fn begin-str))
                 (directory-files dir)))))

(define (object-file-extract-number fn)
  (or (string->number
       (substring fn
                  (let loop ((i (string-length fn)))
                    (if (zero? i)
                        0
                        (let ((chr
                               (char->integer
                                (string-ref fn (- i 1)))))
                          (if (and (>= chr (char->integer #\0))
                                   (<= chr (char->integer #\9)))
                              (loop (- i 1))
                              i))))
                  (string-length fn)))
      0))

(define (last-object-file path)
  (let ((lst (object-files path))
        (max-num -1)
        (res #f))
    (let loop ((lst lst))
      (if (not (null? lst))
          (let ((num (object-file-extract-number (car lst))))
            (if (> num max-num)
                (begin
                  (set! max-num num)
                  (set! res (car lst))))
            (loop (cdr lst)))))
    res))

(define (clean-file path)
  (for-each delete-file
            (object-files path)))

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

;;;; ---------- Loading ----------

(define *load-once-registry* (make-table))

(define *load-and-init-registry* (make-table))

(define (load-and-init file module-ref)
  (let* ((fn
          (let* ((ol (string-append file ".ol"))
                 (fn (if (file-exists? ol)
                         (with-input-from-file ol
                           read-line)
                         (last-object-file file))))
            (and fn
                 (path-normalize fn
                                 #f
                                 file))))
         
         (result
          (and fn
               (or (table-ref *load-and-init-registry*
                              fn
                              #f)
                   (##load-object-file fn #t)))))
    
    (cond
     ((not fn)
      (load file))
     
     (else
      (if (not
           (and (vector? result)
                (= 3 (vector-length result))))
          (error "Failed to load file:" file result))
      
      (table-set! *load-and-init-registry*
                  fn
                  result)
      
      (let ((missing-constants
             (vector-ref result 1)))
        (for-each
            (lambda (pair)
              (let ((var (car pair))
                    (mod (cdr pair)))
                (print "*** WARNING -- Variable \""
                       var
                       "\" used in module \""
                       mod
                       "\" is undefined\n")))
          missing-constants))
      
      (let* ((exec-vect
              (vector-ref result 0))
             (exec-len
              (vector-length exec-vect))
             (ns
              (let ((str (module-reference-namespace module-ref)))
                (substring str 0 (- (string-length str) 1))))
             (procedure-or-vector
              (if (= exec-len 1)
                  (vector-ref exec-vect 0)
                  (let loop ((i 0))
                    (cond
                     ((>= i exec-len)
                      (error "Module initializer not found for:" ns))
                     
                     ((let ((name-sym (##procedure-name
                                       (vector-ref exec-vect i))))
                        (and
                         name-sym
                         (let* ((name-str
                                 (symbol->string name-sym))
                                (name
                                 (substring name-str
                                            1
                                            (string-length name-str))))
                           (equal? name ns))))
                      (vector-ref exec-vect i))
                        
                     (else
                      (loop (+ 1 i))))))))
        ;; The API for this changed in Gambit 4.5.3. This is to be
        ;; compatible with Gambits both newer and older than this.
        ((if (vector? procedure-or-vector)
             (vector-ref procedure-or-vector 1)
             procedure-or-vector)))))))

(define (load-once file-with-extension module)
  (let ((module (and module (resolve-one-module module))))
    (parameterize
     ((*top-environment* (make-top-environment module))
      (*expansion-phase*
       (syntactic-tower-first-phase
        (make-syntactic-tower))))
     (let* ((file (path-strip-trailing-directory-separator
                   (path-strip-extension
                    (path-normalize file-with-extension))))
            (scm (string-append file ".scm"))
            (scm-exists? (file-exists? scm))
            (time (table-ref *load-once-registry* file #f)))
       (if (not (equal? time
                        (or (not scm-exists?)
                            (file-last-changed-seconds scm))))
           (let ((info (and module
                            (TODO-module-info module))))
             ;; If the file needs to be compiled, compile it (if it
             ;; isn't compiled and is set to force-compile or if the
             ;; scm file is newer than the object file.)
             (if (and module
                      (let ((res (module-needs-compile? module)))
                        (if (module-info-force-compile info)
                            res
                            (eq? res #t))))
                 (begin
                   (print file " is being compiled...\n")
                   (module-compile! module)))
             ;; Load it.
             (let ((ret (load-and-init file module)))
               (table-set! *load-once-registry*
                           file (or (not scm-exists?)
                                    (file-last-changed-seconds scm)))
               ret)))))))



;;;; ---------- Compilation ----------

(define (compile-sexp-to-c sexp
                           fn
                           #!key
                           (options '()))
  (##gc) ;; Avoid out-of-memory related crashes
  (let ((hook (lambda (_) sexp))
        (prev-hook #f))
    (dynamic-wind
        (lambda ()
          (set! prev-hook c#expand-source)
          (set! c#expand-source hook))
        (lambda ()
          (compile-file-to-c "/dev/null"
                             output: fn
                             options: options))
        (lambda ()
          (set! c#expand-source prev-hook)))))

(define (compile-c-to-o c-filename
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

(define (link-files o-files
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
     (let* ((mods (map module-reference-from-file files))
            (c-files-no-ext
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
                                        (- (string-length ns) 1))))
                          dir)
                         (loop (cdr mods)
                               (+ 1 i))))))))
            (c-files
             (let loop ((fs c-files-no-ext)
                        (accum '()))
               (cond
                ((pair? fs)
                 (let ((f (car fs)))
                   (loop (cdr fs)
                         `(,(string-append f "-rt.c")
                           ,(string-append f "-ct.c")
                           ,(string-append f "-mi.c")
                           ,@accum))))
                (else
                 accum))))

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
          (let ((compile-sexp-to-o
                 (lambda (sexp fn)
                   (compile-sexp-to-c (expr:deep-fixup sexp) fn)
                   (display "." port)
                   (compile-c-to-o fn verbose: verbose)
                   (display "." port))))
            (call-with-values
                (lambda ()
                  (module-macroexpand mod (file-read-as-expr file)))
              (lambda (runtime-code
                       compiletime-code
                       info-code)
                (compile-sexp-to-o runtime-code
                                   (string-append c-file "-rt.c"))
                (compile-sexp-to-o compiletime-code
                                   (string-append c-file "-ct.c"))
                (compile-sexp-to-o info-code
                                   (string-append c-file "-mi.c")))))
          (newline))
        mods c-files-no-ext files)
       
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
            (map path-strip-extension c-files)
            output: link-c-file))
         
         (display "Compiling link file..\n" port)
         (compile-c-to-o link-c-file verbose: verbose)
         
         (display "Linking files..\n" port)
         (link-files
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
