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
         (filter (lambda (fn)
                   (and (string-begins-with fn begin-str)
                        (string->number
                         (substring fn
                                    (string-length begin-str)
                                    (string-length fn)))))
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
  (for-each (lambda (fn)
              (let ((dep-fn (deps-file-name fn)))
                (if (file-exists? dep-fn)
                    (delete-file dep-fn)))
              (delete-file fn))
            (object-files path)))

;;;; ---------- Loading ----------

(define (load-module-scm-file module-ref file)
  (let ((runtime-code
         compiletime-code
         visit-code
         info-code
         (module-macroexpand module-ref
                             (file-read-as-expr file))))
    (values (lambda ()
              (eval-no-hook runtime-code))
            (eval-no-hook compiletime-code)
            (eval-no-hook visit-code)
            (u8vector->module-reference
             (eval-no-hook info-code)))))

;; Takes the return value of ##load-object-file and returns a table
;; mapping "[module namespace]-[rt, ct or mi]" to the instantiation
;; procedure.
(define (load-object-file-result->table result)
  (list->table
   (map (lambda (x)
          (cons (vector-ref x 0)
                (vector-ref x 1)))
     (vector->list
      (vector-ref result 0)))))

;; file should be an absolute path to an object file
(define load-module-object-table
  (let ((registry (make-table)))
    (lambda (file)
      (or (table-ref registry file #f)
          (let ((result (##load-object-file file #t)))
            (if (not (and (vector? result)
                          (= 3 (vector-length result))))
                (error "Failed to load file:" file result))
            
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

            (let ((table (load-object-file-result->table result)))
              (table-set! registry file table)
              table))))))

(define (deps-file-name fn)
  (string-append fn ".deps"))

(define (load-deps-file fn)
  (with-input-from-file fn
    (lambda ()
      (let* ((fs (file-size fn))
             (u8v (make-u8vector fs)))
        (read-subu8vector u8v 0 fs)
        (u8vector->module-reference u8v)))))

(define (load-deps-from-deps-file fn)
  (for-each module-reference-ref
    (load-deps-file fn)))

(define (load-module-from-file module-ref file-with-extension)
  (let* ((file
          (path-strip-trailing-directory-separator
           (path-strip-extension
            (path-normalize file-with-extension))))
         
         (object-fn
          (let* ((ol (string-append file ".ol"))
                 (object-fn (if (file-exists? ol)
                                (with-input-from-file ol
                                  read-line)
                                (last-object-file file))))
            (and object-fn
                 (path-normalize object-fn #f file))))

         (force-compile
          (lambda ()
            (module-compile! module-ref)
            (load-module-from-file module-ref file-with-extension))))
    
    (cond
     ((eq? #t (module-needs-compile? module-ref))
      (force-compile))
     
     ((not object-fn)
      (let ((rt
             ct
             vt
             mi
             (load-module-scm-file module-ref
                                   (string-append file ".scm"))))
        (let ((ret (assq 'force-compile mi)))
          (if (and ret (cdr ret))
              (force-compile)
              (values rt ct vt mi)))))
     
     (else      
      (let* ((module-object-table
              (let ((deps-fn (deps-file-name object-fn)))
                (if (file-exists? deps-fn)
                    (load-deps-from-deps-file deps-fn))
                (load-module-object-table object-fn)))
             (ns
              (let ((str (module-reference-namespace module-ref)))
                (substring str 0 (- (string-length str) 1))))
             (rt (table-ref module-object-table
                            (string-append ns "-rt")
                            #f))
             (ct (table-ref module-object-table
                            (string-append ns "-ct")
                            #f))
             (vt (table-ref module-object-table
                            (string-append ns "-vt")
                            #f))
             (mi (table-ref module-object-table
                            (string-append ns "-mi")
                            #f)))
        (if (not (and rt ct vt mi))
            (error "Module initializers not found for:" ns))
        
        (values rt
                (ct)
                (vt)
                (u8vector->module-reference
                 (mi))))))))



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
                        shared
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
              (if shared
                  (string-append "-D___DYNAMIC " cc-options)
                  cc-options)
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
                              (options '())
                              (cc-options "")
                              (ld-options-prelude "")
                              (ld-options "")
                              verbose)
  (generate-tmp-dir
   "~~/lib/modules/work/compile-tmp/"
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
                           ,(string-append f "-vt.c")
                           ,(string-append f "-mi.c")
                           ,@accum))))
                (else
                 accum))))

            (deps-tree empty-tree)
            (ld-options-prelude-accum "")
            (ld-options-accum "")

            (standalone #f)
            (save-depfile #t)
            (save-links #f))

       (case mode
         ((exe)
          (set! standalone #t)
          (set! save-depfile #f))
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
             (let ((runtime-code
                    compiletime-code
                    visit-code
                    info-code
                    (module-macroexpand mod (file-read-as-expr file))))
               (let* ((info-tbl
                       (list->table
                        (u8vector->module-reference (eval-no-hook info-code))))
                      
                      (compile-sexp-to-o
                       (lambda (sexp fn)
                         (compile-sexp-to-c
                          (expr:deep-fixup sexp)
                          fn
                          options:
                          (append (table-ref info-tbl 'options)
                                  options))
                         (display "." port)
                         (compile-c-to-o
                          fn
                          verbose: verbose
                          cc-options: cc-options
                          shared: (not (eq? mode 'exe)))
                         (display "." port))))

                 (set! ld-options-prelude-accum
                       (string-append ld-options-prelude-accum
                                      " "
                                      (table-ref info-tbl 'ld-options-prelude)))
                 (set! ld-options-accum
                       (string-append ld-options-accum
                                      " "
                                      (table-ref info-tbl 'ld-options)))
                 
                 (let ((add-deps!
                        (lambda (deps)
                          (for-each
                              (lambda (dep)
                                (set! deps-tree
                                      (tree-add deps-tree
                                                (module-reference-absolutize
                                                 dep
                                                 mod)
                                                module-reference<?)))
                            deps))))
                   (add-deps! (table-ref info-tbl 'runtime-dependencies))
                   (add-deps! (table-ref info-tbl 'compiletime-dependencies)))
                 
                 (compile-sexp-to-o runtime-code
                                    (string-append c-file "-rt.c"))
                 (compile-sexp-to-o compiletime-code
                                    (string-append c-file "-ct.c"))
                 (compile-sexp-to-o visit-code
                                    (string-append c-file "-vt.c"))
                 (compile-sexp-to-o info-code
                                    (string-append c-file "-mi.c"))))
             (newline))
        mods c-files-no-ext files)
       
       (let ((link-c-file
              (let ((file-name (path-strip-directory
                                to-file)))
                (let loop ((attempt 0))
                  (let ((try
                         (path-expand
                          (string-append file-name
                                         (if (zero? attempt)
                                             ""
                                             (number->string attempt))
                                         ".c")
                          dir)))
                    (if (file-exists? try)
                        (loop (+ 1 attempt))
                        try))))))
         
         (display "Creating link file..\n" port)
         ((if standalone
              link-incremental
              link-flat)
          (map path-strip-extension c-files)
          output: link-c-file
          warnings?: #f)
         
         (display "Compiling link file..\n" port)
         (compile-c-to-o link-c-file
                         verbose: verbose
                         cc-options: cc-options
                         shared: (not (eq? mode 'exe)))
         
         (display "Linking files..\n" port)
         (link-files
          (map (lambda (fn)
                 (string-append (path-strip-extension fn)
                                ".o"))
               (cons link-c-file c-files))
          (path-expand to-file (current-directory))
          standalone: standalone
          verbose: verbose
          ld-options-prelude: (string-append ld-options-prelude
                                             " "
                                             ld-options-prelude-accum)
          ld-options: (string-append ld-options
                                     " "
                                     ld-options-accum)))

       (if save-depfile
           (let ((dep-list
                  (tree->list
                   (tree-difference deps-tree
                                    (list->tree mods module-reference<?)
                                    module-reference<?))))
             (if (pair? dep-list)
                 (begin
                   (display "Writing dependencies file..\n" port)
                   (with-output-to-file
                       (deps-file-name to-file)
                     (lambda ()
                       (let ((u8v (module-reference->u8vector dep-list)))
                         (write-subu8vector u8v
                                            0
                                            (u8vector-length u8v)))))))))

       (if save-links
           (for-each (lambda (file)
                       (with-output-to-file
                           (string-append (path-strip-extension file)
                                          ".ol")
                         (lambda ()
                           (println (path-normalize to-file)))))
                     files))
       
       to-file))))
