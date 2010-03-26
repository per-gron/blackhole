;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;               Utilities for compiling and loading                ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;;;; ---------- Loader utility functions ----------

(define *load-once-registry* (make-table))

(define *load-and-init-registry* (make-table))

(define (load-and-init file mod)
  (with-module-loadenv
   (lambda ()
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
                 (let ((str (module-namespace mod)))
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
                procedure-or-vector)))))))))

(define (load-once file-with-extension module)
  (let ((module (and module (resolve-one-module module))))
    (parameterize
     ((top-environment (make-top-environment module))
      (expansion-phase 0)
      (calc-mode 'load))
     (with-module-cache
      (lambda ()
        (let* ((file (path-strip-trailing-directory-separator
                      (path-strip-extension
                       (path-normalize file-with-extension))))
               (scm (string-append file ".scm"))
               (scm-exists? (file-exists? scm))
               (time (table-ref *load-once-registry* file #f)))
          (if (not (equal? time
                           (or (not scm-exists?)
                               (file-last-changed-seconds scm))))
              (let ((info (and module (module-info module))))
                ;; If the file needs to be compiled, compile it (if it
                ;; isn't compiled and is set to force-compile or if the
                ;; scm file is newer than the object file.)
                (if (and module
                         (let ((res (module-needs-compile? module)))
                           (if (module-info-force-compile? info)
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
                  ret)))))))))

(define (compile-with-options module
                              fn
                              #!key
                              to-c
                              (options '())
                              (cc-options "")
                              (ld-options-prelude "")
                              (ld-options ""))
  (##gc) ;; Avoid out-of-memory related crashes
  (parameterize
   ((top-environment (make-top-environment
                      (resolve-one-module module)))
    (calc-mode 'load))
   (if to-c
       (compile-file-to-c fn
                          output: (or (and (string? to-c) to-c)
                                      (string-append (path-strip-extension fn)
                                                     ".c"))
                          options: (append options *compiler-options*))
       (compile-file fn
                     options: (append options *compiler-options*)
                     cc-options: (string-append
                                  cc-options " " *compiler-cc-options*)
                     ld-options-prelude: (string-append
                                          ld-options-prelude
                                          " "
                                          *compiler-ld-options-prelude*)
                     ld-options: (string-append
                                  ld-options " " *compiler-ld-options*)))))
