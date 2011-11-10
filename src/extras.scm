;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Utility functions                          ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define (module-compile! mod
                         #!key
                         continue-on-error?
                         to-c
                         (port (current-output-port))
                         verbose
                         (options '())
                         (cc-options "")
                         (ld-options-prelude "")
                         (ld-options ""))
  (let* ((mod (resolve-one-module mod))
         (perform-compile!
          (lambda ()
            (let ((path (module-reference-path mod)))
              (let ((result (module-compile-bunch
                             'dyn
                             (string-append
                              (path-strip-extension path)
                              ".o"
                              (number->string
                               (+ 1 (let ((lo (last-object-file path)))
                                      (if lo
                                          (object-file-extract-number lo)
                                          0)))))
                             (list path)
                             options: options
                             cc-options: cc-options
                             ld-options-prelude: ld-options-prelude
                             ld-options: ld-options
                             verbose: verbose
                             port: port)))
                (if (not result)
                    (error "Compilation failed")))))))
    (if continue-on-error?
        (with-exception-catcher
         (lambda (e)
           (display "Warning: Compilation failed: " port)
           (display-exception e port)
           #f)
         perform-compile!)
        (perform-compile!))))


(define (modules-compile! mods #!key
                          continue-on-error? port force?)
  (let* ((mods (resolve-modules mods))
         (mods-sorted
          (filter (lambda (x)
                    (and (memq x mods)
                         (or force? (module-needs-compile? x))))
                  (remove-duplicates
                   (reverse
                    (apply
                     append
                     (map (lambda (x)
                            (let ((info
                                   (loaded-module-info
                                    (module-reference-ref x))))
                            (cons x
                                  (module-info-dependencies info))))
                          mods))))))
         (nmods (length mods-sorted))
         (port (or port (current-output-port)))
         (file-number 1))
    
    (display "Compiling " port)
    (write nmods port)
    (display " modules...\n" port)
    
    (for-each
     (lambda (mod)
       (display " * " port)
       (display (loader-prettify-path
               (module-reference-loader mod)
               (module-reference-path mod))
              port)
       (display " (" port)
       (write file-number port)
       (display "/" port)
       (write nmods port)
       (display ")\n" port)
       ;; The module might have been compiled by load-once earlier.
       (if (or force? (module-needs-compile? mod))
           (module-compile! mod
                            continue-on-error?: continue-on-error?
                            port: (open-u8vector)))
       (set! file-number (+ file-number 1)))
     mods-sorted)))

(define (module-clean! mod)
  (let* ((mod (resolve-one-module mod))
         (fn (module-reference-path mod)))
    (and fn (clean-file (module-reference-path mod)))))

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

(define (modules-in-dir top-dir)
  (map module-reference-from-file
       (module-files-in-dir top-dir)))

;; This function should probably do something about runtime vs
;; compiletime dependencies? Right now it doesn't treat them different
;; at all.
(define (module-deps mod #!optional recursive)
  (if recursive
      (let ((mods
             (flatten
              (let loop ((mod mod))
                (let ((uses (module-info-dependencies
                             (loaded-module-info
                              (module-reference-ref mod)))))
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
                        (module-info-dependencies
                         (loaded-module-info
                          (module-reference-ref x)))))
                mods)))
         equal?))
      (module-info-dependencies
       (loaded-module-info
        (module-reference-ref mod)))))

(define (modules-deps mods #!optional recursive?)
  (filter
   (lambda (x)
     (eq? local-loader
          (module-reference-loader x)))
   (remove-duplicates
    (reverse
     (apply
      append
      (map (lambda (mod)
             (cons mod (module-deps mod recursive?)))
        mods))))))

(define (module-compile/deps! mod #!key continue-on-error? port force?)
  (let ((mod (resolve-one-module mod)))
    (modules-compile! (cons mod (module-deps mod #t))
                      continue-on-error?: continue-on-error?
                      port: port
                      force?: force?)))

(define (module-clean/deps! mod #!key continue-on-error? port)
  (modules-clean! (cons mod (module-deps mod #t))))

(define (module-exported-names mod)
  (append
   (map car (module-info-symbols
             (loaded-module-info
              (module-reference-ref mod))))))

(define (module-generate-export-list mod)
  (cons 'export
        (module-exported-names mod)))

(define (module-compile-to-standalone name mod
                                      #!key
                                      verbose
                                      (port (current-output-port)))
  (let* ((mod (resolve-one-module mod))
         (mods (append (module-deps mod #t)
                       (list mod))))
    (module-compile-bunch
     'exe
     name
     (map (lambda (mref)
            (loader-real-path (module-reference-loader mref)
                              (module-reference-path mref)))
       mods)
     modules: mods
     verbose: verbose
     port: port)))

(define (modules-compile-directory! dir target
                                    #!key
                                    verbose
                                    (port (current-output-port)))
  (module-compile-bunch 'link
                        target
                        (module-files-in-dir dir)
                        port: port
                        verbose: verbose))
