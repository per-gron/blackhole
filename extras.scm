
;; Beware of n^2 algorithms
(define (remove-duplicates list #!optional (predicate eq?))
  (cond
   ((null? list) '())
   ((pair? list)
    (let ((e (car list)))
      (cons e
            (remove-duplicates
             (filter (lambda (x)
                       (not (predicate x e)))
                     (cdr list))
             predicate))))
   (else (raise "Argument to remove-duplicates must be a list"))))

(define (recompile-modules mods #!optional continue-on-error port)
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
  (recompile-modules (cons mod (module-deps mod #t)) continue-on-error port))

(define (module-clean/deps! mod #!optional continue-on-error port)
  (for-each module-clean! (cons mod (module-deps mod #t))))

(define (module-generate-export-list mod)
  (cons 'export
        (append
         (reverse (module-info-defines (module-info mod)))
         (reverse (module-info-macros (module-info mod))))))
