
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








;;;; ---------- Module paths ----------

;; TODO This isn't used right now. I don't remove it, becuase it might
;; be useful when implementing the lib/wget/http thing.

;; A module path is a symbol, for instance /srfi/1

(define (module-path-absolute? p)
  (let ((str (symbol->string p)))
    (and (positive? (string-length str))
         (char=? #\/ (string-ref str 0)))))


;; Removes extraneous "./" and "../" in a URI path. Copied from the
;; uri module
(define (remove-dot-segments str)
  (let* ((in-len (string-length str))
         (res (make-string in-len)))
    ;; i is where we are in the source string,
    ;; j is where we are in the result string,
    ;; segs is a list, used as a stack, of the indices of the
    ;; previously encountered path segments in the result string.
    (letrec
        ((new-segment
          (lambda (i j segs)
            (let* ((segment-start (car segs))
                   (segment-length (- j segment-start 1)))
              (cond
               ;; Check for .
               ((and (= 1 segment-length)
                     (char=? #\. (string-ref res segment-start)))
                (loop (+ 1 i) segment-start segs))
 
               ;; Check for ..
               ((and (= 2 segment-length)
                     (char=? #\. (string-ref res segment-start))
                     (char=? #\. (string-ref res (+ 1 segment-start))))
                (cond
                 ;; Take care of the "/../something" special case; it
                 ;; should return "/something" and not "something".
                 ((and (= 1 segment-start)
                       (char=? #\/ (string-ref res 0)))
                  (loop (+ 1 i) 1 '(1)))
                 
                 ;; This is needed because the code in the else clause
                 ;; assumes that segs is a list of length >= 2
                 ((zero? segment-start)
                  (loop (+ 1 i) 0 segs))
 
                 (else
                  (loop (+ 1 i) (cadr segs) (cdr segs)))))
               
               ;; Check for the end of the string
               ((>= (+ 1 i) in-len)
                j)
 
               (else
                (loop (+ 1 i) j (cons j segs)))))))
         (loop
          (lambda (i j segs)
            (if (>= i in-len)
                (new-segment i j segs)
                (let ((chr (string-ref str i)))
                  (string-set! res j chr)
                  (if (char=? chr #\/)
                      (new-segment i (+ 1 j) segs)
                      (loop (+ 1 i) (+ 1 j) segs)))))))
      (let ((idx (loop 0 0 '(0))))
        (substring res 0 idx)))))

(define (module-path-absolutize p #!optional ref)
  (if (module-path-absolute? p)
      p
      (string->symbol
       (remove-dot-segments
        (if ref
            (string-append (symbol->string ref)
                           "/../"
                           (symbol->string p))
            (string-append "/" (symbol->string p)))))))
