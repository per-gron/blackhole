;;; Source positions

(define (make-pos line col)
  (if (not (and (##fixnum? line)
                (##fixnum? col)))
      (error "Expecting fixnums:" a b))
  (##fixnum.+ (##fixnum.- line 1)
              (##fixnum.* (##fixnum.- col 1) 65536)))

(define (pos-line pos)
  (if (not (and (##fixnum? pos)))
      (error "Expecting fixnum:" pos))
  (+ 1 (bitwise-and pos 65535)))

(define (pos-col pos)
  (if (not (and (##fixnum? pos)))
      (error "Expecting fixnum:" pos))
  (+ 1 (quotient pos 65536)))

;;; Gambit source objects

(define (make-source expr file pos)
  (vector '#(source1) expr file pos))

(define (source? obj)
  (and (vector? obj)
       (##fixnum.= (##vector-length obj) 4)
       (let ((v0 (##vector-ref obj 0)))
         (and (vector? v0)
              (##fixnum.= (##vector-length v0) 1)
              (let ((v00 (##vector-ref v0 0)))
                (case v00
                  ((source1 source2) #t)
                  (else #f)))))))

(define (source-value s) (vector-ref s 1))
(define (source-file s)  (vector-ref s 2))
(define (source-pos s)   (vector-ref s 3))

;;; Black hole location objects

(define-type location
  id: 4D19BA4F-D44D-4801-99D4-732E26D72416
  (file read-only:)
  (pos read-only:))

;; A table of
;; * pairs to location objects
;; * vectors to vectors (of equal length) of location objects
;;
;; TODO Make sure that the weak references actually are weak.
(define *location-table* (make-table weak-keys: #t
                                     test: eq?))

(define (vector-for-each/index fn vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (cond
       ((< i len)
        (fn (##vector-ref vec i) i)
        (loop (+ 1 i)))))))

(define (vector-sharing-map/index fn vec)
  (let* ((len (vector-length vec))
         (new-vec #f))
    (vector-for-each/index
     (lambda (orig-val i)
       (let ((val (fn orig-val i)))
         (if (not (eq? orig-val val))
             (begin
               (if (not new-vec)
                   (begin
                     (set! new-vec (make-vector len #f))
                     (let loop ((j 0))
                       (cond
                        ((< j i)
                         (vector-set! new-vec
                                      j
                                      (vector-ref vec j)))))))
               (vector-set! new-vec i val)))))
     vec)
    (or new-vec vec)))

(define (source-cons source a b)
  (let ((res (cons a b))
        (loc (table-ref *location-table* source #f)))
    (if loc (table-set! *location-table* res loc))
    res))

(define (source-map fn input-list)
  (let loop ((list input-list))
    (cond
     ((pair? list)
      (let ((res (cons (fn (car list))
                       (loop (cdr list))))
            (loc (table-ref *location-table* list #f)))
        (if loc (table-set! *location-table* res loc))
        res))

     ((null? list)
      '())

     (else
      (error "Expected list" input-list)))))

(define (source-dotted-map fn input-list)
  (let loop ((list input-list))
    (cond
     ((pair? list)
      (let ((res (cons (fn (car list))
                       (loop (cdr list))))
            (loc (table-ref *location-table* list #f)))
        (if loc (table-set! *location-table* res loc))
        res))

     ((null? list)
      '())

     (else
      (fn list)))))

(define (source-vector-map fn vec)
  (let ((res (vector-map fn vec))
        (loc (table-ref *location-table* vec #f)))
    (if loc (table-set! *location-table* res loc))
    res))

;; Shares as much structure as possible. If the input doesn't contain
;; any source objects, *location-table* isn't altered, and the same
;; object (as in eq?) is returned.
;;
;; Note that, because how this function is implemented, it discards
;; the source location information of the topmost object.
(define (desourcify source)
  (let rec ((source source)
            ;; mark-location is #f or a thunk that takes a location,
            ;; the location of the current source object. It is never
            ;; invoked more than once, and only when source is a pair.
            (mark-location #f))
    (cond
     ((source? source)
      (if mark-location
          (mark-location (make-location
                          (source-file source)
                          (source-pos  source))))
      (rec (source-value source) #f))
     
     ((pair? source)
      ;; A kindof-dotted map that shares structure as much as possible.
      (let loop ((obj source))
        (cond ((pair? obj)
               (let ((a (car obj))
                     (r (cdr obj))

                     (location-mark #f))
                 (let ((a. (rec a
                                (lambda (loc)
                                  (set! location-mark loc))))
                       (r. (loop (cdr obj))))
                   (let ((result-pair
                          (if (and (eq? a a.)
                                   (eq? r r.))
                              obj
                              (cons a. r.))))
                     (if location-mark
                         (table-set! *location-table*
                                     result-pair
                                     location-mark))
                     result-pair))))
              ((null? obj)
               '())
              (else
               (rec obj #f)))))

     ((vector? source)
      (let* ((location-vec #f)

             (result-vec
              (vector-sharing-map/index
               (lambda (val i)
                 (rec val
                      (lambda (loc)
                        (if (not location-vec)
                            (set! location-vec
                                  (make-vector (vector-length source)
                                               #f)))
                        (vector-set! location-vec
                                     i
                                     loc))))
               source)))
        
        (if location-vec
            (table-set! *location-table*
                        result-vec
                        location-vec))
        result-vec))

     (else
      source))))

;; Shares as much structure as possible. If *location-table* doesn't
;; contain any references to the input, the same object (in the sense
;; of eq?) is returned.
(define (resourcify expr)
  (let rec ((expr expr)
            (location #f))
    (let ((result
           (cond
            ((pair? expr)
             (let ((a (car expr))
                   (r (cdr expr)))
               (let ((a. (rec a (table-ref *location-table* expr #f)))
                     (r. (rec r #f)))
                 (if (and (eq? a a.)
                          (eq? r r.))
                     expr
                     (cons a. r.)))))

            ((vector? expr)
             (let ((location-vec (table-ref *location-table* expr #f)))
               (vector-sharing-map/index
                (lambda (val i)
                  (rec val
                       (and location-vec
                            (vector-ref location-vec i))))
                expr)))
            
            (else
             expr))))
      (if location
          (make-source result
                       (location-file location)
                       (location-pos  location))
          result))))


;; Test:
#;(let* ((source
        '#(#(source1)
           (#(#(source1) 1 "test.scm" 65539)
            #(#(source1)
              #(#(#(source1) 2 "test.scm" 327683)
                #(#(source1) 3 "test.scm" 458755)
                #(#(source1)
                  (#(#(source1) 4 "test.scm" 655363)
                   #(#(source1) 5 "test.scm" 786435))
                  "test.scm"
                  589827)
                #(#(source1) 6 "test.scm" 983043))
              "test.scm"
              196611)
            #(#(source1) 7 "test.scm" 1179651)
            #(#(source1) 8 "test.scm" 1310723))
           "test.scm"
           3))
       (source2 (resourcify (desourcify source))))
  (equal? (source-value source)
          source2))

(define (test x)
  (bh#resourcify (expand-macro (bh#desourcify (bh#expr:deep-fixup x)))))

(test `(+ 1))
(test `(begin (+ 1)))
(test `(lambda (a b) (+ 1)))
