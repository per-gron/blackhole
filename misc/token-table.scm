;;; Extracted from http.scm HTTP server by Per Eckerdal

;==============================================================================

; File: "http.scm", Time-stamp: <2007-04-04 14:42:59 feeley>

; Copyright (c) 2005-2007 by Marc Feeley, All Rights Reserved.

;==============================================================================

(declare
  (standard-bindings)
  (extended-bindings)
  (block))

;==============================================================================

; Token tables.

(define hash-substring
  (lambda (str start end)

    (define loop
      (lambda (h i)
        (if (< i end)
            (loop (modulo (+ (* h 5063) (char->integer (string-ref str i)))
                          65536)
                  (+ i 1))
            h)))

    (loop 0 start)))

(define-macro (make-token-table . alist)

    ; "alist" is a list of lists of the form "(string expression)"

    ; The result is a perfect hash-table represented as a vector of
    ; length 2*N, where N is the hash modulus.  If the string S is in
    ; the hash-table it is at index
    ;
    ;   X = (* 2 (modulo (hash-substring S 0 (string-length S)) N))
    ;
    ; and the associated expression is at index X+1.

    (define hash-substring    ; repeated from above to be
      (lambda (str start end) ; available for macro expansion

        (let loop ((h 0) (i start))
            (if (< i end)
                (loop (modulo (+ (* h 5063) (char->integer (string-ref str i)))
                              65536)
                      (+ i 1))
                h))))

    (define make-perfect-hash-table
      (lambda (alist)
        (let loop1 ((n (length alist)))
          (let ((v (make-vector (* 2 n) #f)))
            (let loop2 ((lst alist))
              (if (pair? lst)
                  (let* ((x (car lst))
                         (str (car x)))
                    (let ((h
                           (* 2
                              (modulo (hash-substring str 0 (string-length str))
                                      n))))
                      (if (vector-ref v h)
                          (loop1 (+ n 1))
                          (begin
                            (vector-set! v h str)
                            (vector-set! v (+ h 1) (cadr x))
                            (loop2 (cdr lst))))))
                  v))))))
    
    (cons 'vector (vector->list (make-perfect-hash-table alist))))

(define token-table-lookup-substring
  (lambda (table str start end)
    (let* ((n (quotient (vector-length table) 2))
           (h (* 2 (modulo (hash-substring str start end) n)))
           (x (vector-ref table h)))

      (define loop
        (lambda (i j)
          (if (< i end)
              (if (char=? (string-ref str i) (string-ref x j))
                  (loop (+ i 1) (+ j 1))
                  #f)
              h)))
      
      (and x
           (= (string-length x) (- end start))
           (loop start 0)))))

(define token-table-lookup-string
  (lambda (table str)
    (token-table-lookup-substring table str 0 (string-length str))))

