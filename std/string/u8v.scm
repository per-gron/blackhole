;;; u8vector utilities, mostly string-related.
;;;
;;; Copyright (c) 2008 Mikael Möre

(define (hex-char->integer c)
  (let ((i (char->integer c)))
    (cond
     ((<= 48 i 57)
      (- i 48))
     ((<= 97 i 102)
      (+ (- i 97) 10))
     ((<= 65 i 70)
      (+ (- i 65) 10))
     (else
      (error "Character not 0-9, A-F, a-f." c)))))

(define (u8vector->string v
                          #!key
                          (char-encoding 'UTF-8)
                          (ignore-errors-in-encoding #f))
  (call-with-input-u8vector
   `(char-encoding: ,char-encoding
                    eol-encoding: cr-lf
                    init: ,v)
   (lambda (input-port)
     (call-with-output-string
      '()
      (lambda (output-port)
        (let loop ()
          (let ((c
                 (with-exception-catcher
                  (lambda (e)
                    (if ignore-errors-in-encoding
                        #f
                        (raise
                         (dumps "Failed to read u8vector, broken "
                                "characters? Data: " v " (end)."))))
                  (lambda ()
                    (read-char input-port)))))
            ; (dbg "Had " c)
            (if (not (eq? c #!eof))
                (begin
                  (if c (write-char c output-port))
                  (loop))))))))))

(define (string->utf8-u8vector s)
  (with-output-to-u8vector
   '(char-encoding: UTF-8 eol-encoding: cr-lf)
   (lambda ()
     (display s))))

(define (u8vector-reverse v)
  (let* ((l (u8vector-length v))
         (r (make-u8vector l)))
    (let loop ((src-idx 0) (target-idx (- l 1)))
      (u8vector-set! r target-idx (u8vector-ref v src-idx))
      (if (> target-idx 0)
          (loop (+ src-idx 1) (- target-idx 1))))
    r))

(define (ISO-8859-1-substring->u8vector str start end)
  (let* ((len (- end start))
         (u8vect (make-u8vector len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (u8vector-set!
             u8vect
             i
             (char->integer (string-ref str (+ start i))))
            (loop (+ i 1)))
          u8vect))))

(define (ISO-8859-1-string->u8vector str)
  (ISO-8859-1-substring->u8vector
   str
   0
   (string-length str)))

(define (subu8vector->ISO-8859-1-string u8vect start end)
  (let* ((len (- end start))
         (str (make-string len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (string-set!
             str
             i
             (integer->char (u8vector-ref u8vect (+ start i))))
            (loop (+ i 1)))
          str))))

(define (u8vector->ISO-8859-1-string u8vect)
  (subu8vector->ISO-8859-1-string
   u8vect
   0
   (u8vector-length u8vect)))

(define (subu8vector-move! src src-start src-end dst dst-start)
  ;; Copy direction must be selected in case src and dst are the same
  ;; vector.
  (if (< src-start dst-start)
      (let loop1 ((i (- src-end 1))
                  (j (- (+ dst-start (- src-end src-start)) 1)))
        (if (< i src-start)
            dst
            (begin
              (u8vector-set! dst j (u8vector-ref src i))
              (loop1 (- i 1)
                     (- j 1)))))
      (let loop2 ((i src-start)
                  (j dst-start))
        (if (< i src-end)
            (begin
              (u8vector-set! dst j (u8vector-ref src i))
              (loop2 (+ i 1)
                     (+ j 1)))
            dst))))


(define (subu8vector->hex-string u8vect start end)
  (define (digit->char d)
    (string-ref "0123456789abcdef" d))

  (let* ((len (- end start))
         (n (* len 2))
         (str (make-string n)))
    (let loop ((i 0) (j (- len 1)) (k 0))
      (if (>= j 0)
          (let ((x (u8vector-ref u8vect k)))
            (string-set! str i (digit->char (quotient x 16)))
            (string-set! str (+ i 1) (digit->char (modulo x 16)))
            (loop (+ i 2) (- j 1) (+ k 1)))
          str))))

(define (u8vector->hex-string u8vect)
  (subu8vector->hex-string
   u8vect
   0
   (u8vector-length u8vect)))


(define (hex-string->u8vector s)
  (let* ((l (string-length s)))
    (if (= (modulo l 2) 1) (error "String length not multiple of two" s))
    (let* ((c (/ l 2))
           (v (make-u8vector c)))
  (let loop ((i 0))
    (let* ((m (* i 2))
           (a (hex-char->integer (string-ref s m)))
           (b (hex-char->integer (string-ref s (+ m 1))))
           (n (+ (* a 16) b)))
      (u8vector-set! v i n)
      (let ((i (+ i 1)))
        (if (not (eq? i c))
            (loop i)))))
      v)))

(define (u8vector-invert! v)
  (let loop ((i (u8vector-length v)))
      (if (not (zero? i))
          (let ((i (- i 1)))
            (u8vector-set! v i (##fixnum.bitwise-xor
                                (u8vector-ref v i)
                                255))
            (loop i)))))


(define (dump-u8vector-port-to-other-u8vector-port content-in
                                                   #!optional
                                                   (content-out '()))
  (if (null? content-out)
      (call-with-output-u8vector
       '()
       (lambda (port)
         (dump-u8vector-port-to-other-u8vector-port content-in port)))

      (let* ((tmp-bufsize (* 50 1024))
             (tmp-buffer (make-u8vector tmp-bufsize)))
        (let loop ()
          (let ((n (read-subu8vector tmp-buffer 0 tmp-bufsize content-in)))
            (if (> n 0)
                (begin
                  (write-subu8vector tmp-buffer 0 n content-out)
                  (loop))))))))
