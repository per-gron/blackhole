(import ../srfi/13)

(define (make-counter #!optional (serializer (lambda (x) x)))
  (let ((c -1))
    (lambda ()
      (set! c (+ c 1))
      (serializer c))))

(define (counter-stringer str)
  (let ((gssymlen (string-length str)))
    (lambda (x)
      (reverse-list->string
       (let loop ((num x))
         (let ((idx (modulo num gssymlen)))
           (cons (string-ref str idx)
                 (if (eq? idx num)
                     '()
                     (loop (/ (- num idx) gssymlen))))))))))

(define (counter-prefixer c p)
  (lambda (x)
    (string-append p (c x))))

(define (counter-symbolizer c)
  (lambda (x)
    (make-uninterned-symbol
     (c x))))


(define (make-randomizer str #!optional (genlen 7))
  (let ((strlen (string-length str)))
    (lambda ()
      (let* ((ret (make-string genlen)))
        (let loop ((i 0))
          (if (< i genlen)
              (begin
                (string-set! ret i (string-ref str (random-integer strlen)))
                (loop (+ i 1)))))
        ret))))