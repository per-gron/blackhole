;; A very simple and non-evolved, fixed-size circular buffer based on a vector

(export make-circular-buffer
        circular-buffer-length
        circular-buffer-fill!
        circular-buffer-push!
        circular-buffer-first
        circular-buffer-last)

(define-type circular-buffer
  constructor: make-circular-buffer-internal
  (vec read-only:)
  pos)

(define (make-circular-buffer size #!optional (init 0))
  (make-circular-buffer-internal (make-vector size init)
                                 0))

(define (circular-buffer-length buf)
  (vector-length
   (circular-buffer-vec buf)))

(define (circular-buffer-fill! buf val)
  (vector-fill! (circular-buffer-vec buf)
                val))

(define (circular-buffer-push! buf val)
  (let* ((vec (circular-buffer-vec buf))
         (new-pos (modulo (+ 1 (circular-buffer-pos buf))
                          (vector-length vec))))
    (circular-buffer-pos-set! buf new-pos)
    (vector-set! vec new-pos val)))

(define (circular-buffer-ref buf pos)
  (let ((vec (circular-buffer-vec buf)))
    (vector-ref vec
                (modulo pos
                        (vector-length vec)))))

(define (circular-buffer-first buf)
  (circular-buffer-ref buf 0))

(define (circular-buffer-last buf)
  (circular-buffer-ref buf -1))
