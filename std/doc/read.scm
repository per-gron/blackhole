(private)

;; The following four functions need that _gambit#.scm is included:
(##include "~~/lib/_gambit#.scm")

(define (read-next-char-or-eof re) ;; possibly returns end-of-file
  (macro-read-char (macro-readenv-port re)))

(define (peek-next-char-or-eof re) ;; possibly returns end-of-file
  (macro-peek-char (macro-readenv-port re)))

(define (readenv-filepos-set! re pos)
  (macro-readenv-filepos-set! re pos))

(define (readenv-wrap re val)
  (macro-readenv-wrap re val))


(define (read-scmd-comment read read-char peek-char)
  (read-char) ;; Ignore the ;
  (let ((peek (peek-char)))
    (if (eq? #\{ peek)
        (read)
        (let loop ()
          (let ((chr (read-char)))
            (if (and (char? chr)
                     (not (eq? chr #\newline)))
                (loop))))))
  #f)

(define (read-scmd-at-form read read-char peek-char)
  (let ((peek (peek-char)))
    (if (eq? peek #\;)
        (read-scmd-comment read read-char peek-char)
        (let* ((only-command #t)
               (res
                (let loop ((so-far (list (read))))
                  (let ((peek (peek-char)))
                    (cond
                     ((eq? #\[ peek)
                      (set! only-command #f)
                      (loop (append so-far
                                     (read))))
                      
                      ((eq? #\{ peek)
                       (set! only-command #f)
                       (loop (append so-far
                                     (read))))
                      
                      (else
                       so-far))))))
          
           ;; Handle the special case when no [] or {} is present
           (if only-command
               (car res)
               res)))))

(define (read-scmd-stringblock read read-char peek-char #!key ignore-braces)
  (letrec ((res '())
           (string-out (open-output-string ""))
           (push
            (lambda (x)
              (let ((str (get-output-string string-out)))
                (if (positive?
                     (string-length str))
                    (push str)))
              (set! res
                    (cons x res)))))
    (let loop ((brace-nesting 0))
      (let ((chr (read-char)))
        (cond
         ((eq? #!eof chr)
          #t)

         ((and (eq? #\} chr)
               (zero? brace-nesting)
               (not ignore-braces))
          #t)
         
         ((eq? #\@ chr)
          (let ((ret (read-scmd-at-form read read-char peek-char)))
            (if ret
                (push ret)))
          (loop brace-nesting))
         
         (else
          (write-char chr string-out)
          (loop (+ brace-nesting
                   (cond
                    ((eq? #\{ chr) 1)
                    ((eq? #\} chr) -1)
                    (else 0))))))))
    (push '()) ;; Make sure string-out is flushed.
    (reverse (cdr res))))


(/private)

(define (make-scmd-readtable)
  (let ((rt (##make-standard-readtable))
        (fn (lambda (fun)
              (lambda (re c)
                (let ((start-pos (##readenv-current-filepos re)))
                  (read-next-char-or-eof re) ;; skip #\{ or #\@
                  (let ((res (fun
                              (lambda ()
                                (##desourcify (##read-datum-or-eof re)))
                              (lambda ()
                                (read-next-char-or-eof re))
                              (lambda ()
                                (peek-next-char-or-eof re)))))
                    ;; set pos to start of datum
                    (readenv-filepos-set! re start-pos)
                    (readenv-wrap re res)))))))
    (##readtable-char-class-set! rt
                                 #\@
                                 #t
                                 (fn read-scmd-at-form))
    (##readtable-char-class-set! rt
                                 #\{
                                 #t
                                 (fn read-scmd-stringblock))
    rt))

(define scmd-readtable (make-scmd-readtable))

(define (with-scmd-readtable thunk)
  (let* ((p (current-input-port))
         (old-rt (input-port-readtable p)))
    (dynamic-wind
        (lambda ()
          (input-port-readtable-set! p scmd-readtable))
        thunk
        (lambda ()
          (input-port-readtable-set! p old-rt)))))

(define (read-scmd)
  (with-scmd-readtable
   (lambda ()
     (read-scmd-stringblock read
                            read-char
                            peek-char
                            ignore-braces: #t))))

(define (read-scmd-string str)
  (with-input-from-string
      str
    (lambda ()
      (read-scmd))))
