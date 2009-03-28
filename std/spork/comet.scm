(import (termite)
        ../srfi/1
        ../net/http-server
        js
        widget
        core)

(export comet-js-module
        
        comet-connect
        comet-connection-do
        comet-connection-close!
        
        make-comet
        comet?
        comet-add!
        comet-remove!
        comet-send
        comet-close!)

(define comet-js-module
  (js-module ()
      (core-js-module)
    
    (define (on-data-fun xhr)
      (let ((pos 256))
        (lambda ()
          (let loop ()
            (let* ((str (xhr.responseText.substr pos))
                   (nl-pos (str.indexOf "\n")))
              (if (not (eq? -1 nl-pos))
                  (let* ((len (parseInt (str.substr 0 nl-pos)))
                         (data-pos (+ nl-pos 1))
                         (end-pos (+ data-pos len)))
                    (if (>= str.length (+ data-pos len))
                        (begin
                          (eval (str.substr data-pos
                                            end-pos))
                          (set! pos (+ pos end-pos))
                          (loop))))))))))
    
    (set! Comet
     (lambda (address)
       ;; The setTimeout is a hack to avoid loading bars in WebKit
       ;; (at least). There might me a more elegant solution to this.
       (setTimeout
        (lambda ()
          (let* ((xhr (new XMLHttpRequest))
                 (on-data (on-data-fun xhr)))
            (xhr.open "POST" address #t)
            (set! xhr.onreadystatechange
                  (lambda ()
                    (if (eq? 3 .readyState)
                        (on-data))))
            (xhr.send null)))
        20)))))


(define x*256 "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\
               xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\
               xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\
               xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\
               xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")

(define-type comet-connection
  id: 985E1855-B9EA-48BD-B1A1-F99F13F67EE7
  constructor: comet-conn-constructor
  predicate: comet-conn-pred
  process)

(define comet-connection? comet-conn-pred)

(define (comet-connect setter
                       #!optional
                       keepalive-msg
                       webkit-workaround)
  (call/cc
   (lambda (ret)
     (reply-chunk
      (lambda (send-chunk)
        (call/cc
         (lambda (close)
           ;; Work around limitation of Webkit
           (if webkit-workaround
               (send-chunk (lambda () (display x*256))))


           (let* ((conn (comet-conn-constructor #f))
                  (send
                   (lambda (str)
                     (with-exception-catcher
                      (lambda (e)
                        (comet-connection-process-set! conn #f)
                        (close))
                      (lambda ()
                        (send-chunk
                         (lambda ()
                           (let ((vec (with-output-to-u8vector
                                       '(char-encoding: UTF-8 eol-encoding: cr-lf)
                                       (lambda () (display str)))))
                           (display (number->string
                                     (u8vector-length vec)))
                           (display "\n")
                           (write-subu8vector vec 0 (u8vector-length vec))))))))))
             
             (comet-connection-process-set!
              conn
              (spawn
               (lambda ()
                 (let loop ()
                   (recv
                    (('send str)
                     (send str))
                    
                    ('close
                     (close))
                    
                    (after 60
                           (if keepalive-msg
                               (send keepalive-msg))))
                   (loop)))))
             
             (cond
              ((comet? setter)
               (comet-add! setter conn))
              
              ((procedure? setter)
               (setter conn))

              (else
               (error "Invalid argument to comet-connect" setter))))
           
           (ret))))
      code: 200
      headers: '((content-type . application/x-socio-stream)))))
  (spork-die))

(define (comet-connection-do comet js)
  (let ((proc (comet-connection-process comet)))
    (if proc
        (begin
          (! (comet-connection-process comet) (list 'send js))
          #t)
        #f)))

(define (comet-connection-close! comet)
  (! (comet-connection-process comet) 'close))


(define-type comet
  id: 6F5D65A1-6363-4AA7-AA79-4C43C0E1DE7D
  constructor: comet-constructor
  predicate: comet-pred
  conns
  mutex)

(define (with-comet! cmt thunk)
  (let ((mtx (comet-mutex cmt)))
    (dynamic-wind
        (lambda ()
          (mutex-lock! mtx))
        thunk
        (lambda ()
          (mutex-unlock! mtx)))))

(define (make-comet)
  (comet-constructor '() (make-mutex)))

(define comet? comet-pred)

(define (comet-add! comet conn)
  (with-comet!
   comet
   (lambda ()
     (comet-conns-set!
      comet
      (cons conn
            (comet-conns comet))))))

(define (comet-remove! comet conn)
  (with-comet!
   comet
   (lambda ()
     (comet-conns-set!
      comet
      (delete! comet
               (comet-conns comet)
               eq?)))))

(define (comet-send comet js)
  (with-comet!
   comet
   (lambda ()
     (comet-conns-set!
      comet
      (filter! (lambda (x)
                 (comet-connection-do x js))
               (comet-conns comet))))))

(define (comet-close! comet)
  (with-comet!
   comet
   (lambda ()
     (for-each (lambda (x)
                 (comet-connection-close! x))
               (comet-conns comet))
     (comet-conns-set! comet '()))))
