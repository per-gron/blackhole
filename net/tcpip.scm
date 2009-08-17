;;; TCP/IP utilities.
;;;
;;; Copyright (c) 2008 Mikael Möre, 1994-2009 Marc Feeley, All Rights
;;; Reserved.

(import ../string/util)

(define (call-with-tcp-client hostname port-num proc)
  (let ((port #f))
    (dynamic-wind
        (lambda ()
          (if port
              (error "Cannot re-enter call-with-tcp-client"))
          (set! port
                (open-tcp-client
                 (list server-address: hostname
                       port-number: port-num))))
        (lambda ()
          (proc port))
        (lambda ()
          (close-port port)))))

(define (ipv4->u8vector ip)
  (let ((v (string-split #\. ip))
        (o (make-u8vector 4)))
    
    (if (not (= (length v) 4))
        (error "Invalid IPv4 IP number!" ip))
    
    (list->u8vector (map string->number v))))

;;; (More or less extracted from ~~/lib/_io#.scm)

(define (device-port-rdevice-condvar port)
  (##vector-ref port 44))
(define (device-port-wdevice-condvar port)
  (##vector-ref port 45))
(define (port-rtimeout port)
  (##vector-ref port 11))
(define (port-wtimeout port)
  (##vector-ref port 15))

(define (port-wait-for-input port)
  (##wait-for-io!
   (device-port-rdevice-condvar port)
   (port-rtimeout port)))

(define (port-wait-for-output port)
  (##wait-for-io!
   (device-port-wdevice-condvar port)
   (port-wtimeout port)))
