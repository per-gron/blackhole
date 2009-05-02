;;; TCP/IP utilities.
;;;
;;; Copyright (c) 2008 Mikael Möre, 1994-2009 Marc Feeley, All Rights
;;; Reserved.

(import ../string/util
        portutil)

(define (call-with-tcp-client hostname port-num proc)
    (let* ((port
            (open-tcp-client
             (list server-address: hostname
                   port-number: port-num)))
           (result
            (proc port)))
      (close-port port)
      result))

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
