;;; Copyright (c) 1994-2009 by Marc Feeley, All Rights Reserved.
;;;
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

