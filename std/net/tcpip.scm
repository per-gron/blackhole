;;; TCP/IP utilities.
;;;
;;; Copyright (c) 2008 Mikael Möre

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
