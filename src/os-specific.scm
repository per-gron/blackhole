;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                      OS-specific Definitions                     ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define windows?
  (char=? #\\ (let ((cd (current-directory))) 
                (string-ref cd (- (string-length cd) 1)))))

(define null-device
  (if windows?
      "\\\\.\\NUL"
      "/dev/null"))
