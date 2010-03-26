;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                    Loaded module data structure                  ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define-type loaded-module
  id: 08E43248-AC39-4DA1-863E-49AFA9FFE84E

  (instantiate-runtime read-only:)
  (instantiate-compiletime read-only:)
  (info read-only:)
  (loader read-only:)
  (path read-only:))

;; To be implemented
;; (define (load-module path loader) ...)
