;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                    Loaded module data structure                  ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define-type loaded-module
  id: 08E43248-AC39-4DA1-863E-49AFA9FFE84E
  constructor: make-loaded-module/internal

  (instantiate-runtime read-only:)
  (instantiate-compiletime read-only:)
  (info read-only:)
  (loader read-only:)
  (path read-only:))

(define (make-loaded-module #!key
                            instantiate-runtime
                            instantiate-compiletime
                            info
                            loader
                            path)
  (if (not (and (procedure? instantiate-runtime)
                (procedure? instantiate-compiletime)
                (module-info? info)
                (module-loader? loader)))
      (error "Invalid parameters"))
  (make-loaded-module/internal instantiate-runtime
                               instantiate-compiletime
                               info
                               loader
                               path))

;; To be implemented
;; (define (load-module path loader) ...)
