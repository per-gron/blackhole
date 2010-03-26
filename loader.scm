;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Loader data structure                      ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; Was previously (TODO Remove)
;; (define-type loader
;;   id: 786F06E1-BAF1-45A5-B31F-ED09AE93514F
;;  
;;   ;; Returns a module-info object for the given module
;;   (calculate-info unprintable: equality-skip: read-only:)
;;   (path-absolutize unprintable: equality-skip: read-only:)
;;   ;; Returns what should be in the module-file field of the module
;;   ;; object.
;;   (absolute-file unprintable: equality-skip: read-only:)
;;   (module-name unprintable: equality-skip: read-only:))


(define-type loader
  id: 786F06E1-BAF1-45A5-B31F-ED09AE93514F

  (path-absolute? unprintable: equality-skip: read-only:)
  ;; Takes a possible relative path and an origin path to which the
  ;; path should be interpreted relative to, and returns an absolute
  ;; path object.
  (path-absolutize unprintable: equality-skip: read-only:)
  (load-module unprintable: equality-skip: read-only:)
  ;; Takes a module and returns a string for the name of the module
  (module-name unprintable: equality-skip: read-only:))

