;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Loader data structure                      ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define-type loader
  id: 786F06E1-BAF1-45A5-B31F-ED09AE93514F

  ;; Returns a module-info object for the given module
  (calculate-info unprintable: equality-skip: read-only:)
  ;; Takes a relative module identifier symbol and optionally an
  ;; origin path, to which the path should be interpreted relative
  ;; to, and returns the object that should be in the path field of a
  ;; module object.
  (path-absolutize unprintable: equality-skip: read-only:)
  ;; Returns what should be in the module-file field of the module
  ;; object.
  (absolute-file unprintable: equality-skip: read-only:)
  ;; Takes a module and returns a string for the name of the module
  (module-name unprintable: equality-skip: read-only:))
