;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                  Module reference data structure                 ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; A module reference object consists of the loader and the module path

(define-type module-reference
  id: 48AC4955-EC9E-466F-B8EF-B7F0B9BBC63D

  ;; The loader object
  (loader read-only:)
  ;; The absolute module path. It must be a hashable object.
  (path read-only:))

