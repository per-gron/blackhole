;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                  Module reference data structure                 ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; A module reference object consists of the loader and the module path

(define-type module
  id: 48AC4955-EC9E-466F-B8EF-B7F0B9BBC63D

  ;; The loader object
  (loader read-only:)
  ;; The absolute module path. It must be a hashable object.
  (path read-only:)
  ;; The namespace, lazily initialized
  (ns unprintable: init: #f equality-skip:)
  ;; An absolute file name that uniqely identifies this particular
  ;; module. This file needs to exist as long as the module exists on
  ;; this particular machine. Lazily initialized
  (file module-file-lazy module-file-set! unprintable: init: #f equality-skip:))

(define (module-file mod)
  (or (module-file-lazy mod)
      (let ((val ((loader-absolute-file
                   (module-loader mod)) mod)))
        (module-file-set! mod val)
        val)))
