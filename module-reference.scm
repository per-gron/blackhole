;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                  Module reference data structure                 ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; A module reference object consists of the loader and the module path

(define-type module-reference
  id: 48AC4955-EC9E-466F-B8EF-B7F0B9BBC63D
  constructor: make-module-reference/internal

  ;; The loader object
  (loader read-only:)
  ;; The absolute module path. It must be a hashable object.
  (path read-only:))

(define (make-module-reference loader path)
  (if (not (loader? loader))
      (error "Invalid parameters"))
  (make-module-reference/internal loader path))

(define (module-reference-absolute? module-reference)
  ((loader-path-absolute? module-reference)
   (module-reference-path module-reference)))

(define (module-reference-absolutize module-reference ref)
  (cond
   ((module-reference-absolute? module-reference)
    module-reference)

   ((not (equal? (module-reference-loader module-reference)
                 (module-reference-loader ref)))
    (error "Cannot absolutize a relative module reference with \
            respect to a module reference with a different module \
            loader"))

   (else
    ((loader-path-absolutize (module-reference-loader ref))
     (module-reference-path module-reference)
     (module-reference-path ref)))))

(define (module-reference->u8vector ref)
  (object->u8vector ref
                    (lambda (obj)
                      (if (loader? obj)
                          (loader->skeleton obj)
                          obj))))

(define (u8vector->module-reference vec)
  (u8vector->object vec
                    (lambda (obj)
                      (if (loader? obj)
                          (skeleton->loader obj)
                          obj))))
