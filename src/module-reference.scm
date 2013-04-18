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

(define (module-reference<? a b)
  (if (not (and (module-reference-absolute? a)
                (module-reference-absolute? b)))
      (error "Module references must be absolute" a b))
  (let ((la (module-reference-loader a))
        (lb (module-reference-loader b)))
    (or (loader<? la lb)
        (and (equal? la lb)
             ;; It's really dirty to assume that module-reference-path
             ;; is a string...
             ((loader-path<?-fn la) ;; Since la = lb, it doesn't
                                    ;; matter which one we choose
              (module-reference-path a)
              (module-reference-path b))))))

(define (current-loader)
  (let ((cm (current-module-reference)))
    (if cm
        (module-reference-loader (current-module-reference))
        local-loader)))

(define (module-reference-absolute? module-reference)
  (loader-path-absolute? (module-reference-loader module-reference)
                         (module-reference-path module-reference)))

(define (module-reference-absolutize module-reference ref)
  (cond
   ((module-reference-absolute? module-reference)
    module-reference)

   ((not (equal? (module-reference-loader module-reference)
                 (module-reference-loader ref)))
    (error "Cannot absolutize a relative module reference with respect to a module reference with a different module loader" module-reference ref))

   (else
    (make-module-reference
     (module-reference-loader module-reference)
     (loader-path-absolutize (module-reference-loader ref)
                             (module-reference-path module-reference)
                             (module-reference-path ref))))))

(define module-reference-namespace
  (let ((fn
         (lambda (mod)
           (let ((mod (resolve-one-module mod)))
             (if (not (module-reference-absolute? mod))
                 (error "Module reference must be absolute" mod))
             
             (string-append
              (let ((loader (module-reference-loader mod))
                    (path (module-reference-path mod)))
                (if (eq? loader black-hole-module-loader)
                    "bh"
                    (namespace-choose-unique
                     (loader-module-name loader path)
                     (loader-real-path loader path))))
              "#")))))
    (lambda (mod)
      ;; This function might be called with #f as argument
      (if mod (fn mod) "~#"))))

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
