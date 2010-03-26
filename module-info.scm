;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                     Module info data structure                   ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; Previous definition: (TODO remove this)
;;  
;; (define-type module-info
;;   id: 726DB40B-AB18-4396-A570-BB715B602DB9
;;  
;;   (symbols read-only:)
;;   (exports read-only:)
;;   (imports read-only:)
;;   (uses read-only:)
;;  
;;   (options read-only:)
;;   (cc-options read-only:)
;;   (ld-options-prelude read-only:)
;;   (ld-options read-only:)
;;   (force-compile? read-only:)
;;  
;;   (environment read-only:))

(define-type module-info
  id: 726DB40B-AB18-4396-A570-BB715B602DB9
  constructor: make-module-info/internal

  (exports read-only:)
  
  (runtime-dependencies read-only:)
  (compiletime-dependencies read-only:)

  (options read-only:)
  (cc-options read-only:)
  (ld-options-prelude read-only:)
  (ld-options read-only:)
  (force-compile? read-only:)

  (namespace read-only:)

  (environment read-only:))

;; To be implemented:
;; (define (make-module-info module-info-alist) ...)

