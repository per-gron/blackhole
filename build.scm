(##namespace ("module#"))

(##include "~~/lib/gambit#.scm")

(declare (standard-bindings)
	 (extended-bindings)
	 (block))


(##include "util.scm")             ;; Utility functions. There because
                                   ;; I can't include libraries from
                                   ;; here
(##include "expr.scm")             ;; Library for handling Gambit source objects
(##include "hygiene.scm")          ;; The syntax closures implementation
(##include "syntax-rules.scm")     ;; The syntax-rules implementation
(##include "resolvers.scm")        ;; Module, import and export resolvers
(##include "namespace.scm")        ;; Namespace choosing functionality
(##include "compile-load.scm")     ;; Utilities for compiling and loading
(##include "syntactic-tower.scm")  ;; Syntactic tower data structure
(##include "module-reference.scm") ;; Module reference data structure
(##include "module-info.scm")      ;; Module info data structure
(##include "loader.scm")           ;; Loader data structure
(##include "module.scm")           ;; The module system core
(##include "lib.scm")              ;; The lib (fetching remote modules) implementation
(##include "extras.scm")           ;; Some utilities, for instance module-compile/deps!



;;;; ---------- Hack for configuration ----------

;; Variables declared here are used all over the place.

;; Configuration directives
(define *compiler-options* '())
(define *compiler-cc-options* "")
(define *compiler-ld-options-prelude* "")
(define *compiler-ld-options* "")

(set! *module-resolvers*
      `((here . ,current-module-resolver)
        (module . ,(make-singleton-module-resolver
                    module-module-loader))
        (lib . ,lib-module-resolver)
        (std . ,(package-module-resolver "~~lib/modules/std"))))


;; ---------- Add the hooks =) ----------

(let ((hook (lambda (compiling?)
              (lambda (src)
                (let ((ret (expr:deep-fixup
                            (expand-macro src))))
                  ;; Useful when debugging
                  ;; (pp (expr*:strip-locationinfo ret))
                  ret)))))
  (set! ##expand-source (hook #f))
  (set! c#expand-source (hook #t)))

(##vector-set!
 (##thread-repl-channel-get! (current-thread))
 6
 (lambda (channel level depth)
   (let ((mod (environment-module (top-environment))))
     (if mod
         (begin
           (print ((loader-module-name (module-loader mod)) mod))
           (if (##fixnum.< 0 level)
               (print "/")))))
   (##repl-channel-ports-read-command channel level depth)))
