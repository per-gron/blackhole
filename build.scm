(##namespace ("module#"))

(##include "~~/lib/gambit#.scm")

(define *blackhole-path* (getenv "BLACKHOLE_PATH" "~~/lib/modules"))

(declare
 (standard-bindings)
 (extended-bindings)
 (block))

;; Utility functions. There because I can't include libraries from here
(##include "util.scm")

;; Library for handling Gambit source objects
(##include "expr.scm")

;; The syntax closures implementation
(##include "hygiene.scm")

;; The syntax-rules implementation
(##include "syntax-rules.scm")

;; The module system core
(##include "module.scm")

;; The lib (fetching remote modules) implementation
(##include "lib.scm")

;; Some utilities, for instance module-compile/deps!
(##include "extras.scm")



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

;; Add cond-expand feature by default
(set! ##cond-expand-features (cons 'black-hole ##cond-expand-features))

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
