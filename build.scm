(##namespace ("module#"))

(##include "~~/lib/gambit#.scm")

(declare (standard-bindings)
	 (extended-bindings)
	 (block))


(##include "tree.scm")                  ;; A functional weighted tree data structure
(##include "util.scm")                  ;; Utility functions.
(##include "expr.scm")                  ;; Library for handling Gambit
                                        ;; source objects
(##include "syntactic-tower.scm")       ;; Syntactic tower data structure
(##include "hygiene.scm")               ;; The hygiene system implementation
(##include "core-forms.scm")            ;; Core forms
(##include "syntax-rules.scm")          ;; The syntax-rules implementation
(##include "resolvers.scm")             ;; Module, import and export resolvers
(##include "namespace.scm")             ;; Namespace choice functionality
(##include "compile-load.scm")          ;; Utilities for compiling and loading
(##include "module-reference.scm")      ;; Module reference data structure
(##include "module-info.scm")           ;; Module info data structure
(##include "loader.scm")                ;; Loader data structure
(##include "loaded-module.scm")         ;; Loaded module data structure
(##include "module-macroexpansion.scm") ;; The module system core
(##include "extras.scm")                ;; Some utilities, for instance
                                        ;; module-compile/deps!
(##include "lib.scm")                   ;; The lib (fetching remote
                                        ;; modules) implementation



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

(define (apply-hooks!)
  (let ((hook
         (lambda (src)
           (let ((ret (expr:deep-fixup
                       (suspend-ns-table-changes
                        (lambda ()
                          (expand-macro src))))))
             ;; Useful when debugging
             ;; (pp (expr*:strip-locationinfo ret))
             ret))))
    (set! ##expand-source hook)
    (set! c#expand-source hook))

  (##vector-set!
   (##thread-repl-channel-get! (current-thread))
   6
   (lambda (channel level depth)
     (with-exception-catcher
      (lambda (e)
        (print e))
      (lambda ()
        (let ((mod (environment-module-reference (*top-environment*))))
          (if mod
              (begin
                (print (loader-module-name (module-reference-loader mod)
                                           (module-reference-path mod)))
                (if (##fixnum.< 0 level)
                    (print "/")))))))
     (##repl-channel-ports-read-command channel level depth))))

(define (start-repl! #!key suppress-message)
  (apply-hooks!)
  (if (not suppress-message)
      (println "Gambit-C with Black Hole"))
  (##repl))




(define (module-files-args)
  (if (null? (cdr (command-line)))
      '()
      (cddr (command-line))))

(define (module-file-arg)
  (car (module-files-args)))

(define (read-string str)
  (with-input-from-string str
    (lambda ()
      (read))))

(define (module-reference-prettyprint ref)
  (println (module-reference-path ref)))

#;(let ((command
       (if (null? (cdr (command-line)))
           'repl
           (string->symbol (cadr (command-line))))))
  (case command
    ((repl)
     (start-repl!))
    ((dependencies)
     (for-each (lambda (module)
                 (for-each module-reference-prettyprint
                   (module-deps module)))
       (resolve-modules
        (map read-string (module-files-args)))))
    (else
     (error "Unknown command"))))

(apply-hooks!)
