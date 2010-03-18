;; TODO
;; * implementera include
;; * Den kompilerar inte om dependencies när det behövs (pga
;;   makro-expansion)
;; * Stöd för att kompilera till annan mapp
;; * Gör en funktion som söker och visar vilka moduler som
;;   definierar en viss symbol
;; * Lägg till stöd för att detektera namespace-konflikter
;; * Jag borde nästan helt kasta expr.scm
;; * Kolla möjligheten att implementera kompatibilitetslager för R6RS
;; * Ändra så att inget som krävs för runtime i build är exporterat by
;;   default i moduler. Däremot ska allt vara importerat by default i
;;   REPLen.
;;
;; Enkel TODO
;; * Implementera lib
;; * Källkodsplatser
;; * do, time, parameterize form för hygiensystemet

;; Design limitations
;; * Just nu om man har en fil som kräver kompilering kommer
;;   den att kompileras när den (use)as, oavsett om det redan
;;   är en kompilering igång. Detta kraschar Gambit.
;;     Compile-all-modules går runt denna begränsning, så såvitt
;;   jag vet visar sig detta endast när man har en fil som
;;   kräver kompilering som (use)ar en annan fil som kräver
;;   kompilering, då (use) av den första filen alltid kommer
;;   krascha om inte den andra redan är kompilerad.
;; * #!key parameters är omöjligt att implementera hygien till.

;; Feature requests
;; * Something to be able to work around #!key parameter hygiene

(##namespace ("module#"))

(##include "~~/lib/gambit#.scm")

(declare (standard-bindings)
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

;;(set! *compiler-options* '(debug))
;;(set! *compiler-cc-options* "-I/usr/local/BerkeleyDB.4.7/include")
;;(set! *compiler-ld-options-prelude* "-L/usr/local/BerkeleyDB.4.7/lib")

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

