;; TODO
;; * Det saknas macro-use
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
;; * byt namn build => module

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

;; Some utilities, for instance module-compile/deps!
(##include "extras.scm")


;; Add the hooks =)

(define module-hook (make-parameter (lambda (src compiling?) src)))

(let ((hook (lambda (compiling?)
              (lambda (src)
                (let ((ret (expr:deep-fixup
                            ((module-hook)
                             (expand-macro src)
                             compiling?))))
                  ;; Useful when debugging
                  ;;(pp (expr*:strip-locationinfo ret))
                  ret)))))
  (set! ##expand-source (hook #f))
  (set! c#expand-source (hook #t)))

