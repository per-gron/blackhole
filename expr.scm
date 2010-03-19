;; Copyright 2006-2009 by Christian Jaeger, <christian at pflanze mine nu>

;; Published under the terms of either the GNU LGPL Version 2.1 or the
;; Apache License Version 2.0; you can choose whichever license you
;; prefer.

(define (map* fn obj #!optional (tail '()))
  (let rec ((obj obj))
    (cond ((pair? obj)
	   (cons (fn (car obj))
		 (rec (cdr obj))))
	  ((null? obj)
	   tail)
	  (else
	   (fn obj)))))

;; a map* that shares structure as much as possible
(define (map** fn obj #!optional (tail '()))
  (let rec ((obj obj))
    (cond ((pair? obj)
	   (let ((a (car obj))
		 (r (cdr obj)))
	     (let ((a. (fn a))
		   (r. (rec (cdr obj))))
	       (if (and (eq? a a.)
			(eq? r r.))
		   obj
		   (cons a. r.)))))
	  ((null? obj)
	   tail)
	  (else
	   (fn obj)))))


;; cj Sat, 01 Jul 2006 19:51:48 +0200
;; moved some stuff from my unfinished cj-syntax module here, and added some more.

;; call them "expr"'s ?
;; or "syntax" like before?  or  ?


;; NOTE: most (if not all) of the functionality in this module is also
;; available as routines in Gambit core; when I wrote this code I
;; didn't know what was available where, so I wrote this from scratch,
;; but at some point I or someone else should look into replacing this
;; with the Gambit core routines and maybe merge what's left into
;; Gambit core.


(define (expr? obj) ;; returns false for expressions without 'attached notes'
  (and (vector? obj)
       (##fixnum.= (##vector-length obj) 4)
       (let ((v0 (##vector-ref obj 0)))
	 (and (vector? v0)
	      (##fixnum.= (##vector-length v0) 1)
	      (let ((v00 (##vector-ref v0 0)))
		(case v00
		  ((source1 source2) #t)
		  (else #f)))))))

(define (if-expr obj th)
  (if (expr? obj)
      (th)
      (error "not an expression with attached location information:" obj)))
(define-macro (check-expr obj . body)
  `(if-expr ,obj (lambda ()
		   ,@body)))
;(define (expr:value expr) (check-expr expr (@expr:value expr)))
; (define-macro (define-check-proxy name)
;   (define to-name (symbol-append '@ name))
;   `(define (,name val)
;      (check-expr ,val
; 		 (,to-name ,val))))
(define-macro (define-unsafe/safe parms . body)	;; only the first argument in parms is checked!
  (let ((name (car parms))
	(obj (cadr parms))
	(rest (cddr parms)))
    (define unsafe-name (string->symbol
                         (string-append
                          "@"
                          (symbol->string name))))
    ;;(pp-through (list "define-unsafe/safe\n")
    `(begin
       (define (,unsafe-name ,obj ,@rest)
	 ,@body)
       (define (,name ,obj ,@rest)
	 (check-expr ,obj
		     (,unsafe-name ,obj ,@rest))))))



(define (expr:strip-locationinfo expr)
  (let ((v (expr:value expr)))
    (cond ((pair? v)
           (dotted-map expr:strip-locationinfo v))
          ((vector? v)
           (vector-map expr:strip-locationinfo v))
          (else v))))


;" (define (syntax-type syn) ;; returns false, if it discovers   ach. wirr.
;   (let ((type-vec (vector-ref syn 0)))
;     (and (= (vector-length type-vec)
; 	    1)
; 	 (vector-ref type-vec 0))))
; "

(define (expr*:type expr) ;; returns false if it doesn't 'fit'
  (and (vector? expr)
       (>= (vector-length expr)
	   1)
       (let ((type-vec (vector-ref expr 0)))
	 (and (vector? type-vec)
	      (= (vector-length type-vec)
		 1)
	      (vector-ref type-vec 0)))))


(define-unsafe/safe (expr:value expr)
  (vector-ref expr 1))

(define-unsafe/safe (expr:file expr)
  (vector-ref expr 2))

(define-unsafe/safe (expr:pos expr) ;; combined line/col
  (vector-ref expr 3))

(define (make-error message)
  (lambda (erroneous-object)
    (error message erroneous-object)))


;;cj 30.10.06: todo  an error/exception function which emits an exception with text with location
;;ehr now only todo make it better and place it to correct place
(define (expr:error message expr)
  (error message
	 (expr:strip-locationinfo expr)
	 '|:|
	 'file
	 (expr*:file expr)
	 'line
	 (expr*:line expr)
	 'col
	 (expr*:col expr)))

;;/todo




(define (pos:line pos)
  (+ 1 (bitwise-and pos 65535)))


(define-unsafe/safe (expr:line expr
			       #!optional
			       (error (make-error "expr:line: no position info:"))
			       (success (lambda (x) x)))
  (cond ((@expr:pos expr)
	 => (lambda (pos)
	      (success (pos:line pos))))
	(else
	 (error expr))))


(define (pos:col pos)
  (+ 1 (quotient pos 65536)))


(define-unsafe/safe (expr:col syn
			      #!optional
			      (error (make-error "expr:col: no position info:"))
			      (success (lambda (x) x)))
  (cond ((@expr:pos syn)
	 => (lambda (pos)
	      (success (pos:col pos))))
	(else
	 (error syn))))

;; ((hm wird die benamsung hier nun,  mit : wäre schlecht, inkonsistent?  with-expr:line/col ??))
(define-unsafe/safe (with-expr-line/col expr
					success
					#!optional
					(error (make-error "with-expr-line/col: no position info:"))
					;; and take even one more error handler for type errors? instead of using the checker inserted from the macro
					)
  (cond ((@expr:pos expr)
	 => (lambda (pos)
	      (success (+ 1 (bitwise-and pos 65535))
		       (+ 1 (quotient pos 65536)))))
	(else
	 (error expr))))

(define (expr*:value obj)
  (if (expr? obj)
      (@expr:value obj)
      obj))

(define (_proxy_falseonerror op)
  (lambda (obj)
    (if (expr? obj)
	(op obj)
	#f)))
(define expr*:file (_proxy_falseonerror @expr:file))
(define expr*:pos (_proxy_falseonerror @expr:pos))

(define (_proxy_falseonerror_indirect op)
  (lambda (obj)
    (and (expr? obj)
	 (op obj (lambda (v)
		   #f)))))
(define expr*:line (_proxy_falseonerror_indirect @expr:line))
(define expr*:col (_proxy_falseonerror_indirect @expr:col))

(define (clone-expr expr)
  (vector (vector-ref expr 0)
          (vector-ref expr 1)
          (vector-ref expr 2)
          (vector-ref expr 3)))
;; clone- vorher, weil gleich wie make-. es ist keine methode? doch an sich schon. aber keine daten abruf.
;; (well, functional setters   es wäre ein functional setter ohne action ausser clone)(aber in functional setting gäbs kein Grund für clone)

;; scheme/the module system should elicit the argument types from the ops, like ocaml,
;; but I cannot use ##fixnum.+ etc. as long as it doesn't infer types and thus isn't safe.
;; So I write those:
(define (make-fixnum-op unsafe-op)
  (lambda (a b)
    (if (and (##fixnum? a)
	     (##fixnum? b))
	(unsafe-op a b)
	(error "expecting fixnums:" a b))))

(define fixnum:+ (make-fixnum-op ##fixnum.+))
(define fixnum:- (make-fixnum-op ##fixnum.-))
(define fixnum:* (make-fixnum-op ##fixnum.*))


(define (make-pos line col)
  (+ (fixnum:- line 1)
     (* (fixnum:- col 1) 65536)))


;; quasi "overloaded" on the number of arguments:
(define (make-expr val
		   #!optional
		   (file "(generated)")
		   (pos-or-line 0)
		   maybe-col)
  (vector '#(source1) ;; was ist das?
	  val
	  file
	  (if maybe-col
	      (make-pos pos-or-line
			maybe-col)
	      pos-or-line)))

;;(alt:  "sollte ich es  syntax-value-replace   oder syntax-replace-value nennen ?")
(define-unsafe/safe (expr:value-set expr val)
  (vector (vector-ref expr 0)
	  val
	  (vector-ref expr 2)
	  (vector-ref expr 3)))

(define (expr*:value-set expr val)
  (if (expr? expr)
      (@expr:value-set expr val)
      val))

;; ----------------------------------------------

(define (read-all-expr #!optional (port (current-input-port))) ;; NOTE: does NOT return an expr. It returns a *list* of expr's.
  (let recur ()
    (let ((expr (##read-expr-from-port port)))
      (if (eof-object? expr) '()
	  (cons expr (recur))))))

(define (file-read-all-expr filepath)
  (with-input-from-file filepath read-all-expr))

(define (file-read-as-expr filepath) ;; not 100% correct name, since it does NOT return an expr on the upmost level.
  (cons 'begin
	(file-read-all-expr filepath)))



;; some wisdom:
;; 	      (list 'quasiquote expr)
;; behält location info über das eval hinweg offenbar nicht; weil wohl [im eval optimiert wird oder nein eher] es dann versteckt ist?. stepper zeigt es nicht mehr richtig an.
;; also muss man auch da expr*:value-set verwenden.



(define (expr:fixup expr) ;; without recursion
  (if (expr? expr)
      expr
      (make-expr expr ; '(fixup)
		 )))

;; recursively, and peeks into existing expr's; functional, but
;; shares structure as much as possible
(define (expr:deep-fixup obj
                         #!optional
                         (file "(generated)")
                         (pos 0))
  (cond ((expr? obj)
	 (let ((v (@expr:value obj)))
	   ;; only turn over lists once, only the head needs wrapping.
	   ;; so don't call ourselves recursively here. Need a special
	   ;; map* function which also shares structure as much as
	   ;; possible.
	   (let ((v. (if (pair? v) ;; check this or we'll end up
                                   ;; wrapping them twice. Even with
                                   ;; the pair check below.
			 (let ((file (@expr:file obj))
                               (pos (@expr:pos obj)))
                           (map** (lambda (x)
                                    (expr:deep-fixup x file pos)) v))
			 v)))
	     (if (eq? v v.)
		 obj
		 (@expr:value-set obj v.)))))
	(else
	 (make-expr (if (pair? obj) ;; only recurse here if it's
                                    ;; actually a pair! endless loop
                                    ;; ensues otherwise of cours.
			(map** (lambda (x)
                                 (expr:deep-fixup x file pos))
                               obj)
			obj)
                    file
                    pos))))


;; cj {Thu Jun 14 18:05:06 2007}
;; for debugging/playing only:

(define-type element
  id: c6adce4d-d8c6-4713-8a47-45151f9be290
  val
  file
  line
  col)



(define (make-sexprelement val file line col)
  `(element val: ,val
	    file: ,file
	    line: ,line
	    col: ,col))
;;^-hm hat einfach auchwas für sich. zumindestjumpbar in emacs.  ah heh dotted end is aber strange dann hehe. Schad auch (auchhier!) dass pp keyword und value nicht nebeneinander sondern untereinander ausgibt, FEELEY sagen. (ok problem weil nicht garantiert dass keywords von nem value gefolgt sind? aber solangs nid ein keyword ist..? wel.)
(define (make-vectorelement.1 val file line col)
  `#(element val: ,val
	    file: ,file
	    line: ,line
	    col: ,col))
;; wowda hat man foifer und weggli. ausser dass still keyword+val nicht nebeneinander.
(define (make-vectorelement val file line col)
  (vector '<element> val file line col))
;;heh das wohl am besten in diesem Fall.
;;positional eben wieder.rein.  (leider keine serial numbers attached. aber ok, wenn functional, eh nicht so sehr gedacht)
;; TJA.... aber wo isch denn das nun anders als das originalformat hehehehe. (ah ok line und col separiert)


;; "expr" is still a bad name. "notatedexprelement" ?

;; expr can be a single annotated element, a list of annotated
;; elements, or anything at all. Only annotated elements are turned
;; into elements. This is recursively, tree based processing.including
;; dotted ends.

(define (try-exprelement->element expr)	
  (map* (lambda (obj)
	  (if (expr? obj)
	      (make-vectorelement (try-exprelement->element (@expr:value obj))
			    (@expr:file obj)
			    (@expr:line obj)
			    (@expr:col obj))
	      obj))
	expr))
;; (error "not an expr:" expr)

; (define t (file-read-all-expr "../mod/_cj-expr-testinput.scm"; "test-load.scm"
; 			      ))



(define (expr*:make-fun fun)
  (lambda (expr)
    (expr*:value-set
     expr
     (fun (expr*:value expr)))))

(define (expr*:make-fun* fun)
  (lambda (expr)
    (fun (expr*:value expr))))

(define expr*:car (expr*:make-fun car))
(define expr*:cdr (expr*:make-fun cdr))
(define expr*:cadr (expr*:make-fun cadr))
(define expr*:null? (expr*:make-fun* null?))
(define expr*:pair? (expr*:make-fun* pair?))


(define (expr*:map fn expr)
  (expr*:value-set
   expr
   (map (expr*:make-fun fn)
	(expr*:value expr))))

(define (expr*:dotted-map fn expr)
  (expr*:value-set
   expr
   (dotted-map (expr*:make-fun fn)
               (expr*:value expr))))

(define (expr*:strip-locationinfo expr)
  (let ((v (expr*:value expr)))
    (cond ((pair? v)
           (dotted-map expr*:strip-locationinfo v))
          ((vector? v)
           (vector-map expr*:strip-locationinfo v))
          (else v))))
