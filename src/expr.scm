;; Copyright 2006-2009 by Christian Jaeger, <christian at pflanze mine nu>

;; Published under the terms of either the GNU LGPL Version 2.1 or the
;; Apache License Version 2.0; you can choose whichever license you
;; prefer.

;; a kindof-dotted map that shares structure as much as possible
(define (map** fn obj)
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
	   '())
	  (else
	   (fn obj)))))


;; NOTE: most (if not all) of the functionality in this module is also
;; available as routines in Gambit core; when I wrote this code I
;; didn't know what was available where, so I wrote this from scratch,
;; but at some point I or someone else should look into replacing this
;; with the Gambit core routines and maybe merge what's left into
;; Gambit core.


;; Returns false for expressions without 'attached notes'
(define (expr? obj)
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

;; Only the first argument in parms is checked!
(define-macro (define-unsafe/safe parms . body)
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


;; TODO I think this does the same thing as ##source-code (barring safeness?)
(define-unsafe/safe (expr:value expr)
  (vector-ref expr 1))

(define-unsafe/safe (expr:file expr)
  (vector-ref expr 2))

(define-unsafe/safe (expr:pos expr) ;; combined line/col
  (vector-ref expr 3))

(define (make-error message)
  (lambda (erroneous-object)
    (error message erroneous-object)))


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

;; TODO This function does the same thing as ##source-strip (barring safeness?)
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

;; clone- vorher, weil gleich wie make-. es ist keine methode? doch an
;; sich schon. aber keine daten abruf.  (well, functional setters es
;; wäre ein functional setter ohne action ausser clone)(aber in
;; functional setting gäbs kein Grund für clone)

;; scheme/the module system should elicit the argument types from the
;; ops, like ocaml, but I cannot use ##fixnum.+ etc. as long as it
;; doesn't infer types and thus isn't safe.  So I write those:
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

(define (make-expr val file pos)
  (vector '#(source1) val file pos))

(define-unsafe/safe (expr:value-set expr val)
  (vector (vector-ref expr 0)
	  val
	  (vector-ref expr 2)
	  (vector-ref expr 3)))

;; I think ##expand-source-template (_nonstd.scm) does something
;; similar to this
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


;; TODO This function does the same thing as ##sourcify-deep. (Does
;; ##sourcify-deep share structure?) (Does ##sourcify-deep look inside
;; source objects?) (barring safeness?)
;;
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
                                    (expr:deep-fixup x file pos))
                                  v))
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

(define (expr*:make-fun fun)
  (lambda (expr)
    (expr*:value-set
     expr
     (fun (expr*:value expr)))))

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

;; TODO This function does a similar thing to ##desourcify. The
;; difference is that this functions continues to search within
;; s-exprs that are not sourcified.
(define (expr*:strip-locationinfo expr)
  (let ((v (expr*:value expr)))
    (cond ((pair? v)
           (dotted-map expr*:strip-locationinfo v))
          ((vector? v)
           (vector-map expr*:strip-locationinfo v))
          (else v))))

(define-macro (with-expr* name . body)
  `(expr*:value-set
    ,name
    (let ((,name (expr*:value ,name)))
      ,@body)))

(define-macro (let-expr* spec . body)
  (let ((spec (if (vector? spec)
                  (vector-ref spec 1)
                  spec)))
    `(let ((,(car spec) ,(cadr spec)))
       (with-expr* ,(car spec) ,@body))))
