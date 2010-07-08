;; Taken from http://osdir.com/ml/lisp.scheme.scsh/1996-04/msg00010.html
;; Written by Olin Shivers
;; I removed the let-optionals macro (Per Eckerdal)

;;; This file defines three macros for parsing optional arguments to procs:
;;;     (LET-OPTIONALS  arg-list ((var1 default1) ...) . body)
;;;     (LET-OPTIONALS* arg-list ((var1 default1) ...) . body)
;;;     (:OPTIONAL rest-arg default-exp)
;;;
;;; The LET-OPTIONALS macro is defined using the Clinger/Rees
;;; explicit-renaming low-level macro system. You'll have to do some work to
;;; port it to another macro system.
;;;
;;; The LET-OPTIONALS* and :OPTIONAL macros are defined with simple
;;; high-level macros, and should be portable to any R4RS system.
;;;
;;; These macros are all careful to evaluate their default forms *only* if
;;; their values are needed.
;;;
;;; The top-level forms in this file are Scheme 48 module expressions.
;;; I use the module system to help me break up the expander code for 
;;; LET-OPTIONALS into three procedures, which makes it easier to understand
;;; and test. But if you wanted to port this code to a module-less Scheme
;;; system, you'd probably have to inline the three procs into the actual
;;; macro definition.
;;;
;;; The only interesting module that is exported by this file is
;;;     LET-OPT
;;; which obeys the following interface:
;;;     (exports (let-optionals  :syntax)
;;;              (let-optionals* :syntax)
;;;              (:optional       :syntax))
;;;
;;; To repeat: This code is not simple Scheme code; it is module code. 
;;; It must be loaded into the Scheme 48 ,config package, not the ,user 
;;; package. 
;;;
;;; The only non-R4RS dependencies in the macros are ERROR 
;;; and CALL-WITH-VALUES.
;;; 
;;; See below for details on each macro.
;;;     -Olin

;;; (LET-OPTIONALS arg-list ((var1 default1) ...) 
;;;   body
;;;   ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for binding a procedure's optional arguments to either
;;; the passed-in values or a default.
;;;
;;; The expression takes a rest list ARG-LIST and binds the VARi to
;;; the elements of the rest list. When there are no more elements, then
;;; the remaining VARi are bound to their corresponding DEFAULTi values.
;;; It is an error if there are more args than variables.
;;;
;;; - The default expressions are *not* evaluated unless needed.
;;;
;;; - When evaluated, the default expressions are carried out in the *outer*
;;;   environment. That is, the DEFAULTi forms do *not* see any of the VARi
;;;   bindings.
;;;
;;;   I originally wanted to have the DEFAULTi forms get eval'd in a LET*
;;;   style scope -- DEFAULT3 would see VAR1 and VAR2, etc. But this is
;;;   impossible to implement without side effects or redundant conditional
;;;   tests. If I drop this requirement, I can use the efficient expansion
;;;   shown below. If you need LET* scope, use the less-efficient 
;;;   LET-OPTIONALS* form defined below.
;;;
;;; Example:
;;; (define (read-string! str . maybe-args)
;;;   (let-optionals maybe-args ((port (current-input-port))
;;;                              (start 0)
;;;                              (end (string-length str)))
;;;     ...))
;;;
;;; expands to:
;;; 
;;; (let* ((body (lambda (port start end) ...))
;;;        (end-def (lambda (%port %start) (body %port %start <end-default>)))
;;;        (start-def (lambda (%port) (end-def %port <start-default>)))
;;;        (port-def  (lambda () (start-def <port-def>))))
;;;   (if (null? rest) (port-def)
;;;       (let ((%port (car rest))
;;;             (rest (cdr rest)))
;;;       (if (null? rest) (start-def %port)
;;;           (let ((%start (car rest))
;;;                 (rest (cdr rest)))
;;;             (if (null? rest) (end-def %port %start)
;;;                 (let ((%end (car rest))
;;;                       (rest (cdr rest)))
;;;                   (if (null? rest) (body %port %start %end)
;;;                       (error ...)))))))))


;;; (:optional rest-arg default-exp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for evaluating optional arguments and their defaults
;;; in simple procedures that take a *single* optional argument. It is
;;; a macro so that the default will not be computed unless it is needed.
;;; 
;;; REST-ARG is a rest list from a lambda -- e.g., R in
;;;     (lambda (a b . r) ...)
;;; - If REST-ARG has 0 elements, evaluate DEFAULT-EXP and return that.
;;; - If REST-ARG has 1 element, return that element.
;;; - If REST-ARG has >1 element, error.

(compile-options no-global-state: #t)
(export :optional
        let-optionals*)

(define-syntax :optional
  (syntax-rules ()
    ((:optional rest default-exp)
     (let ((maybe-arg rest))
       (cond ((null? maybe-arg) default-exp)
             ((null? (cdr maybe-arg)) (car maybe-arg))
             (else (error "too many optional arguments" maybe-arg)))))))


;;; (LET-OPTIONALS* args ((var1 default1) ... [rest]) body1 ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is just like LET-OPTIONALS, except that the DEFAULTi forms
;;; are evaluated in a LET*-style environment. That is, DEFAULT3 is evaluated
;;; within the scope of VAR1 and VAR2, and so forth.
;;;
;;; - If the last form in the ((var1 default1) ...) list is not a 
;;;   (VARi DEFAULTi) pair, but a simple variable REST, then it is
;;;   bound to any left-over values. For example, if we have VAR1 through
;;;   VAR7, and ARGS has 9 values, then REST will be bound to the list of
;;;   the two values of ARGS. If ARGS is too short, causing defaults to
;;;   be used, then REST is bound to '().
;;; - If there is no REST variable, then it is an error to have excess
;;;   values in the ARGS list.


;;; This just interfaces to REALLY-LET-OPTIONALS*, which expects
;;; the ARGS form to be a variable.

(define-syntax really-let-optionals*
  (syntax-rules ()
    ;; Standard case. Do the first var/default and recurse.
    ((really-let-optionals* args ((var1 default1 typecheck1 ...) etc ...)
       body1 ...)
     (call-with-values (lambda () (if (null? args)
                                      (values default1 '())
                                      (values (car args) (cdr args))))
                       (lambda (var1 rest)
                         (really-let-optionals* rest (etc ...)
                           body1 ...))))

    ;; Single rest arg -- bind to the remaining rest values.
    ((really-let-optionals* args (rest) body1 ...)
     (let ((rest args)) body1 ...))

    ;; No more vars. Make sure there are no unaccounted-for values, and
    ;; do the body.
    ((really-let-optionals* args () body1 ...)
     (if (null? args) (begin body1 ...)
         (error "Too many optional arguments." args)))))

(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* args vars&defaults body1 ...)
     (let ((rest args))
       (really-let-optionals* rest vars&defaults body1 ...)))))

