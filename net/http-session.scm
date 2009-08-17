;;; HTTP session support for the HTTP server.
;;;
;;; Copyright (c) 2008 Per Eckerdal

(import ../misc/uuid
        http-server)

(export make-session-variable
        session-id
        with-session-id
        session-generate-id
        session-get
        session-regenerate-id
        session-new
        session-create
        session-destroy
        session-open)

;==============================================================================

; Sessions.

;; TODO One possible optimization for sessions might be to lazily store the
;; current session in the current request object. This would save unneeded
;; lookups in the session registry via the cookie table.

(define session-registry (make-table))

(define (make-counter)
  (let ((num 0))
    (lambda ()
      (set! num (+ num 1))
      num)))

(define session-variable-counter (make-counter))
(define session-variable-nochange (gensym))
(define session-id-override (make-parameter #f))
(define session-cookie-name "_s")

(define (session-set-cookie! id)
  (cookie-set! session-cookie-name id path: "/"))

(define (make-session-variable-fun default-val)
  (let ((id (session-variable-counter)))
    (lambda (#!optional (set-to session-variable-nochange))
        (if (eq? set-to session-variable-nochange)
            (if (null? (current-request))
                (default-val)
                (let ((sess (session-open)))
                  (or (table-ref sess id #f)
                      (let ((v (default-val)))
                        (table-set! sess id v)
                        v))))
            (table-set! (session-open) id set-to)))))

(define-syntax make-session-variable
  (syntax-rules ()
    ((make-session-variable)
     (make-session-variable-fun
      (lambda () #f)))

    ((make-session-variable default-val)
     (let ((val default-val))
       (make-session-variable-fun
        (lambda () val))))))

;; Gets the current session id
(define (session-id)
  (or (session-id-override)
      (cookie-get session-cookie-name)))

(define (with-session-id id thunk)
  (parameterize
   ((session-id-override id))
   (session-set-cookie! id)
   (thunk)))

(define (session-generate-id)
  (make-uuid))

;; Opens the current session if it exists. Returns the session
;; if it did, otherwise #f
(define (session-get)
  (let ((curr-id (session-id)))
    (and curr-id
         (let ((sess (table-ref session-registry curr-id #f)))
           (or sess
               (begin
                 ;; Invalid session cookie
                 (cookie-del! session-cookie-name)
                 #f))))))

;; Regenerates the session id. This is here for security reasons;
;; a session id that changes on important occasions like login will
;; be more difficult to hijack
(define (session-regenerate-id)
  (let* ((old-id (session-id))
         (new-id (session-generate-id))
         (sess (table-ref session-registry old-id)))
    (table-set! session-registry old-id)
    (table-set! session-registry new-id sess)
    (session-set-cookie! new-id)))

;; Creates a new session and removes the old one if it exists.
(define (session-new)
  (session-destroy)
  (session-create))

;; Creates a session. Does nothing if a session exists. Returns
;; the session.
(define (session-create)
  (let ((sid (session-id)))
    (if (not sid)
        (let ((new-sid (session-generate-id))
              (sess (make-table)))
          (session-set-cookie! new-sid)
          (table-set! session-registry new-sid sess)
          sess)
        (session-get))))

;; Destroys a session. Does nothing if a session doesn't exist.
(define (session-destroy)
  (let ((sid (session-id)))
    (if sid
        (begin
          (table-set! session-registry sid)
          (cookie-del! session-cookie-name)))))

;; Opens an existing session or creates a new one
(define (session-open)
  (or (session-get) (session-create)))


