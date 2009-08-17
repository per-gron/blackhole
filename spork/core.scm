(import ../net/http-server
        ../net/http-session
        ../net/uri
        ../srfi/1
        ../srfi/13
        ../string/util
        ../string/sxml-to-xml
        ../misc/exception
        ../misc/al
        ../misc/splice
        counter)

(syntax-begin
 (import (only: ../srfi/1 filter)))

(export make-spork
        spork?
        spork-pattern
        spork-function
        spork-prefix
        spork-prefix!
        spork
        add-spork
        
        cutlery?
        make-cutlery
        cutlery-join
        cutlery-join!
        cutlery-add-spork!
        cutlery-add!
        cutlery-sporks
        cutlery-prefix
        cutlery-prefix!
        
        registry-put!
        register-function
        spork-die
        spork-reply-dont-die
        spork-reply
        spork-reply-xml
        
        goto
        goto-here
        
        fork
        ajax-fork
        show
        
        spork-server?
        spork-server-errors
        spork-server-errors-set!
        spork-server-public-kids
        spork-server-public-kids-set!
        spork-server-cutlery
        spork-server-cutlery-set!
        spork-server-root
        spork-server-root-set!
        make-spork-server
        spork-server-define-error
        
        show-error
        force-method
        
        spork-server-run
        spork-serve
        
        splice
        splice?
        unsplice
        
        make-backtrackable-variable
        make-session-variable
        make-variable)

;; Sporks

(define-type spork
  id: 3E6E0900-4308-4D58-8DF9-EE220A415355
  (pattern read-only:)
  (function read-only:))

(define (spork-prefix spork prefix)
  (make-spork (cons prefix (spork-pattern spork))
              (spork-function spork)))

(define (spork-prefix! spork prefix)
  (spork-pattern spork
                 (cons prefix
                       (spork-pattern spork))))

;; TODO This is only used by spork, so it should be in a let-syntax
;; really, but the module system doesn't support that right now.
(define-syntax lambda-ignorestring
  (sc-macro-transformer
   (lambda (form env)
     `(lambda ,(filter identifier? (cadr form))
        ,@(cddr form)))))

(define-syntax spork
  (syntax-rules ()
    ((spork (args ...) body ...)
     (make-spork
      '(args ...)
      (lambda-ignorestring
       (args ...)
       (show
        (lambda (url)
          body ...)))))))

(define-syntax add-spork
  (syntax-rules ()
    ((add-spork name (args ...) body ...)
     (cutlery-add-spork!
      name
      (spork (args ...) body ...)))))

;; Cutleries

(define-type cutlery
  id: D9017117-DA9D-4C17-AF35-63B02220B414
  constructor: make-cutlery-internal
  pages)

(define (make-cutlery . sporks)
  (make-cutlery-internal sporks))

(define (cutlery-join a b)
  (make-cutlery-internal
   (append
    (cutlery-pages a)
    (cutlery-pages b))))

(define (cutlery-join! a b)
  (cutlery-pages-set!
   a
   (append (cutlery-pages a)
           (cutlery-pages b)))
  a)

(define (cutlery-add-spork! c spork)
  (cutlery-pages-set!
   c
   (cons spork
         (cutlery-pages c)))
  (void))

(define (cutlery-add! c address fun)
  (cutlery-add-spork! c (make-spork address fun)))

(define (cutlery-sporks c)
  (cutlery-pages c))

(define (cutlery-prefix c prefix)
  (apply make-cutlery
         (map (lambda (spork)
                (spork-prefix spork prefix))
              (cutlery-sporks c))))

(define (cutlery-prefix! c prefix)
  (for-each (lambda (spork)
              (spork-prefix! spork prefix))
            (cutlery-sporks c)))

;; Pages

(define (address-match? s addrs)
  (let ((ret '()))
    (and (let loop ((addr addrs) (str s))
           (if (null? addr)
               (eq? 0 (string-length str))
               (let ((hd (car addr)))
                 (cond
                  ((symbol? hd)
                   (let ((pos (if (null? (cdr addr))
                                  (string-length str)
                                  (string-contains str (cadr addr)))))
                     (if pos
                         (begin
                           (set! ret (cons (substring str 0 pos) ret))
                           (loop (cdr addr)
                                 (substring str pos (string-length str))))
                         #f)))
                  ((string? hd)
                   (if (string-prefix? hd str)
                       (loop (cdr addr)
                             (string-remove-prefix str hd))
                       #f))))))
         (reverse ret))))

(define (split-path url)
  (let ((pos (string-contains url "/@")))
    (if pos
        (cons (string-remove-prefix (substring url 0 pos) "/")
              (substring url (+ pos 2) (string-length url)))
        (cons (string-remove-prefix url "/")
              #f))))

(define (call-page cutlery addr)
  (let loop ((page (cutlery-sporks cutlery)))
    (if (null? page)
        #f
        (if (let ((res (address-match? addr (spork-pattern (car page)))))
              (if res
                  (parameterize
                   ((current-frame (cons '() #f)))
                   (apply (spork-function (car page)) res)
                   (spork-reply
                    (lambda ()
                      (print "Ehh... return to call-page (this is an error)\n"))))
                  #f))
            #t
            (loop (cdr page))))))


(define current-path/server (make-parameter #f))

;; Core

(define kid-registry (make-session-variable (make-table)))
(define public-kid-registry (make-table))
(define current-frame (make-parameter #f))

(define generate-continuation-id
  (make-randomizer "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890-_.,:;$[]*/"))

(define (generate-unique-continuation-id #!optional registry)
  (let loop ((tries 0))
    (if (> tries 1000)
        (error "Failed to generate continuation id")
        (let ((id (generate-continuation-id)))
          (if (table-ref (or registry (kid-registry)) id #f)
              (loop (+ tries 1))
              id)))))

(define (registry-put! fun #!optional public kid)
  (let* ((registry (if public
                       (spork-server-public-kids public)
                       (kid-registry)))
         (kid (or kid (generate-unique-continuation-id
                       registry))))
    (table-set! registry
                kid
                (cons fun
                      (current-frame)))
    (let* ((cp (current-path/server))
            (server (if cp
                        (cdr cp)
                        default-server))
            (root (spork-server-root server)))
      (string-append
       root
       (if cp
           (let ((str (caar cp)))
             (if (eq? 0 (string-length str))
                 ""
                 (string-append "/" str)))
           "")
       (if public "/@@" "/@")
       kid))))

(define (registry-run kid param #!optional public)
  (let ((res (table-ref (if public
                            public-kid-registry
                            (kid-registry))
                        kid
                        #f)))
    (if res
        (parameterize
         ((current-frame (cons '()
                               (cdr res))))
         ((car res) param))
        #f)))

(define suicide (make-parameter #f))

(define (spork-die)
  ((suicide) #f))

(define (register-function fun #!optional public kid)
  (registry-put!
   (lambda (ret)
     (fun)
     (reply (lambda ()
              (print "ehh.. return from register-function")))
     (spork-die))
   public
   kid))

(define (spork-reply-dont-die . args)
  (run-before-show)
  (apply reply args))

(define (spork-reply . args)
  (apply spork-reply-dont-die args)
  (spork-die))

(define (spork-reply-xml . args)
  (run-before-show)
  (apply reply-xml args)
  (spork-die))

(define (goto url #!optional (run-before #t))
  (if run-before
      (run-before-show))
  (http-redirect 303 (render-widget-with-env url))
  (spork-die))

(define (goto-here #!optional (run-before #t))
  (call/cc
   (lambda (k)
     (goto (registry-put! k) run-before))))

;; Widgets

(define (fork a b)
  (letrec ((forked
            (make-backtrackable-variable
             (delay
               ;; Registry put has to be called from within
               ;; the lambda below and the a function must
               ;; only be invoked once, hence the delay.
               ;;
               ;; It's important that render-widget-cont is
               ;; captured here; It's not initialized when fork
               ;; is called first, and it will be lost at the time
               ;; the registry-put! callback is invoked.
               ;;
               ;; Ie: render-widget-cont is only valid in
               ;; the widget rendering phase
               (let* ((rwc (render-widget-cont)))
                 (a (lambda ()
                      (registry-put!
                       (lambda (ret)
                         ;; The modification of forked is sent as
                         ;; a thunk to rwc. This is because
                         ;; forked has to be set in the dynamic
                         ;; environment of the render and not of
                         ;; this request.
                         (rwc
                          (cons (current-frame)
                                (lambda ()
                                  (forked
                                   (delay
                                     ;; Who knows when this will be
                                     ;; called. It's best to use the
                                     ;; query of the current dynamic
                                     ;; environment and not ret above.
                                     (b (request-query
                                         (current-request)))))))))))))))))
    (lambda () (force (forked)))))


(define run-before-show-redirect
  (make-parameter #t))

(define (run-before-show)
  ;; This is to assist ajax-fork.
  (if (and (run-before-show-redirect)
           (assoc "__frk" (request-get-query (current-request))))
      (parameterize
       ((run-before-show-redirect #f))
       (show
        (lambda (url)
          `(e (@ (r ,url))))
        doctype: 'xml
        mime: "text/xml"))))

(define generate-ajax-fork-id
  (make-randomizer "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"))

(define (ajax-fork a b)
  (let* ((id (generate-ajax-fork-id))
         (f (fork
             (lambda (url)
               `(span
                 (@ (id ,id))
                 ,(a (lambda ()
                       (string-append
                        (url)
                        "?_frk="
                        id)))))
             b)))
    (lambda ()
      ;; The call to (f) has to be first, because it might do lots of
      ;; strange stuff with the continuation and it's important that
      ;; we don't cache values taken from (current-request) before
      ;; that. (Hence the let*)
      (let* ((v (render-widget (f)))
             (req-frk-id (al-get (request-get-query
                                  (current-request))
                                 "__frk")))
        (if (equal? req-frk-id id)
            (parameterize
             ((run-before-show-redirect #f))
             (show
              (lambda (url)
                `(e
                  (@ (xmlns "http://www.w3.org/1999/xhtml"))
                  ,v))
              doctype: 'xml
              mime: "text/xml"))
            v)))))

 
;; This is initialized when the widget tree is being rendered,
;; it points to the continuation just before the tree starts
;; to be rendered.
;;   It is used by fork.
(define render-widget-cont (make-parameter #f))

(define (render-widget xml)
  (cond
   ((pair? xml)
    (let ((head (render-widget (car xml))))
      (if (splice? head)
          (append (map render-widget
                       (unsplice head))
                (render-widget (cdr xml)))
        (cons head
              (render-widget (cdr xml))))))
   
   ((procedure? xml)
    (render-widget (xml)))
   
   (else
    xml)))

(define (render-widget-with-env xml)
  (let* ((kont #f)
         (r (call/cc (lambda (k)
                       (set! kont k)
                       #f)))
         (thunk (lambda ()
                  (parameterize
                   ((render-widget-cont kont))
                   (render-widget xml)))))
    (if r
        (parameterize
         ((current-frame (cons '() (car r))))
         ((cdr r)) ;; see fork
         (thunk))
        (thunk))))

(define (show fun
              #!key
              (doctype 'xhtml1)
              (code 200)
              (headers '())
              (mime "text/html"))
  (call/cc
   (lambda (k)
     ;; Only save the continuation in the registry if it's actually used.
     (let* ((kid-promise (delay (registry-put! k)))
            (kid (lambda () (force kid-promise))))
       (spork-reply
        (lambda ()
          (let ((val (fun kid)))
            (if (and (not (string? val)) doctype)
                (let ((val (render-widget-with-env val)))
                  (if (eq? doctype 'xml)
                      (sxml>>xml-fast val)
                      (sxml>>xhtml-fast val)))
                (print val))))
        code: code
        headers: headers
        type: mime)))))

;; Web server

(define-type spork-server
  id: 97796622-0CCE-4422-9521-6C1C054B3A2F
  constructor: make-spork-server-internal
  errors
  public-kids
  cutlery
  root)

(define default-server
  (make-spork-server-internal '() #f #f ""))

(define (make-spork-server cutlery #!key errors root)
  (make-spork-server-internal
   (or errors (spork-server-errors default-server))
   (make-table)
   cutlery
   (or root "")))

(define (spork-server-define-error server num lambda)
  (spork-server-errors-set!
   server
   (cons (cons num lambda)
         (spork-server-errors server))))

(define (show-error num . args)
  (let* ((path/server (current-path/server))
         (server (if path/server
                     (cdr path/server)
                     default-server))
         (v (assq num (spork-server-errors server))))
    (if v
        (spork-reply-xml (apply (cdr v) args)
                         code: num)
        (error "Error page not defined" num))))

;; Method is expected to be an uppercase string
(define (force-method method)
  (if (not (equal? (request-method (current-request))
                   method))
      (show-error 405)))

(spork-server-define-error
 default-server 400
 (lambda (#!optional msg)
   `(html
     (head (title "Bad Request"))
     (body
      (h1 "Bad Request")
      (p ,(or msg
              "Invalid request."))))))

(spork-server-define-error
 default-server 403
 (lambda ()
   `(html
     (head (title "Forbidden"))
     (body
      (h1 "Forbidden")
      (p "You don't have permission to access this page.")))))

(spork-server-define-error
 default-server 404
 (lambda ()
   `(html
     (head (title "File Not Found"))
     (body
      (h1 "File Not Found")
      (p "The URL you requested could not be found.")))))

(spork-server-define-error
 default-server 405
 (lambda ()
   `(html
     (head (title "Method Not Allowed"))
     (body
      (h1 "Method Not Allowed")
      (p "The request was made using an invalid method.")))))

(spork-server-define-error
 default-server 500
 (lambda (#!optional exc)
   `(html
     (head (title "Internal Server Error"))
     (body
      (h1 "Internal Server Error")
      ,@(if (not exc)
            `((p "The server encountered an internal error or "
                 "misconfiguration and was unable to complete "
                 "your request."))
            `((pre
               ,(exception/continuation->string exc))))))))

(define (handle-req server)
  (let ((cutlery (spork-server-cutlery server)))
    (call/cc
     (lambda (exit)
       (suicide exit)
       (let* ((req (current-request))
              (uri (request-uri req))
              (path (uri-path uri))
              (s-path (split-path path)))
         (parameterize
          ((current-path/server (cons s-path server)))
          (let ((id (cdr s-path))
                (url (car s-path)))
            (if id
                (if (let ((public (and (positive? (string-length id))
                                       (eq? #\@ (string-ref id 0)))))
                      (not (registry-run (if public
                                             (substring id
                                                        1
                                                        (string-length id))
                                             id)
                                         (request-query req)
                                         public)))
                    (goto (string-append "/" url)))
                (if (not (call-page cutlery url))
                    (show-error 404))))))))))

(define (handle-req-catch-errs server)
  (lambda ()
    (with-exception/continuation-catcher
     (lambda (e)
       (show-error 500 e))
     (lambda ()
       (handle-req server)))))

(define (spork-server-run server #!optional (port 8080))
  (http-server-start!
   (make-http-server (handle-req-catch-errs server)
                     port)))

(define (spork-serve #!key
                     (port 8080)
                     errors
                     root)
  (let ((c (make-cutlery)))
    (thread-start!
     (make-thread
      (lambda ()
        (spork-server-run (make-spork-server c
                                             errors: errors
                                             root: root)
                          port))))
    c))

;; Backtrackable variables

(define generate-bt-id #f)
(let ((bt 0))
  (set! generate-bt-id
        (lambda ()
          (set! bt (+ bt 1))
          bt)))

(define (bt-get id cf default)
  (let ((fv (assq id (car cf))))
    (if fv
        (cdr fv)
        (let ((parent-frame (cdr cf)))
          (if parent-frame
              (bt-get id parent-frame default)
              default)))))

(define (bt-set! id val)
  (let ((cf (current-frame)))
    (set-car! cf
              (cons (cons id val)
                    (car cf)))
    val))

(define backtrackable-variable-nochange (gensym))

(define (make-backtrackable-variable #!optional default-val)
  (let ((id (generate-bt-id)))
    (lambda (#!optional (val backtrackable-variable-nochange))
      (if (eq? val backtrackable-variable-nochange)
          (bt-get id (current-frame) default-val)
          (bt-set! id val)))))

(define (make-variable #!optional default-val)
  (let ((val default-val))
    (lambda (#!optional (new-val backtrackable-variable-nochange))
      (if (eq? val backtrackable-variable-nochange)
          val
          (begin
            (set! val new-val)
            new-val)))))

