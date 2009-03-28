(import core
        counter
        js
        ../net/x-www-form-urlencoded
        ../net/http-server
        ../srfi/1
        ../srfi/13
        ../misc/al
        ../string/util)

;; Utilities

(define (url-add-parameter url key val)
  (let ((key (urlencode key))
        (val (urlencode val)))
    (if (string-index url #\?)
        (string-append url "&" key "=" val)
        (string-append url "?" key "=" val))))

(define-macro (define-tag spec . body)
  (let ((attrs '(id class style)))
    `(define (,@spec
              ,@(if (and (list? spec)
                         (memq '#!key spec))
                    '()
                    '(#!key))
              ,@attrs)
       (define-macro (@ . args)
         (cons 'quasiquote
               (list
                (append (cons '@ args)
                        ',(map (lambda (a)
                                 (list 'unquote-splicing
                                       `(if ,a `((,',a ,,a)) '())))
                               attrs)))))
       ,@body)))

;; Misc. widgets

(define-tag (link text #!optional (href "javascript:void(0);"))
  `(a ,(@ (href ,href))
      ,text))

(define (w/link text fun #!key ajax)
  ((if ajax ajax-fork fork)
   (lambda (url) (link text url))
   (lambda (ret) (fun))))

(define-tag (w/link* text fun)
  (let ((url (register-function fun)))
    `(a ,(@ (href ,url)) ,text)))

(define (head title . rest)
  `(head (title ,title)
         ,@rest))

(define (basic-template . content)
  `(html
    ,(head "Spork")
    (body
     ,@content)))

(define (js-include file)
  `(script (@ (src ,file) (type "text/javascript")) " "))

(define (tabbed-pane args
                     #!key
                     (links
                      (lambda (choices url-fun)
                        `(ul (@ (class "tabbed_pane_choices"))
                             ,@(map (lambda (x)
                                      `(li ,(link (car x)
                                                  (url-fun x))))
                                    choices))))
                     (content
                      (lambda (content)
                        `(div (@ (class "tabbed_pane_contents"))
                              ,@content)))
                     (wrap
                      (lambda (links content)
                        `(div (@ (class "tabbed_pane"))
                              ,links
                              ,content))))
  (let loop ((ret #f))
    (fork-choose
     (lambda (choice)
       (wrap
        (links args (lambda (tab)
                      (choice (lambda (_)
                                (loop tab)))))
        (content
         (if ret
             (cdr ret)
             (cdar args))))))))

;; Navigation

(define-tag (nav-item text url)
  (and url `(li ,(@) ,(link text url))))

(define-tag (navigation lst)
  `(ul ,(@)
    ,@(filter
       (lambda (x) x)
       (map (lambda (p) (apply nav-item p))
            lst))))

(define-syntax nav
  (syntax-rules ()
    ((nav "INNER")
     '())
    
    ((nav "INNER" (elm ...) rest ...)
     (cons (list elm ...)
           (nav "INNER" rest ...)))

    ((nav elm ...)
     (navigation
      (nav "INNER" elm ...)))))

;; Javascript

(define core-js-module
  (js-module (for-each
              map
              obj->query-string
              jsfork
              event-observe
              event-stop)
      ()
    
    (define (for-each fun arr)
      (if (or (vector? arr)
              (instanceof arr NodeList)
              (instanceof arr HTMLCollection))
          (do ((i 0 (+ i 1)))
              ((= i arr.length))
            (fun (ref arr i) i))
          (for-in key arr
                  (fun (cons key (ref arr key))))))

    (define (map fun arr)
      (let ((res (vector)))
        (for-each (lambda (elm idx)
                    (res.push (fun elm idx)))
                  arr)
        res))

    (let* ((old-xhr window.XMLHttpRequest)
           ;; Browser type
           (gecko? (not (not window.controllers)))
           (ie? (and window.document.all
                     (not window.opera)))
           ;; Constructor
           (new-xhr (lambda ()
                      (set! ._object (if old-xhr
                                         (new old-xhr)
                                         (new window.ActiveXObject
                                              "Microsoft.XMLHTTP")))
                      (set! ._listeners (vector))
                      this)))
      
      (define (clean-transport req)
        ;; Bugfix: IE - memory leak (on-page leak)
        (set! req._object.onreadystatechange (new window.Function))
        (delete req._headers))
      
      (define (get-document req)
        (let ((doc req.responseXML))
          ;; Try parsing responseText
          (if (and ie?
                   doc
                   (not doc.documentElement)
                   ((: (req.getResponseHeader "Content-Type") match)
                    (regexp "[^/]+/[^\\+]+\\+xml")))
              (begin
                (set! doc
                      (new window.ActiveXObject "Microsoft.XMLDOM"))
                (doc.loadXML req.responseText)))
          ;; Check if there is no error in document
          (if (and doc
                   (or (and ie? (not (eq? doc.parseError 0)))
                       (eq? doc.documentElement.tagName "parsererror")))
              '()
              doc)))
      
      (define (synchronize-values req)
        (trylet (set! req.responseText req._object.responseText))
        (trylet (set! req.responseXML (get-document req._object)))
        (trylet (set! req.status req._object.status))
        (trylet (set! req.statusText req._object.statusText)))
      
      (define (ready-state-change-helper req)
        ;; Sniffing code
        (if new-xhr.onreadystatechange
            (new-xhr.onreadystatechange.apply req))
        
        ;; Fake event
        (req.dispatchEvent
         (obj type "readystatechange"
              bubbles #f
              cancelable #f
              timeStamp (+ (new Date) 0))))
      
      ;; Bugfix: FF w/ Firebug installed would break pages if not executed
      (if (and gecko? old-xhr.wrapped)
          (set! new-xhr.wrapped old-xhr.wrapped))
      
      ;; Constants and class-level event handlers
      (for-each
       (lambda (pair)
         (set! (ref new-xhr (car pair))
               (cdr pair)))
       (obj
        UNSENT 0
        OPENED 1
        HEADERS_RECEIVED 2
        LOADING 3
        DONE 4
        onreadystatechange '()
        onopen '()
        onsend '()
        onabort '()
        
        toString (lambda () "[XMLHttpRequest]")))
      
      ;; Public properties, instance-level event handlers
      ;; and public methods
      (let ((proto new-xhr.prototype))
        (for-each
         (lambda (pair)
           (set! (ref proto (car pair))
                 (cdr pair)))
         (obj
          readyState new-xhr.UNSENT
          responseText ""
          responseXML '()
          status 0
          statusText ""
          onreadystatechange '()
          open (lambda (method url async user password)
                 (if (undefined? async)
                     (set! async #t))
                 (define request this)
                 (define state .readyState)
                 
                 ;; Bugfix: IE memory leak on page unload (inter-page leak)
                 (define on-unload #f)
                 (if ie?
                     (begin
                       (set! on-unload
                             (lambda ()
                               (if (not
                                    (eq? request._object.readyState
                                         new-xhr.DONE))
                                   (clean-transport request)
                                   (request.abort))))
                       (if async
                           (window.attachEvent "onunload" on-unload))))
                 
                 (set!
                  ._object.onreadystatechange
                  (lambda ()
                    (cond
                     ((and gecko? (not async))
                      #f)
                     
                     ;; Bugfix: Firefox fires unneccesary DONE when aborting
                     (request._aborted
                      (set! request.readyState new-xhr.UNSENT)
                      #f)
                     
                     (else
                      ;; Synchronize state
                      (set! request.readyState
                            request._object.readyState)
                      
                      (synchronize-values request)
                      
                      (if (eq? request.readyState new-xhr.DONE)
                          (begin
                            (clean-transport request)
                            
                            ;; Bugfix: IE - memory leak in interrupted
                            (if (and ie? async)
                                (window.detachEvent "onunload"
                                                    on-unload))))
                      
                      ;; Bugfix: IE and Gecko fire OPEN readystate twice
                      (if (not (and (eq? state request.readyState)
                                    (eq? state 1)))
                          (ready-state-change-helper request))
                      
                      (set! state request.readyState)))))
                 
                 ;; Add method sniffer
                 (if new-xhr.onopen
                     (new-xhr.opopen.apply this arguments))
                 
                 (._object.open.apply ._object arguments)
                 
                 ;; Bugfix: Gecko - missing readystatechange calls in
                 ;; synchronous requests
                 (if (and gecko?
                          (not async))
                     (begin
                       (set! .readyState new-xhr.OPENED)
                       (ready-state-change-helper this))))
          
          send (lambda (data)
                 ;; Add method sniffer
                 (if new-xhr.onsend
                     (new-xhr.onsend.apply this arguments))
                 
                 ;; Bugfix: Safari - fails sending documents
                 ;; created/modified dynamically, so an explicit
                 ;; serialization required Bugfix: IE - rewrites any
                 ;; custom mime-type to "text/xml" in case an XMLNode
                 ;; is sent Bugfix: Gecko - fails sending Element
                 ;; (this is up to the implementation either to
                 ;; standard)
                 (if (and data data.nodeType)
                     (begin
                       (set! data
                             (if window.XMLSerializer
                                 ((: (new window.XMLSerializer) serializeToString)
                                  data)
                                 data.xml))
                       (if (not (ref ._headers "Content-Type"))
                           (._object.setRequestHeader "Content-Type"
                                                      "application/xml"))))
                 
                 (._object.send data)
                 
                 ;; Bugfix: Gecko - missing readystatechange calls in
                 ;; synchronous requests
                 (if (and gecko? (not ._async))
                     (begin
                       (set! .readyState new-xhr.OPENED)
                       
                       ;; Synchronize state
                       (synchronize-values this)
                       
                       ;; Simulate missing states
                       (while (and (not ._aborted)
                                   (< .readyState new-xhr.DONE))
                              (incr! .readyState)
                              (ready-state-change-helper this)))))
          
          abort (lambda ()
                  ;; Add method sniffer
                  (if new-xhr.onabort
                      (new-xhr.onabort.apply this arguments))
                  
                  ;; Bugfix: Gecko - unneccesary DONE when aborting
                  (if (> .readyState new-xhr.UNSENT)
                      (set! ._aborted #t))
                  
                  (._object.abort)
                  
                  ;; Bugfix: IE - memory leak
                  (clean-transport this))
          
          getAllResponseHeaders (lambda ()
                                  (._object.getAllResponseHeaders))
          
          getResponseHeader (lambda (name)
                              (._object.getResponseHeader name))
          
          setRequestHeader (lambda (name value)
                             ;; Bugfix: IE - cache issue
                             (if (not ._headers)
                                 (set! ._headers (obj)))
                             (set! (ref ._headers name) value)
                             
                             (._object.setRequestHeader name value))
          
          ;; EventTarget interface implementation
          
          addEventListener (lambda (name handler use-capture)
                             (let ((listener #f)
                                   (idx 0))
                               (while (and
                                       (set! listener
                                             (ref ._listeners idx))
                                       (not
                                        (and (eq? (ref listener 0)
                                                  name)
                                             (eq? (ref listener 1)
                                                  handler)
                                             (eq? (ref listener 2)
                                                  use-capture))))
                                      (incr! idx))
                               ;; Remove listener
                               (if listener
                                   (._listeners.push
                                    (vector name handler use-capture)))))
          
          removeEventListener (lambda (name handler use-capture)
                                (let ((listener #f)
                                      (idx 0))
                                  (while (and
                                          (set! listener
                                                (ref ._listeners idx))
                                          (not
                                           (and (eq? (ref listener 0)
                                                     name)
                                                (eq? (ref listener 1)
                                                     handler)
                                                (eq? (ref listener 2)
                                                     use-capture))))
                                         (incr! idx))
                                  
                                  (if listener
                                      (._listeners.splice idx 1))))
          
          dispatchEvent (lambda (evt)
                          (let ((evt
                                 (obj type evt.type
                                      target this
                                      currentTarget this
                                      eventPhase 2
                                      bubbles evt.bubbles
                                      cancelable evt.cancelable
                                      timeStamp evt.timeStamp
                                      ;; There is no flow
                                      stopPropagation (lambda () #f)
                                      ;; There is no default action
                                      preventDefault (lambda () #f)
                                      ;; Original event should be inited
                                      initEvent (lambda () #f))))
                            
                            ;; Execute onreadystatechange
                            (if (and (eq? evt.type "readystatechange")
                                     .onreadystatechange)
                                ((: (or .onreadystatechange.handleEvent
                                        .onreadystatechange)
                                    apply)
                                 this
                                 (vector evt)))
                            
                            ;; Execute listeners
                            (for-each (lambda (l)
                                        (if (and (eq? (ref l 0) evt.type)
                                                 (not (ref l 2)))
                                            ((: (or (: (ref l 1) handleEvent)
                                                    (ref l 1))
                                                apply)
                                             this
                                             (vector evt))))
                                      ._listeners)))
          
          toString (lambda () "[object XMLHttpRequest]"))))
      
      (set! window.XMLHttpRequest new-xhr))
    
    (define (obj->query-string obj)
      (join
       (map (lambda (x)
                (+ (escape (car x))
                   "="
                   (escape (cdr x))))
              obj)
         "&"))
    
    (define (jsfork id url vars)
      (let ((xhr (new XMLHttpRequest)))
        (xhr.open "POST" (string-append url "?__frk=" id) #t)
        (set! xhr.onreadystatechange
              (lambda ()
                (if (eq? .readyState XMLHttpRequest.DONE)
                    (let* ((documentElement .responseXML.documentElement)
                           (r (documentElement.getAttribute "r"))
                           (elm (document.getElementById id)))
                      (if r
                          (set! document.location.href r)
                          (let ((n (init-node
                                    (document.importNode
                                     (ref documentElement.childNodes 0)
                                     #t))))
                            (elm.parentNode.replaceChild n elm)))))))
        (xhr.setRequestHeader "Content-Type" "application/x-www-form-urlencoded")
        (xhr.send (obj->query-string vars))))
    
    (define (event-observe element name handler)
      (element.addEventListener name handler #f)
      element)
    
    (define (event-stop evt)
      (if evt.preventDefault
          (evt.preventDefault))
      (if evt.stopPropagation
          (evt.stopPropagation))
      (set! evt.stopped #t))
    
    (define (init-node n)
      (define (parse-fork-url url)
         (url.match
          (regexp "(.*?)\\?_frk=(.*)")))

      (if n.getElementsByTagName
          (begin
            (for-each (lambda (x)
                        (event-observe
                         x
                         "submit"
                         (lambda (evt)
                           (let ((m (parse-fork-url x.action)))
                             (if m
                                 (begin
                                   (event-stop evt)
                                   (jsfork (ref m 2)
                                           (ref m 1)
                                           (let ((res (obj)))
                                             (for-each
                                              (lambda (x)
                                                (if x.name
                                                    (set! (ref res x.name)
                                                          x.value)))
                                              (.getElementsByTagName "input"))
                                             res))))))))
                      (n.getElementsByTagName "form"))
            
            (for-each (lambda (x)
                        (let ((m (parse-fork-url x.href)))
                          (if m
                              (set! x.href
                                    (string-append
                                     "javascript:"
                                     (js-mangle-name jsfork)
                                     "('"
                                     (escape (ref m 2))
                                     "','"
                                      (ref m 1)
                                     "')")))))
                      (n.getElementsByTagName "a"))))
      
      n)
    
    (event-observe
     window
     "load"
     (lambda (evt)
       (init-node document)))))

(define (js-spork address . modules)
  (make-spork (list address)
              (lambda ()
                (show
                 (lambda (_)
                   (apply string-append
                          (map js-module-code modules)))
                 doctype: #f
                 mime: "text/javascript"))))

;; Fork tools

(define (forkever fun #!key ajax)
  (let loop ((ret #f))
    ((if ajax ajax-fork fork)
     (lambda (url)
       (fun url ret))
     (lambda (ret)
       (loop ret)))))

(define (fork-choose fun #!key ajax)
  (let ((callback-list '()))
    ((if ajax ajax-fork fork)
     (lambda (url-generator)
       (let ((url (url-generator)))
         (fun
          (lambda (callback)
            (let ((pair (assq callback callback-list)))
              (url-add-parameter
               url
               "__frc"
               (number->string
                (if pair
                    (cdr pair)
                    (let ((new-id (if (null? callback-list)
                                      0
                                      (+ 1 (cdar callback-list)))))
                      (set! callback-list
                            (cons (cons callback new-id)
                                  callback-list))
                      new-id)))))))))
     (lambda (ret)
       (let* ((num-str (al-get ret "__frc"))
              (num (and num-str
                        (with-exception-catcher
                         (lambda (e) #f)
                         (lambda () (string->number num-str)))))
              (proc
               (let loop ((lst callback-list))
                 (cond
                  ((null? lst) #f)
                  ((= num (cdar lst)) (caar lst))
                  (else (loop (cdr lst)))))))
         (if proc
             (proc ret)
             (show-error 400)))))))

;; Wizard

(define (wizard . args)
  (define (fun funs sofar)
    (lambda (ret)
      (if (null? funs)
          #f
          (let ((sofar (cons ret sofar)))
            (apply
             (car funs)
             (cons (fun (cdr funs) sofar)
                   (cdr (reverse sofar))))))))
  ((fun args '()) #f))

;;; wizard does this when three arguments are passed to it:
;;  ((car args)
;;   (lambda (ret)
;;     ((cadr args)
;;      (lambda (ret2)
;;        ((caddr args)
;;         #f
;;         ret
;;         ret2))
;;      ret)))

;; Forms

(define (check-errors checks success)
  (lambda (ret err)
    (let ((errs '()))
      (for-each
       (lambda (ch)
         (let* ((val (al-get ret (car ch)))
                (err (and val ((cdr ch)
                               val
                               ret
                               (car ch)))))
           (if err
               (set!
                errs
                (cons (cons (car ch)
                            (if (eq? err #t)
                                "Invalid"
                                err))
                      errs)))))
       checks)
      (if (not (null? errs))
          (err errs)))
    (success ret)))

(define (form-verify-length len #!optional msg)
  (lambda (val others name)
    (and (< (string-length val) len)
         (or msg
             (string-append
              " "
              (string-humanize name)
              " has to be at least "
              (number->string len)
              " characters long")))))

(define (form-verify-presence #!optional msg)
  (lambda (val others name)
    (and (= 0 (string-length val))
         (or msg
             (string-append
              (string-humanize name)
              " can't be empty")))))

(define (form-verify-equal with msg)
  (lambda (val others name)
    (and (not (equal? val (al-get others with)))
         msg)))

(define (form-label id name)
  `(label (@ (for ,id)
             (id ,(string-append id "_label")))
          ,name))

(define (make-form-field-type type
                              #!key
                              (value-fun
                               (lambda (val)
                                 `((value ,val)))))
  (lambda (id
           name
           #!key
           default
           label-class)
    (values
     `(input (@ (type ,type)
                (name ,id)
                (id ,id)
                ,@(let ((val (form-get-field id)))
                    (if (or val default)
                        (value-fun (or val default))
                        '()))))
     `(label (@ (for ,id)
                ,@(if label-class 
                      `((class ,label-class))
                      `()))
             ,name)
     (let ((err (form-get-error id)))
       (and err `(strong ,err))))))

(define text-field (make-form-field-type "text"))

(define password-field (make-form-field-type "password"))

(define-tag (hidden-field id value)
  `(input ,(@ (type "hidden") (value ,value))))

(define checkbox
 (make-form-field-type
  "checkbox"
  value-fun: (lambda (val)
               (if val
                   `((checked "checked"))
                   '()))))

(define (submit-button #!optional (text "Submit"))
  (values
   `(input (@ (type "submit") (value ,text)))
   #f
   #f))

(define (select-field id name vals)
  (values
   `(select
     (@ (name ,id)
        (id ,id))
     ,@(map (lambda (x)
              (let* ((p (pair? x))
                     (id (if p (car x) x))
                     (name (if p (cdr x) x)))
                `(option (@ (value ,id))
                         ,name)))
            vals))
   (form-label id name)))

(define (text-area id name #!optional (value ""))
  (values
   `(textarea (@ (id ,id) (name ,id)) ,value)
   (form-label id name)))

(define (fieldset name . fields)
  `(fieldset
    (legend ,name)
    ,@(let ((err (form-get-error)))
        (if err
            `((strong ,err))
            '()))
    (ol
     ,@fields)))

(define form-errors (make-parameter #f))
(define form-fields (make-parameter #f))

(define (form-get-error #!optional id)
  (let ((res (and (form-errors) (assoc id (form-errors)))))
    (and res (cdr res))))

(define (form-get-field id)
  (let ((res (and (form-fields) (assoc id (form-fields)))))
    (and res (cdr res))))

(define-tag (form fun content #!key (default '()) (ajax #f))
  (if (string? fun)
      (parameterize
       ((form-errors #f)
        (form-fields '()))
       `(form ,(@ (action ,fun) (method "post"))
              ,@(content)))
      (let loop ((ret #f) (err #f))
        ((if ajax ajax-fork fork)
         (lambda (url)
           (parameterize
            ((form-fields (or ret default))
             (form-errors (if (string? err)
                              `((#f . ,err))
                              err)))
            `(form ,(@ (action ,url) (method "post"))
                   ,@(content))))
         (lambda (ret)
           (let* ((err #f))
             (or
              (and
               ret
               (call/cc
                (lambda (fret)
                  (set! err
                        (call/cc
                         (lambda (k)
                           (fret
                            (parameterize
                             ((form-fields (or ret default)))
                             (let ((val (fun ret k)))
                               (if ajax (goto-here))
                               val))))))
                  #f)))
              (begin
                (goto-here)
                (loop ret err)))))))))

(define (form-controls . args)
  `(,@(if (form-get-error)
          `((p ,(form-get-error)))
          '())
    (ol ,@(map (lambda (arg)
                 (call-with-values (lambda () arg)
                   ;; For some reason I don't get #!optional to work here
                   ;; I won't bother doing that now.
                   (lambda (content . rest) ;; rest is label, error
                     (let ((label (and (not (null? rest)) (car rest)))
                           (error (and (not (null? rest))
                                       (not (null? (cdr rest)))
                                       (cadr rest))))
                       `(li
                         ,@(if label `(,label) '())
                         ,@(if error `(,error) '())
                         ,content)))))
               args))))
