;;; Utilities

;; TODO This one is already defined in util.scm
(define-macro (push! list obj)
  `(set! ,list (cons ,obj ,list)))

(define (string-for-each fn str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond
       ((= i len) #!void)
       (else
        (fn (string-ref str i))
        (loop (+ i 1)))))))

(define (reverse-list->string list)
  (let ((str (make-string (length list))))
    (let loop ((i 0)
               (list list))
      (cond
       ((pair? list)
        (string-set! str i (car list))
        (loop (+ 1 i) (cdr list)))))
    str))

(define (string-split chr str #!optional (sparse #f))
  (let* ((curr-str '())
         (result '())
         (new-str (lambda ()
                    (push! result (reverse-list->string curr-str))
                    (set! curr-str '())))
         (add-char (lambda (chr)
                     (push! curr-str chr))))
    (string-for-each (lambda (c)
                       (cond
                        ((eq? c chr)
                         (if (or (not sparse) (not (null? curr-str))) (new-str)))
                        (else
                         (add-char c))))
                     str)
    (new-str)
    (reverse result)))

;; TODO This isn't used
(define (read-all-string #!optional (port (current-input-port)))
  (let* ((buf-len 100)
         (buf (make-u8vector buf-len)))
    (with-output-to-string
      ""
      (lambda ()
        (let loop ()
          (let ((chr (read-char port)))
            (cond
             ((not (eq? chr #!eof))
              (write-char chr)
              (loop)))))))))

(define (read-url url)
  (with-input-from-process
   (list path: "curl"
         arguments: '("-s" "www.google.com"))
   (lambda ()
     (read))))

;;; Version numbers

(define-type version
  id: 31B8EF4A-9244-450F-8FA3-A5E914448B3A

  (major read-only:)
  (minor read-only:)
  (build read-only:))

(define (symbol->version sym)
  (if (not (symbol? sym))
      (error "Expected symbol" sym))
  (let* ((str (symbol->string sym))
         (str-len (string-length str))
         (str-no-v
          (if (> str-len 1)
              (substring str 1 str-len)
              (error "Invalid format" sym)))
         (split-string (string-split #\. str-no-v))
         (split-string-len (length split-string)))
    (if (not (<= 0 split-string-len 3))
        (error "Invalid format" sym))
    (let ((s->i
           (lambda (str)
             (let ((res (string->number str)))
               (if (or (not (integer? res))
                       (< res 0))
                   (error "Invalid format" res sym))
               res))))
      (make-version (and (>= split-string-len 1)
                         (s->i (car split-string)))
                    (and (>= split-string-len 2)
                         (s->i (cadr split-string)))
                    (and (= split-string-len 3)
                         (s->i (caddr split-string)))))))

(define (version->symbol v)
  (string->symbol
   (apply
    string-append
    `("v"
      ,@(if (version-major v)
            `(,(number->string
                (version-major v))
              ,@(if (version-minor v)
                    `("."
                      ,(number->string
                        (version-minor v))
                      ,@(if (version-build v)
                            `("."
                              ,(number->string
                                (version-build v)))
                            '()))
                    '()))
            '())))))

(define (version-comparison pred?)
  (lambda (v ref)
    (or (not (version-major v))
        (and (pred? (version-major v)
                    (version-major ref))
             (or (not (version-minor v))
                 (and (pred? (version-minor v)
                             (version-minor ref))
                      (or (not (version-build v))
                          (pred? (version-build v)
                                 (version-build ref)))))))))
  
(define version~=? (version-comparison =))
(define version~<? (version-comparison <))
(define version~<=? (version-comparison <=))
(define version~>? (version-comparison >))
(define version~>=? (version-comparison >=))
  


;;; Package metadata

(define-type package-metadata
  id: FBD3E6A5-3587-4152-BF57-B7D5E448DAB8

  (version read-only:)
  (maintainer read-only:)
  (author read-only:)
  (homepage read-only:)
  (description read-only:)
  (keywords read-only:)
  (license read-only:)

  (exported-modules read-only:)
  (default-module read-only:)
  (module-directory read-only:))

(define (parse-package-data form)
  (define (find-one? pred? lst)
    (let loop ((lst lst))
      (cond
       ((null? lst)
        #f)

       ((pair? lst)
        (if (pred? (car lst))
            #t
            (loop (cdr lst))))

       (else
        (error "Improper list" lst)))))
  
  (if (or (not (list? form))
          (not (eq? 'package (car form))))
      (error "Invalid package metadata" form))
  (let* ((tbl (list->table (cdr form)))

         (one
          (lambda (name pred? #!key require?)
            (let ((lst (table-ref tbl name #f)))
              (if (and require? (not lst))
                  (error "Package attribute required:" name))
              (and lst
                   (if (or (not (pair? lst))
                           (not (null? (cdr lst)))
                           (not (pred? (car lst))))
                       (error "Invalid package metadata"
                              (list name lst))
                       (car lst))))))
         (list
          (lambda (name pred?)
            (let ((lst (table-ref tbl name #f)))
              (and lst
                   (if (or (not (list? lst))
                           (find-one? (lambda (x) (not (pred? x)))
                                      lst))
                       (error "Invalid package metadata"
                              (list name lst))
                       lst))))))
    (make-package-metadata
     (let ((v (symbol->version (one 'version symbol? require?: #t))))
       (if (not (version-build v))
           (error "Complete version required" (version->symbol v)))
       v)
     (one 'maintainer string?)
     (one 'author string?)
     (one 'homepage string?)
     (one 'description string?)
     (list 'keywords symbol?)
     (list 'license symbol?)
     
     (list 'exported-modules symbol?)
     (one 'default-module symbol?)
     (one 'module-directory string?))))
     

;;; Package list

