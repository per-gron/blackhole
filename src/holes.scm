;;; Utilities

;; TODO This is already defined in util.scm
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
  (let* ((len (length list))
         (str (make-string len)))
    (let loop ((i (- len 1))
               (list list))
      (cond
       ((pair? list)
        (string-set! str i (car list))
        (loop (- i 1) (cdr list)))))
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

(define version-complete? version-build)

(define (version<? a b)
  (if (not (and (version-complete? a)
                (version-complete? b)))
      (error "Can't compare incomplete versions" a b))
  (let ((a-maj (version-major a))
        (b-maj (version-major b))
        (a-min (version-minor a))
        (b-min (version-major b))
        (a-b (version-build a))
        (b-b (version-build b)))
    (or (< a-maj b-maj)
        (and (= a-maj b-maj)
             (or (< a-min b-min)
                 (and (= a-min b-min)
                      (< a-b b-b)))))))

(define (string->version str #!key force-complete?)
  (if (not (string? str))
      (error "Expected string" str))
  (let* ((str-len (string-length str))
         (str-no-v
          (if (> str-len 1)
              (substring str 1 str-len)
              (error "Invalid format" str)))
         (split-string (string-split #\. str-no-v))
         (split-string-len (length split-string)))
    (if (not (<= 0 split-string-len 3))
        (error "Invalid format" str))
    (let ((s->i
           (lambda (str)
             (let ((res (string->number str)))
               (if (or (not (integer? res))
                       (< res 0))
                   (error "Invalid format" res str))
               res))))
      (let ((res
             (make-version (and (>= split-string-len 1)
                                (s->i (car split-string)))
                           (and (>= split-string-len 2)
                                (s->i (cadr split-string)))
                           (and (= split-string-len 3)
                                (s->i (caddr split-string))))))
        (if (and force-complete?
                 (not (version-complete? res)))
            (error "Version is not complete" str))
        res))))

(define (symbol->version str #!key force-complete?)
  (string->version (symbol->string str)
                   force-complete?: force-complete?))

(define (version->string v)
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
           '()))))

(define (version->symbol v)
  (string->symbol (version->string v)))

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

(define (parse-package-metadata form)
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
     (or (one 'module-directory string?)
         ""))))

(define (load-package-metadata fn)
  (with-input-from-file fn
    (lambda ()
      (parse-package-metadata (read)))))
     

;;; Package list

(define (load-package-list)
  ;; TODO
  '((sack
     ("http://...."
      (package
       (version v0.1.1)
       (maintainer "Per Eckerdal <per dot eckerdal at gmail dot com>")
       (author "Per Eckerdal <per dot eckerdal at gmail dot com>")
       (homepage "http://example.com")
       (description "An example package")
       (keywords example i/o)
       (license lgpl/v2.1 mit)

       (exported-modules server
                         lala
                         hello/duh)
       (default-module lala)
       (module-directory "src")
       
       (depends
        (sack (>= v1))
        pregexp))))))

(define (parse-package-list package-list)
  (list->table
   (map (lambda (package)
          (cons
           (car package)
           (map (lambda (package-version-desc)
                  (if (not (= 2 (length package-version-desc)))
                      (error "Invalid package version descriptor"
                             package-version-desc))
                  (cons (car package-version-desc)
                        (parse-package-metadata
                         (cadr package-version-desc))))
             (cdr package))))
     package-list)))


;;; Local packages


(define local-packages-dir
  "/Users/per/prog/gambit/blackhole/work/pkgs")

(define pkgfile-name
  "pkgfile")

(define-type installed-package
  id: EC2E4078-EDCA-4BE4-B81E-2B60468F042D
  
  (name read-only:)
  (version read-only:)
  (dir read-only:)
  (metadata installed-package-metadata/internal
            installed-package-metadata-set!
            init: #f))

(define (installed-package<? a b)
  (let ((a-name (installed-package-name a))
        (b-name (installed-package-name b)))
    (or (string<? a-name b-name)
        (and (string=? a-name b-name)
             (version<? (installed-package-version a)
                        (installed-package-version b))))))

(define (installed-package-metadata ip)
  (let ((md (installed-package-metadata/internal ip)))
    (or md
        (let* ((pkg-filename (path-expand
                              "pkgfile"
                              (installed-package-dir ip)))
               (md (if (file-exists? pkg-filename)
                       (load-package-metadata
                        pkg-filename)
                       (error "Pkgfile does not exist:"
                              pkg-filename))))
          (installed-package-metadata-set! ip md)
          md))))

(define (installed-packages #!optional
                            (pkgs-dir local-packages-dir))
  (let ((pkg-dirs
         (filter (lambda (x)
                   (is-directory? (path-expand x pkgs-dir)))
                 (if (file-exists? pkgs-dir)
                     (directory-files pkgs-dir)
                     '()))))
    (list->tree
     (map (lambda (pkg-dir)
            (let ((version-str
                   (last (string-split #\- pkg-dir))))
              (if (= (string-length version-str)
                     (string-length pkg-dir))
                  (error "Invalid package directory name" pkg-dir))
              (let ((version
                     (string->version
                      (last (string-split #\- pkg-dir))
                      force-complete?: #t))
                    (pkg-name
                     (substring pkg-dir
                                0
                                (- (string-length pkg-dir)
                                   (string-length version-str)
                                   1))))
                (make-installed-package
                 pkg-name
                 version
                 (path-expand pkg-dir pkgs-dir)))))
       pkg-dirs)
     installed-package<?)))


;;; Module loader and resolver

(define (package-module-resolver _ path relative . ids)
  (map (lambda (id)
         (make-module-reference
          loader
          (if relative
              id
              (loader-path-absolutize loader id path))))
    ids))

(define package-loader
  (make-loader
   name:
   'local

   path-absolute?:
   path-absolute?
   
   path-absolutize:
   (lambda (path #!optional ref)
     (path-normalize (string-append (symbol->string path) ".scm")
                     #f ;; Don't allow relative paths
                     (if ref
                         (path-normalize
                          ;; This ensures that (path-directory ref)
                          ;; actually exists. Otherwise path-normalize
                          ;; might segfault.
                          (path-directory ref))
                         (current-directory))))
   
   load-module:
   (lambda (path)
     (let ((ref (make-module-reference local-loader path)))
       (call-with-values
           (lambda ()
             (load-module-from-file ref path))
         (lambda (invoke-runtime
                  invoke-compiletime
                  visit
                  info-alist)
           (make-loaded-module
            invoke-runtime: invoke-runtime
            invoke-compiletime: invoke-compiletime
            visit: visit
            info: (make-module-info-from-alist ref info-alist)
            stamp: (local-loader-get-stamp path)
            reference: ref)))))

   compare-stamp:
   (lambda (path stamp)
     (= (local-loader-get-stamp path)
        stamp))

   module-name:
   (lambda (path)
     (cond ((symbol? path)
            (symbol->string path))
           ((string? path)
            (path-strip-directory
             (path-strip-extension path)))
           (else
            (error "Invalid path" path))))))
