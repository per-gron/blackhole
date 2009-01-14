;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       The actual system                          ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; I have tried to organize this file so that there is no code that
;; depends on code that is below itself as much as possible/convenient.

;; This file only contains the neccesary functions. Utilities are
;; supposed to be in a separare module in the standard library.

;;;; ---------- Package paths ----------

(define package-file "Packagefile")

(define (current-package-directory)
  (package-search-directory (current-directory)))

(define (package-search-directory dir #!optional orig-dir)
  (let ((pfile (path-expand package-file dir)))
    (if (file-exists? pfile)
        dir
        (let ((parent-dir (path-normalize ".." #f dir)))
          (if (equal? dir parent-dir)
              (or orig-dir dir)
              (package-search-directory
               parent-dir
               (or orig-dir dir)))))))

;;;; ---------- Packages ----------

(define-type package
  id: 786F06E1-BAF1-45A5-B31F-ED09AE93514F

  path
  (include unprintable: equality-skip: read-only:)
  (load unprintable: equality-skip: read-only:)
  (all-modules unprintable: equality-skip: read-only:)
  (calculate-info unprintable: equality-skip: read-only:)
  ;; Returns #t if yes, #f if no or 'not-compiled if the module is not
  ;; compiled at all.
  (needs-compile? unprintable: equality-skip: read-only:)
  (clean! unprintable: equality-skip: read-only:)
  (compile! unprintable: equality-skip: read-only:))

;;;; ---------- Module info ----------

(define-type module-info
  id: 726DB40B-AB18-4396-A570-BB715B602DB9

  (defines read-only:)
  (macros read-only:)
  (private-defines read-only:)
  (private-macros read-only:)
  (uses read-only:)
  (options read-only:)
  (cc-options read-only:)
  (ld-options-prelude read-only:)
  (ld-options read-only:)
  (force-compile? read-only:)
  (environment read-only:))

(define empty-module-info
  (make-module-info
   '() '() '() '() '() '() "" "" "" #f builtin-environment))

;; True when a calculating info for a module
(define calcing (make-parameter #f))

;; The placement of this code is a little counterintuitive. It is
;; here, because this is the code that actually does the
;; calc-info. But it's the calcing dynamic variable that actually
;; triggers its use, which is in hygiene.scm.
(define-env load-environment
  "build-loadenv#"
  ()
  ((use
    (nh-macro-transformer
     (lambda pkgs
       (if (*build-loadenv-uses*)
           (*build-loadenv-uses*
            (append (*build-loadenv-uses*)
                    (resolve-modules pkgs))))
       (apply module-use pkgs))))

   (module
    (lambda (code env mac-env)
      (error "Ill-placed module" code)))
   
   (define
     (lambda (code env mac-env)
       (let* ((src (transform-to-lambda (cdr code)))
              (sym (synclosure-extract-form (car src))))
         (let ((var (if (<= 0 (*build-loadenv-private*))
                        *build-loadenv-defines*
                        *build-loadenv-private-defines*)))
           (environment-top-ns-add (top-environment)
                                   sym
                                   (module-namespace (current-module)))
           (var (cons sym (var))))
         (void))))
   
   (define-macro-register
     (lambda (form env mac-env)
       (let ((src (transform-to-lambda (cdr form)))
             (var (if (<= 0 (*build-loadenv-private*))
                      *build-loadenv-macros*
                      *build-loadenv-private-macros*)))
         (var (cons (car src) (var))))
       (void)))
   
   (let
       (lambda (code env mac-env)
         #f))
   
   (letrec
       (lambda (code env mac-env)
         #f))
   
   (lambda
       (lambda (code env mac-env)
         #f))
   
   (private
    (nh-macro-transformer
     (lambda ()
       (*build-loadenv-private*
        (- (*build-loadenv-private*) 1))
       (void))))
   
   (/private
    (nh-macro-transformer
     (lambda ()
       (*build-loadenv-private*
        (+ (*build-loadenv-private*) 1))
       (void))))
   
   (compile-options
    (nh-macro-transformer
     (lambda (#!key options
                    cc-options
                    ld-options-prelude
                    ld-options
                    force-compile)
       (if options
           (*build-loadenv-options*
            options))
       (if cc-options
           (*build-loadenv-cc-options*
            cc-options))
       (if ld-options-prelude
           (*build-loadenv-ld-options-prelude*
            ld-options-prelude))
       (if ld-options
           (*build-loadenv-ld-options*
            ld-options))
       (if force-compile
           (*build-loadenv-force-compile*
            force-compile))
       (void))))
   
   (c-declare
    (lambda args
      (void)))
   
   (c-initialize
    (lambda args
      (void)))
   
   (c-define-type
    (lambda args
      (void)))
   
   (c-lambda
    (lambda args
      (void)))
   
   (c-define
    (lambda args
      (void)))))

(define-macro (make-loadenv-vars . vars)
  (let ((syms (map (lambda (var)
                     (string->symbol
                      (string-append
                       "*build-loadenv-"
                       (symbol->string (car var))
                       "*")))
                   vars))
        (defaults (map cadr vars)))
    `(begin
       ,@(map (lambda (sym)
                `(define ,sym (make-parameter #f)))
              syms)
       
       (define (with-build-loadenv thunk)
         (parameterize
          ,(map (lambda (sym default)
                  `(,sym ,default))
                syms
                defaults)
          (thunk))))))

(make-loadenv-vars (defines '())
                   (macros '())
                   (private-defines '())
                   (private-macros '())
                   (uses '())
                   (options '())
                   (cc-options "")
                   (ld-options-prelude "")
                   (ld-options "")
                   (force-compile #f)
                   (private 0))

(define (module-info-calculate module #!optional filename)
  (with-build-loadenv
   (lambda ()
     (let ((env #f))
       (if filename
           (parameterize
            ((top-environment (make-top-environment
                               (resolve-one-module module)))
             (calcing #t))
            (expand-macro
             (expr*:strip-locationinfo
              (file-read-as-expr filename)))
            (set! env (top-environment))))
       
       (make-module-info (*build-loadenv-defines*)
                         (*build-loadenv-macros*)
                         (*build-loadenv-private-defines*)
                         (*build-loadenv-private-macros*)
                         (*build-loadenv-uses*)
                         (*build-loadenv-options*)
                         (*build-loadenv-cc-options*)
                         (*build-loadenv-ld-options-prelude*)
                         (*build-loadenv-ld-options*)
                         (*build-loadenv-force-compile*)
                         env)))))

;;;; ---------- Module paths ----------

;; A module path is a symbol, for instance /srfi/1

(define (module-path-absolute? p)
  (let ((str (symbol->string p)))
    (and (positive? (string-length str))
         (char=? #\/ (string-ref str 0)))))


;; Removes extraneous "./" and "../" in a URI path. Copied from the
;; uri module
(define (remove-dot-segments str)
  (let* ((in-len (string-length str))
         (res (make-string in-len)))
    ;; i is where we are in the source string,
    ;; j is where we are in the result string,
    ;; segs is a list, used as a stack, of the indices of the
    ;; previously encountered path segments in the result string.
    (letrec
        ((new-segment
          (lambda (i j segs)
            (let* ((segment-start (car segs))
                   (segment-length (- j segment-start 1)))
              (cond
               ;; Check for .
               ((and (= 1 segment-length)
                     (char=? #\. (string-ref res segment-start)))
                (loop (+ 1 i) segment-start segs))
 
               ;; Check for ..
               ((and (= 2 segment-length)
                     (char=? #\. (string-ref res segment-start))
                     (char=? #\. (string-ref res (+ 1 segment-start))))
                (cond
                 ;; Take care of the "/../something" special case; it
                 ;; should return "/something" and not "something".
                 ((and (= 1 segment-start)
                       (char=? #\/ (string-ref res 0)))
                  (loop (+ 1 i) 1 '(1)))
                 
                 ;; This is needed because the code in the else clause
                 ;; assumes that segs is a list of length >= 2
                 ((zero? segment-start)
                  (loop (+ 1 i) 0 segs))
 
                 (else
                  (loop (+ 1 i) (cadr segs) (cdr segs)))))
               
               ;; Check for the end of the string
               ((>= (+ 1 i) in-len)
                j)
 
               (else
                (loop (+ 1 i) j (cons j segs)))))))
         (loop
          (lambda (i j segs)
            (if (>= i in-len)
                (new-segment i j segs)
                (let ((chr (string-ref str i)))
                  (string-set! res j chr)
                  (if (char=? chr #\/)
                      (new-segment i (+ 1 j) segs)
                      (loop (+ 1 i) (+ 1 j) segs)))))))
      (let ((idx (loop 0 0 '(0))))
        (substring res 0 idx)))))

(define (module-path-absolutize p #!optional ref)
  (if (module-path-absolute? p)
      p
      (string->symbol
       (remove-dot-segments
        (if ref
            (string-append (symbol->string ref)
                           "/../"
                           (symbol->string p))
            (string-append "/" (symbol->string p)))))))

;;;; ---------- Module objects ----------

;; A module object consists of the package and the absolute module name

(define-type module
  id: 48AC4955-EC9E-466F-B8EF-B7F0B9BBC63D

  ;; The package object
  (package read-only:)
  ;; The absolute module name
  (path read-only:))

;;;; ---------- Package resolvers ----------

;; A package resolver is a function that takes a list (like (/srfi/1)
;; or (spork /core)), the current package and the current module path
;; (or #f) and returns a module identifier.

(define (current-package-resolver pkg path . ids)
  (map (lambda (id)
         (make-module
          pkg
          (module-path-absolutize id path)))
       ids))

(define (make-package-resolver pkg)
  (lambda (_ path . ids)
    (map (lambda (id)
           (if (not (module-path-absolute? id))
               (error "Module name must be absolute")
               (make-module pkg id)))
         ids)))

(define (make-singleton-package-resolver pkg)
  (lambda (_ __)
    (list (make-module pkg #f))))

(define package-resolvers '())

(define (resolve-module name #!optional cm)
  (if (module? name)
      (list name)
      (call-with-values
          (lambda ()
            (cond
             ((symbol? name)
              (values 'here (list name)))
             ((pair? name)
              (values (car name) (cdr name)))
             (else
              (error "Invalid module identifier:" name))))
        (lambda (resolver-id resolver-args)
          (let ((resolver (let ((pair (assq resolver-id
                                            package-resolvers)))
                            (and pair (cdr pair)))))
            (if (not resolver)
                (error "Package resolver not found:" resolver-id)
                (apply resolver
                       `(,(if cm
                              (module-package cm)
                              (current-package))
                         ,(if cm
                              (module-path cm)
                              (let ((current-mod (current-module)))
                                (if current-mod
                                    (module-path current-mod)
                                    (repl-path))))
                         .
                         ,resolver-args))))))))

(define (resolve-modules names #!optional cm)
  (apply append (map (lambda (x)
                       (resolve-module x cm))
                     names)))

(define (resolve-one-module name #!optional cm)
  (let ((res (resolve-module name cm)))
    (if (or (null? res)
            (not (pair? res))
            (not (null? (cdr res))))
        (error "Module identifier must refer to one module only:" name)
        (car res))))


;;;; ---------- Module utility functions ----------

(define (current-module)
  (environment-module (top-environment)))

(define *repl-package* #f)

(define (repl-package)
  (if (or (not *repl-package*)
          (not (equal? (path-normalize (current-directory))
                       (package-path *repl-package*))))
      (set! *repl-package* (package (current-package-directory))))
  *repl-package*)

(define (repl-path)
  (let ((cd (current-directory))
        (pd (package-path (repl-package))))
    (if (not (equal? (substring cd 0 (string-length pd))
                     pd))
        (error "Internal error in repl-path:" cd pd))
    (string->symbol
     (substring cd
                (- (string-length pd) 1)
                (string-length cd)))))

(define (current-package)
  (let ((cm (current-module)))
    (if cm
        (module-package (current-module))
        (repl-package))))


(define *calc-info-cache* (make-parameter #f))

(define (with-module-cache thunk)
  (if (*calc-info-cache*)
      (thunk)
      (parameterize ((*calc-info-cache* (make-table)))
                    (thunk))))

(define (make-module-util-function fn)
  (lambda (mod)
    (with-module-cache
     (lambda ()
       (fn (resolve-one-module mod))))))

(define module-info
  (make-module-util-function
   (lambda (mod)
     (let ((pp (package-path (module-package mod)))
           (mp (module-path mod)))
       (or (table-ref (*calc-info-cache*)
                      (cons pp mp)
                      #f)
           (let ((ret ((package-calculate-info (module-package mod))
                       mod)))
             (table-set! (*calc-info-cache*) (cons pp mp) ret)
             ret))))))

(define module-include
  (make-module-util-function
   (lambda (mod)
     ((package-include (module-package mod)) mod))))

(define module-load
  (make-module-util-function
   (lambda (mod)
     ((package-load (module-package mod)) mod))))

(define module-needs-compile?
  (make-module-util-function
   (lambda (mod)
     ((package-needs-compile? (module-package mod)) mod))))

(define (module-compile! mod #!optional continue-on-error)
  (let ((mod (resolve-one-module mod)))
    (with-module-cache
     (lambda ()
       (with-exception-catcher
        (lambda (e)
          (if continue-on-error
              (begin
                (display "Warning: Compilation failed: ")
                (display-exception e)
                #f)
              (raise e)))
        (lambda ()
          ((package-compile! (module-package mod)) mod)))))))

(define module-clean!
  (make-module-util-function
   (lambda (mod)
     ((package-clean! (module-package mod)) mod))))

(define module-namespace
  (let ((fn
         (make-module-util-function
          (lambda (mod)
            (let ((path (module-path mod)))
              (and path
                   (string-append
                    (path-strip-directory
                     (symbol->string path))
                    "#")))))))
    (lambda (mod)
      ;; This function might be called with #f as argument
      (if mod (fn mod) ""))))

(define (module-use . modules)
  (let ((modules (resolve-modules modules)))
    (with-module-cache
     (lambda ()
       (let* ((te (top-environment)))
         (for-each
          (lambda (module)
            (for-each (lambda (args)
                        (apply load-once args))
                      (module-load module))
            
            (let ((info (module-info module))
                  (modstr (module-namespace module)))
              (for-each
               (lambda (x)
                 (environment-top-ns-add te
                                         x
                                         modstr))
               (module-info-defines info))
              (for-each
               (lambda (x)
                 (environment-top-ns-macro-add
                  te
                  x
                  (eval-no-hook (string->symbol
                                 (string-append
                                  modstr
                                  (symbol->string x)
                                  "|||macro|||")))
                  (module-info-environment info)))
               (module-info-macros info))))
          modules))
       `(begin
          ,@(map (lambda (module)
                   (module-include module))
                 modules))))))

(define module-module
  (make-module-util-function
   (lambda (mod)
     `(begin
        (use ,mod)
        (##namespace (,(module-namespace mod)))
        ,@*global-includes*
        (use ,@(module-info-uses (module-info mod)))))))


;;;; ---------- Package utility functions ----------

(define *load-once-registry* (make-table))

;; Internal utility
(define (module-prelude module #!optional only-compile?)
  (lambda (src compiling?)
    (if (and only-compile? (not compiling?))
        src
        (begin
          (build-hook (lambda (src compiling?) src))
          `(begin
             (##namespace (,(module-namespace module)))
             ,@*global-includes*
             ,src)))))

(define (load-once file-with-extension #!optional module)
  (let ((module (and module (resolve-one-module module))))
    (parameterize
     ((top-environment (make-top-environment module))
      (calcing #f))
     (with-module-cache
      (lambda ()
        (let* ((file (path-strip-trailing-directory-separator
                      (path-strip-extension
                       (path-normalize file-with-extension))))
               (scm (string-append file ".scm"))
               (scm-exists? (file-exists? scm))
               (time (table-ref *load-once-registry* file #f)))
          (if (not (equal? time (or (not scm-exists?)
                                    (file-last-changed-seconds scm))))
              (let ((info (and module (module-info module))))
                ;; If the file needs to be compiled, compile it (if it
                ;; isn't compiled and is set to force-compile or if the
                ;; scm file is newer than the object file.)
                (if (and module
                         (let ((res (module-needs-compile? module)))
                           (if (module-info-force-compile? info)
                               res
                               (eq? res #t))))
                    (begin
                      (print "load-once: " file " is being compiled...\n")
                      (module-compile! module)))
                ;; Load it.
                (let ((ret (with-exception-catcher
                            (lambda (e)
                              ;; Comparing error *strings* is a REALLY
                              ;; ugly hack, but it'll do for now.
                              (if (and
                                   (os-exception? e)
                                   (equal? "Can't load a given object \
                                          file more than once"
                                           (err-code->string
                                            (os-exception-code e))))
                                  "(Did not load again)"
                                  (raise e)))
                            (lambda ()
                              (with-build-loadenv
                               (lambda ()
                                 (parameterize
                                  ((build-hook (module-prelude module)))
                                  (load file))))))))
                  (table-set! *load-once-registry*
                              file (or (not scm-exists?)
                                       (file-last-changed-seconds scm)))
                  ret)))))))))

(define (compile-with-options module
                              fn
                              #!key (options '()) (cc-options "")
                              (ld-options-prelude "") (ld-options ""))
  (##gc) ;; Avoid out-of-memory related crashes
  (parameterize
   ((top-environment (make-top-environment
                      (resolve-one-module module)))
    (build-hook (module-prelude module #t)))
   (compile-file fn
                 options: (append options *compiler-options*)
                 cc-options: (string-append
                              cc-options " " *compiler-cc-options*)
                 ld-options-prelude: (string-append
                                      ld-options-prelude
                                      " "
                                      *compiler-ld-options-prelude*)
                 ld-options: (string-append
                              ld-options " " *compiler-ld-options*))))

(define (find-file path mod)
  (string-append path
                 "/"
                 (symbol->string (module-path mod))))

(define (find-ext path mod ext)
  (string-append (find-file path mod) "." ext))

(define (find-scm path mod)
  (find-ext path mod "scm"))

(define (object-files path mod)
  (let* ((id (module-path mod))
         (name (symbol->string id))
         (dir (path-directory
               (string-append path "/" name)))
         (begin-str (string-append (path-strip-directory
                                    name)
                                   ".o")))
    (map (lambda (fn)
           (path-expand fn dir))
         (filter (lambda (fn) (string-begins-with fn begin-str))
            (directory-files dir)))))

(define (object-file-extract-number fn)
  (or (string->number
       (substring fn
                  (let loop ((i (string-length fn)))
                    (if (zero? i)
                        0
                        (let ((chr
                               (char->integer
                                (string-ref fn (- i 1)))))
                          (if (and (>= chr (char->integer #\0))
                                   (<= chr (char->integer #\9)))
                              (loop (- i 1))
                              i))))
                  (string-length fn)))
      0))

(define (last-object-file path mod)
  (let ((lst (object-files path mod))
        (max-num -1)
        (res #f))
    (let loop ((lst lst))
      (if (not (null? lst))
          (let ((num (object-file-extract-number (car lst))))
            (if (> num max-num)
                (begin
                  (set! max-num num)
                  (set! res (car lst))))
            (loop (cdr lst)))))
    res))

(define (clean-file path mod)
  (for-each delete-file
            (object-files path mod)))


;;;; ---------- Package implementations ----------

(define (package path)
  (let ((path (path-normalize path)))
    (make-package
     ;; path
     path
     
     ;; include
     (lambda (mod)
       (let* ((info (module-info mod))
              (syms (append (module-info-defines info)
                            (module-info-macros info))))
         `(begin
            ;; Don't include the namespace directive if no symbols are
            ;; declared; it will mean an entierly other thing then.
            ,@(if (null? syms)
                  '()
                  `((##namespace (,(module-namespace mod)
                                  ,@syms)))))))
     
     ;; load
     (lambda (mod)
       (cons (list (find-file path mod)
                   mod)
             (apply append
                    (map module-load
                         (module-info-uses
                          (module-info mod))))))
    
     ;; all-modules
     (lambda ()
       (find-files-with-ext-remove-ext ".scm" path))
     
     ;; calculate-info
     (lambda (mod)
       (module-info-calculate mod (find-scm path mod)))
     
     ;; needs-compile?
     (lambda (mod)
       (let ((of (last-object-file path mod)))
         (if of
             (not (file-newer? of (find-scm path mod)))
             'not-compiled)))
             
     
     ;; clean!
     (lambda (mod)
       (clean-file path mod))
     
     ;; compile!
     (lambda (mod)
       (let ((info (module-info mod)))
         (with-build-loadenv ;; For *build-loadenv-uses*
          (lambda ()
            (let ((result (compile-with-options
                           mod
                           (find-scm path mod)
                           options: (module-info-options info)
                           cc-options: (module-info-cc-options info)
                           ld-options-prelude: (module-info-ld-options-prelude
                                                info)
                           ld-options: (module-info-ld-options info))))
              (if (not result)
                  (error "Compilation failed"))))))))))

(define build-package
  (make-package
   ;; path
   'build
   ;; include
   (lambda (mod)
     '(##namespace ("build#"
                    make-package
                    package-path
                    package-include
                    package-load
                    package-all-modules
                    package-calculate-info
                    package-needs-compile?
                    package-clean!
                    package-compile!

                    make-module-info
                    module-info-defines
                    module-info-macros
                    module-info-private-defines
                    module-info-private-macros
                    module-info-uses
                    module-info-options
                    module-info-cc-options
                    module-info-ld-options-prelude
                    module-info-ld-options
                    module-info-force-compile?
                    module-info-environment
                    module-info-calculate

                    module-path-absolute?
                    module-path-absolutize

                    resolve-module
                    resolve-modules
                    resolve-one-module

                    repl-package
                    current-module
                    current-package

                    with-module-cache

                    make-module
                    module-package
                    module-path
                    module-info
                    module-include
                    module-load
                    module-needs-compile?
                    module-compile!
                    module-clean!
                    module-namespace
                    module-use
                    module-module

                    package
                    build-package)))
   ;; load
   (lambda (mod) '())
   ;; all-modules
   (lambda () '())
   ;; calculate-info
   (lambda (mod) empty-module-info)
   ;; needs-compile?
   (lambda (mod) #f)
   ;; clean!
   (lambda (mod) #f)
   ;; compile!
   (lambda (mod) #f)))

(define termite-package
  (make-package
   ;; path
   'termite
   ;; include
   (lambda (mod)
     '(##include "~~/lib/termite/termite#.scm"))
   ;; load
   (lambda (mod)
     '(("~~/lib/termite/termite")))
   ;; all-modules
   (lambda () '())
   ;; calculate-info
   (lambda (mod) empty-module-info)
   ;; needs-compile?
   (lambda (mod) #f)
   ;; clean!
   (lambda (mod) #f)
   ;; compile!
   (lambda (mod) #f)))

(define ssax-sxml-package
  (make-package
   ;; path
   'ssax-sxml
   ;; include
   (lambda (mod)
     '(##namespace ("ssax-sxml#"
                    ssax:xml->sxml
                    srl:sxml->xml
                    srl:sxml->html
                    sxpath
                    ssax:multi-parser)))
   ;; load
   (lambda (mod)
     '(("~~/lib/ssax-sxml/ssax-sxml")))
   ;; all-modules
   (lambda () '())
   ;; calculate-info
   (lambda (mod) empty-module-info)
   ;; needs-compile?
   (lambda (mod) #f)
   ;; clean!
   (lambda (mod) #f)
   ;; compile!
   (lambda (mod) #f)))

;;;; ---------- Hack for configuration ----------

;; Variables declared here are used all over the place.

;; Configuration directives
(define *global-includes* #f)
(define *compiler-options* '())
(define *compiler-cc-options* "")
(define *compiler-ld-options-prelude* "")
(define *compiler-ld-options* "")

;; Include configuration
(##include "conf.scm")

;; Fill in with default values
(set! *global-includes*
      (or *global-includes*
          `((##include "~~/lib/gambit#.scm"))))
