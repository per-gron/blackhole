;; TODO
;; only
;; except
;; add-prefix
;; rename
;; for


;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       The actual system                          ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; I have tried to organize this file so that there is no code that
;; depends on code that is below itself as much as possible/convenient.

;; This file only contains the neccesary functions. Utilities are
;; supposed to be in a separare module in the standard library.

;;;; ---------- Loaders ----------

(define-type loader
  id: 786F06E1-BAF1-45A5-B31F-ED09AE93514F

  ;; Returns an s-expression with code that use should expand to when
  ;; a module is used.
  (include unprintable: equality-skip: read-only:)
  ;; Returns a list of lists. The inner list is arguments to the
  ;; load-once function, will be called like (apply load-once ...)
  (load unprintable: equality-skip: read-only:)
  ;; Returns a module-info object for the given module
  (calculate-info unprintable: equality-skip: read-only:)
  ;; Takes a relative module identifier symbol and optionally an
  ;; origin path, to which the path should be interpreted relative
  ;; to, and returns the object that should be in the path field of a
  ;; module object.
  (path-absolutize unprintable: equality-skip: read-only:)
  ;; Takes a module and returns a string for the name of the module
  (module-name unprintable: equality-skip: read-only:)
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
  (exports read-only:)
  (uses read-only:)
  (options read-only:)
  (cc-options read-only:)
  (ld-options-prelude read-only:)
  (ld-options read-only:)
  (force-compile? read-only:)
  (environment read-only:))

(define empty-module-info
  (make-module-info
   '() '() '() '() '() "" "" "" #f builtin-environment))

;; True when a calculating info for a module
(define calcing (make-parameter #f))

;; The placement of this code is a little counterintuitive. It is
;; here, because this is the code that actually does thea
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
      (error "Ill-placed module form" code)))
   
   (define
     (lambda (code env mac-env)
       (let* ((src (transform-to-lambda (cdr code)))
              (sym (synclosure-extract-form (car src))))
         (environment-top-ns-add (top-environment)
                                 sym ;; Public (exported) name
                                 sym ;; Private (actual) name
                                 (module-namespace (current-module)))
         (*build-loadenv-defines*
          (cons sym (*build-loadenv-defines*)))
         (void))))
   
   (define-macro-register
     (lambda (form env mac-env)
       (let ((src (transform-to-lambda (cdr form))))
         (*build-loadenv-macros*
          (cons (car src) (*build-loadenv-macros*))))
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
       (pp (list "private is deprecated" (top-environment)))
       (void))))
   
   (/private
    (nh-macro-transformer
     (lambda ()
       (pp (list "/private is deprecated" (top-environment)))
       (void))))
   
   (export
    (lambda (code env mac-env)
      (*build-loadenv-exports*
       (append (cdr (extract-synclosure-crawler code))
               (or (*build-loadenv-exports*) '())))))
   
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
      (*build-loadenv-force-compile* #t)
      (void)))
   
   (c-initialize
    (lambda args
      (*build-loadenv-force-compile* #t)
      (void)))
   
   (c-define-type
    (lambda args
      (*build-loadenv-force-compile* #t)
      (void)))
   
   (c-lambda
    (lambda args
      (*build-loadenv-force-compile* #t)
      (void)))
   
   (c-define
    (lambda args
      (*build-loadenv-force-compile* #t)
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
                   (exports #f)
                   (uses '())
                   (options '())
                   (cc-options "")
                   (ld-options-prelude "")
                   (ld-options "")
                   (force-compile #f))

(define (interpret-loadenv-exports module env)
  (let* ((exps (*build-loadenv-exports*))
         (err (lambda ()
                (error "Invalid exports declaration" x module)))
         (export
          (lambda (name as)
            (if (not (and (symbol? name)
                          (symbol? as)))
                (err))
            (cond
             ((environment-top-ns-macro-get env name) =>
              (lambda (mac)
                (list as name 'mac (car mac) (cadr mac))))

             ((environment-top-ns-get env name) =>
              (lambda (def)
                (list as name 'def (car def))))

             (else
              (error "Name can't be exported because it isn't defined"
                     name))))))
    (if exps
        (apply
         append
         (map (lambda (x)
                (cond
                 ((symbol? x)
                  (list (export x x)))
                 
                 ((and (list? x)
                       (eq? 'rename (car x)))
                  (map (lambda (x)
                         (if (not (and (list? x)
                                       (= (length x) 2)))
                             (err))
                         (apply export x))
                       (cdr x)))
                 
                 (else
                  (err))))
              exps))
        (let ((ns (module-namespace module)))
          (append (map (lambda (name)
                         (list name name 'def ns))
                       (*build-loadenv-defines*))
                  (map (lambda (name)
                         (let ((mac (environment-top-ns-macro-get
                                     env
                                     name)))
                           (list name
                                 name
                                 'mac
                                 (car mac)
                                 (cadr mac))))
                       (*build-loadenv-macros*)))))))

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
       
       (make-module-info
        (*build-loadenv-defines*)
        (*build-loadenv-macros*)
        (interpret-loadenv-exports module env)
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

;; A module object consists of the loader and the module path

(define-type module
  id: 48AC4955-EC9E-466F-B8EF-B7F0B9BBC63D

  ;; The loader object
  (loader read-only:)
  ;; The absolute module path. It must be a hashable object.
  (path read-only:))

;;;; ---------- Module resolvers ----------

;; A module resolver is a function that takes a list (like (/srfi/1)
;; or (spork /core)), the current loader and the current module path
;; (or #f) and returns a module identifier.

;; This is the 'here module resolver function
(define (current-module-resolver loader path . ids)
  (map (lambda (id)
         (make-module
          loader
          ((loader-path-absolutize loader) id path)))
       ids))

(define (package-module-resolver path)
  (lambda (_ __ . ids)
    (map (lambda (id)
           (make-module
            local-loader
            ((loader-path-absolutize local-loader) id path)))
         ids)))

;; This is a helper function for singleton loaders, for instance 'build
(define (make-singleton-module-resolver pkg)
  (lambda (_ __)
    (list (make-module pkg #f))))

(define *module-resolvers* '())

(define (resolve-module name #!optional cm)
  (if (module? name)
      (list name)
      (call-with-values
          (lambda ()
            (cond
             ((symbol? name)
              (values 'here (list name)))
             ((string? name)
              (values 'lib (list name)))
             ((pair? name)
              (values (car name) (cdr name)))
             (else
              (error "Invalid module identifier:" name))))
        (lambda (resolver-id resolver-args)
          (let ((resolver (let ((pair (assq resolver-id
                                            *module-resolvers*)))
                            (and pair (cdr pair)))))
            (if (not resolver)
                (error "Module resolver not found:" resolver-id)
                (apply resolver
                       `(,(if cm
                              (module-loader cm)
                              (current-loader))
                         ,(if cm
                              (module-path cm)
                              (let ((current-mod (current-module)))
                                (and current-mod (module-path current-mod))))
                         ,@resolver-args))))))))

(define (resolve-modules names #!optional cm)
  (apply append
         (map (lambda (x)
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

(define (current-loader)
  (let ((cm (current-module)))
    (if cm
        (module-loader (current-module))
        local-loader)))


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
     (let ((mp (module-path mod)))
       (or (table-ref (*calc-info-cache*) mp #f)
           (let ((ret ((loader-calculate-info
                        (module-loader mod))
                       mod)))
             (table-set! (*calc-info-cache*) mp ret)
             ret))))))

(define module-include
  (make-module-util-function
   (lambda (mod)
     ((loader-include (module-loader mod)) mod))))

(define module-load
  (make-module-util-function
   (lambda (mod)
     ((loader-load (module-loader mod)) mod))))

(define module-needs-compile?
  (make-module-util-function
   (lambda (mod)
     ((loader-needs-compile? (module-loader mod)) mod))))

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
          ((loader-compile! (module-loader mod)) mod)))))))

(define module-clean!
  (make-module-util-function
   (lambda (mod)
     ((loader-clean! (module-loader mod)) mod))))

(define module-namespace
  (let ((fn
         (make-module-util-function
          (lambda (mod)
            (let ((path (module-path mod)))
              (and path
                   (string-append
                    ((loader-module-name (module-loader mod)) mod)
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
                 (if (eq? 'def (caddr x))
                     ;; Regular define
                     (environment-top-ns-add
                      te
                      (car x) ;; The name it's imported as
                      (cadr x) ;; The name it's imported from
                      (cadddr x)) ;; The namespace the variable is defined
                     ;; Macro
                     (environment-top-ns-macro-add
                      te
                      ;; The name it's imported as
                      (car x)
                      ;; The macro procedure. eval is used and not the
                      ;; procedure in the x structure to ensure that
                      ;; we use the compiled macro if it is compiled.
                      (eval-no-hook (string->symbol
                                     (string-append
                                      modstr
                                      (symbol->string
                                       ;; (cadr x) is the name of the
                                       ;; macro as it was defined
                                       ;; originally
                                       (cadr x))
                                      "|||macro|||")))
                      ;; The macro's environment
                      (cadddr (cdr x)))))
               (module-info-exports info))))
          modules))
       ;;`(begin  TODO Remove this
       ;;   ,@(map (lambda (module)
       ;;            (module-include module))
       ;;          modules))
       (void)))))

(define module-module
  (make-module-util-function
   (lambda (mod)
     `(begin
        (use ,mod)
        (##namespace (,(module-namespace mod)))
        ,@*global-includes*
        (use ,@(module-info-uses (module-info mod)))))))


;;;; ---------- Loader utility functions ----------

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

(define (object-files path)
  (let* ((dir (path-directory path))
         (begin-str (string-append (path-strip-directory
                                    (path-strip-extension path))
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

(define (last-object-file path)
  (let ((lst (object-files path))
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

(define (clean-file path)
  (for-each delete-file
            (object-files path)))


;;;; ---------- Loader implementations ----------

(define local-loader
  (make-loader
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
     (cons (list (path-strip-extension (module-path mod))
                 mod)
           (apply append
                  (map module-load
                       (module-info-uses
                        (module-info mod))))))
   
   ;; calculate-info
   (lambda (mod)
     (module-info-calculate mod (module-path mod)))
   
   ;; path-absolutize
   (lambda (path #!optional ref)
     (path-normalize (string-append (symbol->string path) ".scm")
                     #f ;; Don't allow relative paths
                     (if ref
                         (path-directory ref)
                         (current-directory))))

   ;; module-name
   (lambda (mod)
     (path-strip-directory
      (path-strip-extension
       (module-path mod))))
   
   ;; needs-compile?
   (lambda (mod)
     (let* ((path (module-path mod))
            (of (last-object-file path)))
       (if of
           (not (file-newer? of path))
           'not-compiled)))
   
   ;; clean!
   (lambda (mod)
     (clean-file (module-path mod)))
   
   ;; compile!
   (lambda (mod)
     (let ((info (module-info mod)))
       (with-build-loadenv ;; For *build-loadenv-uses*
        (lambda ()
          (let ((result (compile-with-options
                         mod
                         (module-path mod)
                         options: (module-info-options info)
                         cc-options: (module-info-cc-options info)
                         ld-options-prelude: (module-info-ld-options-prelude
                                              info)
                         ld-options: (module-info-ld-options info))))
            (if (not result)
                (error "Compilation failed")))))))))

(define build-loader
  (make-loader
   ;; include
   (lambda (mod)
     '(##begin))
   ;; load
   (lambda (mod) '())
   ;; calculate-info
   (lambda (mod)
     (make-module-info
      '() '()
      (map (lambda (x)
             (list x x 'def "build#"))
           '(make-loader
             loader-include
             loader-load
             loader-calculate-info
             loader-needs-compile?
             loader-clean!
             loader-compile!
             
             make-module-info
             module-info-defines
             module-info-macros
             module-info-exports
             module-info-uses
             module-info-options
             module-info-cc-options
             module-info-ld-options-prelude
             module-info-ld-options
             module-info-force-compile?
             module-info-environment
             module-info-calculate
             
             resolve-module
             resolve-modules
             resolve-one-module
             
             current-module
             current-loader
             
             with-module-cache
             
             make-module
             module-loader
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
             
             module-deps
             module-compile/deps!
             module-clean/deps!
             module-generate-export-list
             
             loader
             build-loader))
      '() '() "" "" "" #f builtin-environment))
   ;; path-absolutize
   (lambda (path #!optional ref) #f)
   ;; module-name
   (lambda (mod) "build")
   ;; needs-compile?
   (lambda (mod) #f)
   ;; clean!
   (lambda (mod) #f)
   ;; compile!
   (lambda (mod) #f)))

(define termite-loader
  (make-loader
   ;; include
   (lambda (mod)
     '(##include "~~/lib/termite/termite#.scm"))
   ;; load
   (lambda (mod)
     '(("~~/lib/termite/termite")))
   ;; calculate-info
   (lambda (mod) empty-module-info)
   ;; path-absolutize
   (lambda (path #!optional ref) #f)
   ;; module-name
   (lambda (mod) "termite")
   ;; needs-compile?
   (lambda (mod) #f)
   ;; clean!
   (lambda (mod) #f)
   ;; compile!
   (lambda (mod) #f)))

(define ssax-sxml-loader
  (make-loader
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
   ;; calculate-info
   (lambda (mod) empty-module-info)
   ;; path-absolutize
   (lambda (path #!optional ref) #f)
   ;; module-name
   (lambda (mod) "ssax-sxml")
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

(set! *compiler-options* '(debug))
;;(set! *compiler-cc-options* "-I/usr/local/BerkeleyDB.4.7/include")
;;(set! *compiler-ld-options-prelude* "-L/usr/local/BerkeleyDB.4.7/lib")

(set! *module-resolvers*
      `((here . ,current-module-resolver)
        (build . ,(make-singleton-module-resolver
                   build-loader))
        (std . ,(package-module-resolver "~~/lib/module/std/"))
        (termite . ,(make-singleton-module-resolver
                     termite-loader))))

;; Fill in with default values
(set! *global-includes*
      (or *global-includes*
          `((##include "~~/lib/gambit#.scm"))))
