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

  (symbols read-only:)
  (exports read-only:)
  (imports read-only:)
  (uses read-only:)
  (options read-only:)
  (cc-options read-only:)
  (ld-options-prelude read-only:)
  (ld-options read-only:)
  (force-compile? read-only:)
  (environment read-only:))


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

;; This is a helper function for singleton loaders, for instance 'module
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



;;;; ---------- Import resolvers ----------

;; Import resolvers are the implementation of (only: [mod] names),
;; add-prefix:, and similar features. They are functions that take the
;; current module, and their arguments ((only: [mod] names ...) would
;; give the arguments '([mod] names ...) to the only: resolver) and
;; return two values: the imported symbols (in the same format as
;; module-info-exports) and a list of the modules that this import
;; depends on.

(define *import-resolvers* '())

(define (resolve-import val #!optional cm)
  (with-module-cache
   (lambda ()
     (cond
      ((and (pair? val)
            (keyword? (car val)))
       (let* ((resolver-id (car val))
              (resolver (let ((pair (assq resolver-id
                                          *import-resolvers*)))
                          (and pair (cdr pair)))))
         (if (not resolver)
             (error "Import resolver not found:" resolver-id)
             (apply resolver
                    (cons (or cm (current-module))
                          (cdr val))))))
      
      (else
       (let ((mods (resolve-module val cm)))
         (values (apply
                  append
                  (map (lambda (mod)
                         (module-info-exports
                          (module-info mod)))
                       mods))
                 mods)))))))

(define (resolve-imports vals #!optional cm)
  (with-module-cache
   (lambda ()
     (let ((defs '())
           (mods '()))
       (for-each (lambda (val)
                   (call-with-values
                       (lambda ()
                         (resolve-import val cm))
                     (lambda (def mod)
                       (set! defs (cons def defs))
                       (set! mods (cons mod mods)))))
                 vals)
       (values (flatten1 defs)
               (flatten1 mods))))))

(define (only-resolver cm mod . names)
  (call-with-values
      (lambda () (resolve-import mod cm))
    (lambda (defs modules)
      (values
       (map (lambda (name)
              (or (assq name defs)
                  (error "only: Symbol not defined" name mod)))
            names)
       modules))))

(define (except-resolver cm mod . names)
  (call-with-values
      (lambda () (resolve-import mod cm))
    (lambda (defs modules)
      (let ((def-clone (map (lambda (x) x) defs)))
        (for-each
         (lambda (name)
           (let ((found? #f))
             (set! def-clone
                   (remove!
                    (lambda (x)
                      (and (eq? (car x) name)
                           (begin
                             (set! found? #t)
                             #t)))
                    def-clone))
             (if (not found?)
                 (error "except: Symbol not defined" name mod))))
         names)
        (values def-clone modules)))))

(define (prefix-resolver cm mod prefix)
  (let ((prefix-str
         (if (symbol? prefix)
             (symbol->string prefix)
             (error "prefix: prefix must be a symbol" prefix))))
    (call-with-values
        (lambda () (resolve-import mod cm))
      (lambda (defs modules)
        (values
         (map (lambda (def)
                (cons (string->symbol
                       (string-append
                        prefix-str
                        (symbol->string (car def))))
                      (cdr def)))
              defs)
         modules)))))

(define (rename-resolver cm mod . renames)
  (call-with-values
      (lambda () (resolve-import mod cm))
    (lambda (defs modules)
      (let ((def-clone (map (lambda (x) x) defs)))
        (for-each
         (lambda (rename)
           (if (not (and (list? rename)
                         (eq? 2 (length rename))
                         (symbol? (car rename))
                         (symbol? (cadr rename))))
               (error "rename: Invalid rename form" rename))

           (let ((pair (assq (car rename)
                             def-clone)))
             (if pair
                 (if (assq (cadr rename) def-clone)
                     (error "rename: Symbol already in set"
                            (cadr rename))
                     (set-car! pair (cadr rename)))
                 (error "rename: Symbol not found" (car rename)))))
         renames)
        (values def-clone modules)))))

(set! *import-resolvers*
      `((only: . ,only-resolver)
        (except: . ,except-resolver)
        (prefix: . ,prefix-resolver)
        (rename: . ,rename-resolver)))



;;;; ---------- Export resolvers ----------

;; Export resolvers are the export equivalent of import
;; resolvers. They parse forms like (rename: (from to)), (re-export:
;; srfi/1). Similar to import resolvers, they take the current
;; environment and a list as arguments. Similar to import resolvers,
;; they return two values: the symbols to be exported, and modules
;; that need to loaded to use this export. (This is to make it
;; possible to implement re-export:)

(define *export-resolvers* '())

(define (export-helper env name as)
  (if (not (and (symbol? name)
                (symbol? as)))
      (error "Invalid exports declaration" name as))
  (cond
   ((environment-get env name) =>
    (lambda (val)
      (if (eq? 'mac (car val))
          (list as
                'mac
                (cadr val) ;; Macro procedure
                (caddr val)) ;; Macro environment
          (list as
                'def
                (cadr val)))))
   
   (else
    (error "Name can't be exported because it isn't defined"
           name))))

;; TODO This code is very similar to resolve-import, which is bad.
(define (resolve-export val env)
  (with-module-cache
   (lambda ()
     (cond
      ((and (pair? val)
            (keyword? (car val)))
       (let* ((resolver-id (car val))
              (resolver (let ((pair (assq resolver-id
                                          *export-resolvers*)))
                          (and pair (cdr pair)))))
         (if (not resolver)
             (error "Export resolver not found:" resolver-id)
             (apply resolver
                    (cons env (cdr val))))))
      
      ((symbol? val)
       (values (list (export-helper env val val))
               '()))

      (else
       (error "Invalid exports declaration" val))))))

;; TODO This code is pretty much a copy/pase of resolve-imports, which
;; is bad.
(define (resolve-exports vals env)
  (with-module-cache
   (lambda ()
     (let ((defs '())
           (mods '()))
       (for-each (lambda (val)
                   (call-with-values
                       (lambda ()
                         (resolve-export val env))
                     (lambda (def mod)
                       (set! defs (cons def defs))
                       (set! mods (cons mod mods)))))
                 vals)
       (values (flatten1 defs)
               (flatten1 mods))))))

(define (rename-export-resolver env . renames)
  (values (map (lambda (rename)
                 (if (not (and (list? rename)
                               (eq? 2 (length rename))))
                     (error "Invalid exports declaration"
                            rename))
                 (export-helper env
                                (car rename)
                                (cadr rename)))
               renames)
          '()))

(define (re-export-export-resolver env . import-decls)
  (resolve-imports import-decls (environment-module env)))

(set! *export-resolvers*
      `((rename: . ,rename-export-resolver)
        (re-export: . ,re-export-export-resolver)))



;;;; ---------- Module info utilities ----------

(define empty-module-info
  (make-module-info
   '() '() '() '() '() "" "" "" #f builtin-environment))

;; The placement of this code is a little counterintuitive. It is
;; here, because this is the code that actually does the
;; calc-info.
(define-env load-environment
  "module-loadenv#"
  ((import
    (lambda (source env mac-env)
      (let ((pkgs (extract-synclosure-crawler
                   (cdr (expr*:strip-locationinfo source)))))
        (with-module-cache
         (lambda ()
           (call-with-values
               (lambda () (resolve-imports
                           (extract-synclosure-crawler pkgs)))
             (lambda (def mod)
               (*module-loadenv-uses*
                (append (*module-loadenv-uses*)
                        mod))
               (*module-loadenv-imports*
                (append (*module-loadenv-imports*)
                        def))
               (module-add-defs-to-env def env))))))))
   
   (module
    (lambda (code env mac-env)
      (error "Ill-placed module form" code)))
   
   (define
     (lambda (source env mac-env)
       (let* ((code (expr*:value source))
              (src (transform-to-lambda (cdr code)))
              (sym (synclosure-extract-form (car src))))
         (environment-add-def! env
                               sym
                               (gen-symbol
                                  (module-namespace (current-module))
                                  sym))
         (*module-loadenv-symbols*
          (cons (cons sym 'def) (*module-loadenv-symbols*)))
         (void))))
   
   (module#define-macro-register
     (lambda (form env mac-env)
       (let ((src (transform-to-lambda (cdr form))))
         (*module-loadenv-symbols*
          (cons (cons (car src) 'mac)
                (*module-loadenv-symbols*))))
       (void)))
   
   (let
       (lambda (code env mac-env)
         (void)))
   
   (letrec
       (lambda (code env mac-env)
         (void)))
   
   (lambda
       (lambda (code env mac-env)
         (void)))
   
   (export
    (lambda (code env mac-env)
      (*module-loadenv-exports*
       (append (cdr (extract-synclosure-crawler code))
               (or (*module-loadenv-exports*) '())))))
   
   (compile-options
    (nh-macro-transformer
     (lambda (#!key options
                    cc-options
                    ld-options-prelude
                    ld-options
                    force-compile)
       (if options
           (*module-loadenv-options*
            options))
       (if cc-options
           (*module-loadenv-cc-options*
            cc-options))
       (if ld-options-prelude
           (*module-loadenv-ld-options-prelude*
            ld-options-prelude))
       (if ld-options
           (*module-loadenv-ld-options*
            ld-options))
       (if force-compile
           (*module-loadenv-force-compile*
            force-compile))
       (void))))
   
   (c-declare
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))
   
   (c-initialize
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))
   
   (c-define-type
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))
   
   (c-lambda
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))
   
   (c-define
    (lambda args
      (*module-loadenv-force-compile* #t)
      (void)))))

(define-macro (make-loadenv-vars . vars)
  (let ((syms (map (lambda (var)
                     (string->symbol
                      (string-append
                       "*module-loadenv-"
                       (symbol->string (car var))
                       "*")))
                   vars))
        (defaults (map cadr vars)))
    `(begin
       ,@(map (lambda (sym)
                `(define ,sym (make-parameter #f)))
              syms)
       
       (define (with-module-loadenv thunk)
         (parameterize
          ,(map (lambda (sym default)
                  `(,sym ,default))
                syms
                defaults)
          (thunk))))))

(make-loadenv-vars (symbols '())
                   (exports #f)
                   (imports '())
                   (uses '())
                   (options '())
                   (cc-options "")
                   (ld-options-prelude "")
                   (ld-options "")
                   (force-compile #f))

(define (loadenv-symbol-defs symbols env)
  (let ((ns (module-namespace
             (environment-module env))))
    (map (lambda (pair)
           (let ((name (car pair))
                 (type (cdr pair)))
             (if (eq? 'def type)
                 (list name 'def (gen-symbol ns name))
                 (let ((mac (environment-get env name)))
                   (if (or (not mac)
                           (not (eq? 'mac (car mac))))
                       (error "Internal error in loadenv-symbol-defs:"
                              mac))
                   (list name ;; exported name
                         'mac
                         (cadr mac) ;; macro procedure
                         (caddr mac)))))) ;; macro environment
         symbols)))

;; This function makes use of the module-loadenv dynamic environment.
(define (interpret-loadenv-exports env)
  (let* ((exps (*module-loadenv-exports*)))
    (if exps
        (resolve-exports exps env)
        (values
         (loadenv-symbol-defs (*module-loadenv-symbols*)
                              env)
           '()))))

(define calc-mode (make-parameter 'repl)) ;; one of 'repl, 'calc, 'load

(define (module-info-calculate module #!optional filename)
  (with-module-loadenv
   (lambda ()
     (let ((env #f))
       (if filename
           (parameterize
            ((calc-mode 'calc)
             (top-environment (make-top-environment
                               (resolve-one-module module))))
            (expand-macro
             (expr*:strip-locationinfo
              (file-read-as-expr filename)))
            (set! env (top-environment))))

       (call-with-values
           (lambda () (interpret-loadenv-exports env))
         (lambda (exports export-uses)
           ;; TODO Add something to check for duplicate imports and
           ;; exports.
           (make-module-info
            (reverse (*module-loadenv-symbols*))
            exports
            (*module-loadenv-imports*)
            (remove-duplicates ;; TODO This is an n^2 algorithm = sloow
             (append export-uses
                     (*module-loadenv-uses*))
             equal?)
            (*module-loadenv-options*)
            (*module-loadenv-cc-options*)
            (*module-loadenv-ld-options-prelude*)
            (*module-loadenv-ld-options*)
            (*module-loadenv-force-compile*)
            env)))))))


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

;; TODO This function should rename modules with name h[number] to not
;; clash with the hygiene.

(define (namespace-rename-reserved str)
  (cond
   ((or (string->number str)
        (string-contains str #\~)
        (equal? str "module"))
    (string-append str "_"))

   (else
    str)))

(define module-namespace
  (let ((fn
         (make-module-util-function
          (lambda (mod)
            (string-append
             (let ((loader (module-loader mod)))
               (if (eq? loader module-module-loader)
                   "module"
                   (namespace-rename-reserved
                    ((loader-module-name loader) mod))))
             "#")))))
    (lambda (mod)
      ;; This function might be called with #f as argument
      (if mod (fn mod) "~#"))))

(define (module-add-defs-to-env defs #!optional (te (top-environment)))
  (with-module-cache
   (lambda ()
     (for-each
      (lambda (def)
        (if (eq? 'def (cadr def))
            ;; Regular define
            (environment-add-def!
             te
             (car def) ;; The name it's imported as
             (caddr def)) ;; The name it's imported from
            ;; Macro
            (environment-add-mac!
             te
             ;; The name it's imported as
             (car def)
             ;; The macro procedure
             (caddr def)
             ;; The macro's environment
             (cadddr def))))
      defs))))

(define (module-load/deps module)
  (with-module-cache
   (lambda ()
     (for-each module-load/deps
               (module-info-uses
                (module-info module)))
     
     (for-each (lambda (args)
                 (apply load-once args))
               (module-load module)))))


(define (module-import modules #!optional (env (top-environment)))
  (with-module-cache
   (lambda ()
     (call-with-values
         (lambda () (resolve-imports modules))
       (lambda (defs mods)
         (if (or (eq? (calc-mode) 'repl)
                 (> (environment-phase env) 0))
             (for-each module-load/deps mods))
         
         (module-add-defs-to-env defs env))))))

(define module-module
  (let* ((repl-environment #f)
         (fn (make-module-util-function
              (lambda (mod)
                (if (not (environment-module (top-environment)))
                    (set! repl-environment (top-environment)))

                (top-environment (make-top-environment mod))
                (module-load/deps mod)
                
                (let ((info (module-info mod)))
                  (module-add-defs-to-env (module-info-imports info))
                  (module-add-defs-to-env
                   (loadenv-symbol-defs (module-info-symbols info)
                                        (module-info-environment info))))
                (void)))))
    (lambda (mod)
      ;; This function might be called with #f as argument
      (if mod
          (fn mod)
          (begin
            (top-environment repl-environment)
            (void))))))

;;;; ---------- Loader utility functions ----------

(define *load-once-registry* (make-table))

(define (load-once file-with-extension #!optional module)
  (let ((module (and module (resolve-one-module module))))
    (parameterize
     ((top-environment (make-top-environment module))
      (calc-mode 'load))
     (with-module-cache
      (lambda ()
        (let* ((file (path-strip-trailing-directory-separator
                      (path-strip-extension
                       (path-normalize file-with-extension))))
               (scm (string-append file ".scm"))
               (scm-exists? (file-exists? scm))
               (time (table-ref *load-once-registry* file #f)))
          (if (not (equal? time
                           (or (not scm-exists?)
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
                              (with-module-loadenv
                               (lambda ()
                                 (load file)))))))
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
    (calc-mode 'load))
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
       (with-module-loadenv ;; For *module-loadenv-uses*
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

(define module-module-loader
  (make-loader
   ;; load
   (lambda (mod) '())
   ;; calculate-info
   (lambda (mod)
     (make-module-info
      '()
      (cons
       (list 'syntax-rules
             'mac
             (lambda (code env mac-env)
               `(module#sc-macro-transformer
                 (apply module#syntax-rules-proc
                        ',(expr*:cdr code))))
             builtin-environment)
       (map (lambda (x)
              (list x 'def (gen-symbol "module#" x)))
            '(expand-macro
              make-syntactic-closure
              capture-syntactic-environment
              extract-syntactic-closure-list
              identifier?
              identifier=?
              sc-macro-transformer
              rsc-macro-transformer
              nh-macro-transformer
              
              make-loader
              loader-load
              loader-calculate-info
              loader-needs-compile?
              loader-clean!
              loader-compile!
              
              make-module-info
              module-info-symbols
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
              module-load
              module-needs-compile?
              module-compile!
              module-clean!
              module-namespace
              module-load/deps
              module-import
              module-module

              modules-compile!
              modules-clean!
              modules-in-dir
              module-deps
              module-compile/deps!
              module-clean/deps!
              module-generate-export-list
              
              loader
              module-module-loader)))
      '() '() '() "" "" "" #f builtin-environment))
   ;; path-absolutize
   (lambda (path #!optional ref) #f)
   ;; module-name
   (lambda (mod) "module")
   ;; needs-compile?
   (lambda (mod) #f)
   ;; clean!
   (lambda (mod) #f)
   ;; compile!
   (lambda (mod) #f)))



;;;; ---------- Some environment creation stuff ----------

;; This code is here because it depends on the module-info machinery.

(define module-env-table
  (let* ((ns (make-table))
         (env (make-environment #f ns ns)))
    (module-add-defs-to-env
     (module-info-exports
      ((loader-calculate-info module-module-loader) #f))
     env)
    ns))

(define (builtin-ns-fun phase?)
  (let* ((builtin-pair
          (cons (env-ns builtin-environment)
                gambit-builtin-table))
         (inside-letrec-table
          (env-ns inside-letrec-environment))
         (calcing-table
          (cons (env-ns load-environment)
                builtin-pair))
         (inside-letrec-pair
          (cons inside-letrec-table
                builtin-pair)))
    (lambda ()
      (cond
       ((and (not phase?)
             (eq? (calc-mode) 'calc))
        calcing-table)
       
       ((inside-letrec)
        inside-letrec-pair)

       (else
        builtin-pair)))))

(define builtin-ns
  (builtin-ns-fun #f))

(define builtin-ns-phase
  (cons module-env-table (builtin-ns-fun #t)))

(define (make-top-environment module)
  (let ((ns (cons
             (make-table)
             (if module
                 builtin-ns
                 (cons module-env-table
                       builtin-ns)))))
    (make-environment module
                      ns
                      ns)))

(define empty-environment
  (make-top-environment #f))

(define top-environment
  (make-parameter (make-top-environment #f)))



;;;; ---------- Hack for configuration ----------

;; Variables declared here are used all over the place.

;; Configuration directives
(define *compiler-options* '())
(define *compiler-cc-options* "")
(define *compiler-ld-options-prelude* "")
(define *compiler-ld-options* "")

(set! *compiler-options* '(debug))
;;(set! *compiler-cc-options* "-I/usr/local/BerkeleyDB.4.7/include")
;;(set! *compiler-ld-options-prelude* "-L/usr/local/BerkeleyDB.4.7/lib")

(set! *module-resolvers*
      `((here . ,current-module-resolver)
        (module . ,(make-singleton-module-resolver
                    module-module-loader))
        (std . ,(package-module-resolver "~~/lib/modules/std/"))))
