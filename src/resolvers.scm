;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                 Module, import and export resolvers              ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;


;;;; ---------- Module resolvers ----------

;; A module resolver is a function that takes a list (like (/srfi/1)
;; or (spork /core)), the current loader and the current module path
;; (or #f) and returns a module reference.

;; This is the 'here module resolver function
(define (current-module-resolver loader path relative . ids)
  (map (lambda (id)
         (make-module-reference
          loader
          (if relative
              id
              (loader-path-absolutize loader id path))))
    ids))

(define *module-resolvers* '())

(define (module-resolver-add! name res)
  (set! *module-resolvers*
        (cons (cons name res)
              *module-resolvers*)))

(define (resolve-module name
                        #!optional cm
                        #!key relative)
  (if (module-reference? name)
      (list name)
      (let ((resolver-id
             resolver-args
             (cond
              ((symbol? name)
               (values 'here (list name)))
              ((string? name)
               (values 'lib (list name)))
              ((pair? name)
               (values 'pkg name))
              ((vector? name)
               (let ((lst (vector->list name)))
                 (values (car lst) (cdr lst))))
              (else
               (error "Invalid module identifier:" name)))))
        (let ((resolver (let ((pair (assq resolver-id
                                          *module-resolvers*)))
                          (and pair (cdr pair))))
              (loader (if cm
                          (module-reference-loader cm)
                          (current-loader))))
          (cond
           ((not resolver)
            (error "Module resolver not found:" resolver-id))
           
           (else
            (apply resolver
                   `(,loader
                     ,(if cm
                          (module-reference-path cm)
                          (let ((current-mod (current-module-reference)))
                            (and current-mod
                                 (module-reference-path current-mod))))
                     ,relative
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
;; module-info-exports except that it can't contain 'self-reference)
;; and a list of the modules that this import depends on.

(define *import-resolvers* '())

(define (resolve-import val
                        #!optional cm
                        #!key relative)
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
                 (cons (or cm (current-module-reference))
                       (cdr val))))))
   
   (else
    (let ((mods (resolve-module val cm relative: relative))
          (absolute-mods (resolve-module val cm)))
      (values (apply
               append
               (map (lambda (module-ref)
                      (module-info-exports
                       (loaded-module-info
                        (module-reference-ref module-ref))))
                 absolute-mods))
              mods)))))

(define (resolve-imports vals
                         #!optional cm
                         #!key relative)
  (let ((defs '())
        (mods '()))
    (for-each (lambda (val)
                (let ((def
                       mod
                       (resolve-import val cm relative: relative)))
                  (set! defs (cons def defs))
                  (set! mods (cons mod mods))))
              vals)
    (values (flatten1 defs)
            (remove-duplicates (flatten1 mods)))))

(define (only-resolver cm mod . names)
  (let ((defs
         modules
         (resolve-import mod cm)))
    (values
     (map (lambda (name)
            (or (assq name defs)
                (error "only: Symbol not defined" name mod)))
       names)
     modules)))

(define (except-resolver cm mod . names)
  (let ((defs
         modules
         (resolve-import mod cm)))
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
      (values def-clone modules))))

(define (prefix-resolver cm mod prefix)
  (let ((prefix-str
         (if (symbol? prefix)
             (symbol->string prefix)
             (error "prefix: prefix must be a symbol" prefix)))
        (defs
         modules
         (resolve-import mod cm)))
    (values
     (map (lambda (def)
            (cons (string->symbol
                   (string-append
                    prefix-str
                    (symbol->string (car def))))
                  (cdr def)))
       defs)
     modules)))

(define (rename-resolver cm mod . renames)
  (let ((defs
         modules
         (resolve-import mod cm)))
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
      (values def-clone modules))))

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

(define (macroexpansion-symbol-defs symbols env)
  (let ((ns (module-reference-namespace
             (environment-module-reference env))))
    (map (lambda (pair)
           (let ((name (car pair))
                 (type (cdr pair)))
             (case type
               ((def)
                (list name
                      'def
                      (gen-symbol ns name)
                      'self-reference))

               ((mac)
                (list name
                      'mac
                      name
                      'self-reference))

               (else
                (error "Internal error in macroexpansion-symbol-defs")))))
      symbols)))

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
                name
                'self-reference)
          (list as
                'def
                (cadr val)
                'self-reference))))
   
   (else
    (error "Name can't be exported because it isn't defined"
           name))))

;; TODO This code is very similar to resolve-import, which is bad.
(define (resolve-export val env)
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
    (list (export-helper env val val)))
   
   (else
    (error "Invalid exports declaration" val))))

;; TODO This code is pretty much a copy/paste of resolve-imports, which
;; is bad.
(define (resolve-exports vals env)
  (let ((defs '()))
    (for-each (lambda (val)
                (push! defs (resolve-export val env)))
      vals)
    (flatten1 defs)))

(define (rename-export-resolver env . renames)
  (map (lambda (rename)
         (if (not (and (list? rename)
                       (eq? 2 (length rename))))
             (error "Invalid exports declaration"
                    rename))
         (export-helper env
                        (car rename)
                        (cadr rename)))
    renames))

(define (re-export-export-resolver env . import-decls)
  (let ((def
         mod
         (resolve-imports import-decls
                          (environment-module-reference env)
                          relative: #t)))
    def))

(set! *export-resolvers*
      `((rename: . ,rename-export-resolver)
        (re-export: . ,re-export-export-resolver)))
