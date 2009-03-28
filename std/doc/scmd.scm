(use /misc/splice
     /srfi/1
     read
     tags
     (build))

(private)

(define-syntax get-tags-include
  (sc-macro-transformer
   (lambda (form env)
     `',(build#module-include 'tags))))

(define tags-include (get-tags-include))

(define-syntax get-tags-module
  (sc-macro-transformer
   (lambda (form env)
     `',(build#resolve-one-module 'tags))))

(define tags-module (get-tags-module))

(define (path->module path)
  (let ((pdir (package-search-directory
               (path-directory path))))
    (make-module (package pdir)
                 (string->symbol
                  (string-append
                   "/"
                   (path-strip-extension
                    (path-normalize path #t pdir)))))))

(define (module-module-nice module)
  (with-exception-catcher
   (lambda (e)
     `(begin
        (##namespace (,(module-namespace module)))
        (##include "~~/lib/gambit#.scm")))
   (lambda ()
     (module-module module))))

(/private)

(define (scmd-parse file)
  (with-input-from-file file read-scmd))

(define (scmd-eval form file)
  (let ((module (path->module file)))
    (parameterize
     ((build#top-environment
       (build#make-top-environment module)))
     (let* ((cte (##make-top-cte))
            (e (lambda (form)
                 (##eval-top form cte))))
       (pp (module-module-nice module))
       (if module
           (e (module-module-nice module)))
       (e `(use ,tags-module))
       (for-each (lambda (x)
                   (pp x)
                   (pp (expand-macro x))
                   (e x)
                   (pp "test"))
                 form)))))

(define (scmd-load file)
  (let ((path
         (path-expand file)))
    (parameterize
     ((current-directory
       (path-directory path)))
     (scmd-block
      (filter
       (lambda (x) (not (eq? x #!void)))
       (scmd-eval (scmd-parse path) path))))))

(define (scmd-include file)
  (apply splice (scmd-load file)))
