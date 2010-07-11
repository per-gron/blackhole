;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Remote packages                            ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; Inspired by CatDancers lib library for Arc
;; http://catdancer.github.com/lib.html

(define (module-path-absolute? str)
  (string-begins-with str "http://"))

(define (module-path-absolutize p ref)
  (if (module-path-absolute? p)
      p
      (remove-dot-segments
       (string-append ref
                      "/../"
                      p))))

(define lib-files-dir
  "~~/lib/modules/work/lib")

(define lib-get-shell-command
  "wget -nv -x -N")

(define lib-cache-dir
  "~~/lib/modules/work/lib-cache")

(define (generate-lib-cache-dir)
  (let loop ((i 0))
    (let ((path (path-expand (number->string i)
                             lib-cache-dir)))
      (if (file-exists? path)
          (loop (+ 1 i))
          path))))

;; This function does not support
(define (fetch-files urls)
  (let ((cache-dir (generate-lib-cache-dir)))
    (dynamic-wind
        (lambda ()
          (create-dir-unless-exists cache-dir))
        (lambda ()
          (parameterize
           ((current-directory cache-dir))
           (or (eq? 0
                    (shell-command
                     (string-append
                      lib-get-shell-command
                      (apply
                       string-append
                       (apply append
                              (map (lambda (url)
                                     (list " " url))
                                   urls)))
                      " 2>/dev/null")))
               (error "Failed to fetch urls" urls)))

          (for-each (lambda (fn)
                      (let ((target (path-expand fn lib-files-dir)))
                        (create-dir-unless-exists
                         (path-directory target))
                        
                        (rename-file (path-expand fn cache-dir)
                                     (path-expand fn lib-files-dir))))
                    (find-files-with-ext "" cache-dir)))
        (lambda ()
          (recursively-delete-file cache-dir)))))

(define (fetch-file url)
  (fetch-files (list url)))

(define (lib-url-path? url)
  (and (string-begins-with url "http://")
       (string-ends-with url ".scm")))

(define (lib-url-path url)
  (if (lib-url-path? url)
      (string-remove-prefix url "http://")
      (error "Not a valid lib URL" url)))

(define (lib-url-full-path url)
  (path-expand (lib-url-path url)
               lib-files-dir))

(define (lib-list)
  (map (lambda (path)
         (string-append "http://" path))
       (find-files-with-ext "" lib-files-dir)))


(define (lib-module-resolver _ __ . urls)
  (map (lambda (url)
         (lib-url-path url) ;; Will throw error if invalid URL
         (make-module-reference lib-loader url))
       urls))

(define lib-loader
  (make-loader
   name:
   'lib

   path-absolute?:
   (lambda (path)
     (error "TODO To be implemented"))
   
   path-absolutize:
   (lambda (path #!optional ref)
     (if (not ref)
         (error "lib-loader's path-absolutize \
                 function requires ref argument"))
     (module-path-absolutize
      (string-append (symbol->string path) ".scm")
      (path-directory ref)))

   ;; absolute-file
   ;;(lambda (mod)
   ;;  (let* ((url (module-reference-path mod))
   ;;         (fn (lib-url-full-path url)))
   ;;    (if (not (file-exists? fn))
   ;;        (fetch-file url))
   ;;    (path-normalize fn)))

   load-module:
   (lambda (path)
     (error "TODO To be implemented"))

   compare-stamp:
   (lambda (path stamp)
     (error "TODO To be implemented"))

   module-name:
   (lambda (path)
     (path-strip-directory
      (path-strip-extension path)))))
