;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                       Remote packages                            ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

;; Inspired by CatDancers lib library for Arc
;; http://catdancer.github.com/lib.html

(define (module-path-absolute? str)
  (string-begins-with str "http://"))

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
         (make-module lib-loader url))
       urls))

(define lib-loader
  (make-loader
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
   ;;  (let* ((url (module-path mod))
   ;;         (fn (lib-url-full-path url)))
   ;;    (if (not (file-exists? fn))
   ;;        (fetch-file url))
   ;;    (path-normalize fn)))

   load-module:
   (lambda (path) TODO)

   module-name:
   (lambda (path)
     (path-strip-directory
      (path-strip-extension path)))))
