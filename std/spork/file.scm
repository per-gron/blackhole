(import core
        ../util/u8v)

(export file-spork-mimes
        file-spork)

;; I have no idea whether this works on non-Unix environments.
;; I don't care right now.
(define (is-directory? dir)
  (file-exists? (string-append dir "/")))

(define (extract-extension fn)
  (let ((len (string-length fn)))
    (let loop ((i 0))
      (cond
       ((>= i len)
        fn)
       
       ((eq? (string-ref fn i) #\.)
        (substring fn i len))
       
       (else
        (loop (+ 1 i)))))))

;; Root is assumed to be normalized, f might be but doesn't have to.
(define (allowed-file? f root)
  (let ((len (string-length root))
        (fn (path-normalize f #f root)))
    (and (>= (string-length fn) len)
         (equal? root
                 (substring fn 0 len))
         fn)))

(define (read-file f root mimes)
  (let ((fn (allowed-file? f root)))
    (if (not fn) (show-error 400))
    (if (not (file-exists? fn)) (show-error 404))
    (if (is-directory? fn) (show-error 403))
    
    (with-input-from-file fn
      (lambda ()
        (spork-reply
         (lambda ()
           (dump-u8vector-port-to-other-u8vector-port
            (current-input-port)
            (current-output-port)))
         type: (or (table-ref mimes (extract-extension fn) #f)
                   (table-ref mimes "default")))))))

(define file-spork-mimes
  (list->table
   '((".avi"     . "video/x-msvideo")
     (".bz2"     . "application/x-bzip")
     (".class"   . "application/octet-stream")
     (".css"     . "text/css")
     (".dtd"     . "text/xml")
     (".dvi"     . "application/x-dvi")
     (".gif"     . "image/gif")
     (".gz"      . "application/x-gzip")
     (".htm"     . "text/html")
     (".html"    . "text/html")
     (".jpeg"    . "image/jpeg")
     (".jpg"     . "image/jpeg")
     (".js"      . "text/javascript")
     (".m3u"     . "audio/x-mpegurl")
     (".mov"     . "video/quicktime")
     (".mp3"     . "audio/mpeg")
     (".mpeg"    . "video/mpeg")
     (".mpg"     . "video/mpeg")
     (".ogg"     . "application/ogg")
     (".pdf"     . "application/pdf")
     (".png"     . "image/png")
     (".ps"      . "application/postscript")
     (".qt"      . "video/quicktime")
     (".sig"     . "application/pgp-signature")
     (".swf"     . "application/x-shockwave-flash")
     (".tar"     . "application/x-tar")
     (".tar.bz2" . "application/x-bzip-compressed-tar")
     (".tar.gz"  . "application/x-tgz")
     (".tbz"     . "application/x-bzip-compressed-tar")
     (".tgz"     . "application/x-tgz")
     (".torrent" . "application/x-bittorrent")
     (".txt"     . "text/plain")
     (".wav"     . "audio/x-wav")
     (".wax"     . "audio/x-ms-wax")
     (".wma"     . "audio/x-ms-wma")
     (".wmv"     . "video/x-ms-wmv")
     (".xml"     . "text/xml")
     (".zip"     . "application/zip")
     ("default"  . "text/plain"))))

(define (file-spork root #!key (path "") (mimes file-spork-mimes))
  (let ((root (path-normalize root)))
    (make-spork
     (list path 'filename)
     (lambda (filename)
       (read-file filename root mimes)))))
