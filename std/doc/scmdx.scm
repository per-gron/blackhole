(use /string/sxml-to-xml
     scmd)

(define scmd-css #<<EOS

html {
font-family: sans-serif;
}

EOS
)

(define (scmdx-load file)
  `(scmdx
    ,@(scmd-load file)))

(define (scmdx->html x)
  `(html
    (head
     (style
         (@ (type "text/css"))
       ,scmd-css))
    (body
     ,@(cdr x))))

(define (scmdx->html-string x)
  (sxml>>xhtml-fast
   (scmdx->html x)))

(define (scmdx-file->html-string file)
  (scmdx->html-string (scmdx-load file)))

(define (t)
  (with-output-to-file "~/tmp.html"
    (lambda ()
      (scmdx-file->html-string "../../../doc/test2.scmd"))))
