;; Module system configuration

(set! *compiler-options* '(debug))
;;(set! *compiler-cc-options* "-I/usr/local/BerkeleyDB.4.7/include")
;;(set! *compiler-ld-options-prelude* "-L/usr/local/BerkeleyDB.4.7/lib")

(set! package-resolvers
      `((here . ,current-package-resolver)
        ;;(example . ,(make-package-resolver
        ;;             (package (string-append *app-path* "/src/example"))))
        (build . ,(make-singleton-package-resolver
                   build-package))
        ;;(termite . ,(make-singleton-package-resolver
        ;;             termite-package))
        ;;(ssax-sxml . ,(make-singleton-package-resolver
        ;;               ssax-sxml-package))
        ))
