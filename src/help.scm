(define help-string #<<THE-END
Example usage:
  bh                   # Open up a REPL

  bh exe FILENAME      # Deploy the module in FILENAME to a standalone
                         executable
  bh compile FILENAMES # Compile the modules in FILENAMES to shared
                         libraries usable in development

  bh install PACKAGE   # Install PACKAGE
  bh uninstall PACKAGE # Uninstall PACKAGE
  bh list              # Show a list of installed packages
  bh search            # Show a list of packages available for
                         installation

Further help:
  bh help commands     # Show a list of available commands
  bh help COMMAND      # Show detailed information about a specific
                         command
THE-END
)

(define help-commands #<<THE-END
Compilation commands:
  bh exe             # Creates standalone executables for deployment
  bh compile         # Compiles modules to shared libraries usable for
                       development
  bh clean           # Cleans up the files that 'bh compile' spits out

Package commands:
  bh install         # Installs packages
  bh uninstall       # Uninstalls packages
  bh list            # Displays a list of installed packages
  bh search          # Searches for packages in the remote repository

Module analysis commands:
  bh deps            # Calculates and displays dependencies of modules
  bh exported-names  # Calculates and displays exported names of
                       modules

Other commands:
  bh help            # Displays Black Hole help
  bh [repl]          # Opens up a REPL
THE-END
)

(define help-exe #<<THE-END
THE-END
)

(define help-compile #<<THE-END
THE-END
)

(define help-clean #<<THE-END
THE-END
)


(define help-install #<<THE-END
THE-END
)

(define help-uninstall #<<THE-END
THE-END
)

(define help-list #<<THE-END
THE-END
)

(define help-search #<<THE-END
THE-END
)


(define help-deps #<<THE-END
THE-END
)

(define help-exported-names #<<THE-END
THE-END
)


(define help-repl #<<THE-END
THE-END
)
