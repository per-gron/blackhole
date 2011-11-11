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
Usage: bh exe [--output/-o] [--quiet/-q] [--verbose/-v] MODULE-FILE

Compiles a module and its dependencies into a standalone executable.
The executable does not contain any Black Hole code or module
metadata, so eval and related functions might not work as expected.

Creating a standalone executable involves compiling all modules again,
even if the modules are compiled already. This is necessary because we
want to get an executable that doesn't contain module metadata.

Because of this, creating a standalone executable with this method is
often quite slow, and is therefore only recommended for deployment.
For development 'bh compile', or even running the code in interpreted
mode is preferable.

By default, the generated executable is stored in a file with the same
name as the module file, but with the file extension stripped.
To override this, use the --output option.

Example usage:
  bh exe test.scm
  bh exe -q -o a.out test.scm

Options:
  --output/-o     Specify the file where the resulting executable is
                  stored
  --quiet/-q      Don't print anything unless necessary
  --verbose/-v    Verbose output
THE-END
)

(define help-compile #<<THE-END
Usage: bh compile [--recursive/-r] [--bunch/-b] [--continue/-k]
                  [--quiet/-q] [--verbose/-v] [--force/-f]
                  MODULE-FILE ...

Compiles one or more module files to .o[n] and .o[n].deps files. The
first file is a shared library that contains the actual code of the
module. The .deps files contain references to the .o[n] files that
need to be loaded before this .o[n] file can be loaded.

This method of compilation is suitable for development, because the
shared libraries contain not only the module's code, but also its
macros and other metadata (dependencies, exported names etc).

The recommended way of using this command while coding is to compile
performance sensitive modules and modules that aren't currently being
worked on, while keeping the modules that are currently worked on
uncompiled. If that is done, much of the performance of compiled code
can be achieved while still getting the debuggability and other
niceties of interpreted code.

If the --bunch option is specified, more than one module can be
compiled into one single shared library. Bunches lose some of the
interactive development features (modules are for instance not
recompiled automatically when imported), so bunches recommended only
for compiling libraries that aren't actively being worked on. This
method of compilation is used behind the scenes when installing
packages.

Bunch compilation leaves .ol files that specify the path of the bunch
file.

Example usage:
  bh compile *.scm
  bh compile -r test.scm
  bh compile --bunch=test.ob -r test.scm

Options:
  --recursive/-r  Also compile the dependencies of the specified
                  modules, and their dependencies, and so on
  --bunch/-b      Compile all of the specified modules into a bunch
                  file, specified as an argument to the bunch option.
  --continue/-c   Continue compilation even if an error is
                  encountered.
  --quiet/-q      Don't print anything unless necessary
  --verbose/-v    Verbose output
  --force/-f      Compile modules even if they don't need to be
                  compiled.
THE-END
)

(define help-clean #<<THE-END
Usage: bh clean [--recursive/-r] [--quiet/-q] MODULE-FILE ...

Removes any .o[n] and .o[n].deps files belonging to the specified
modules. Does not clean bunch files or bunch link files (.ol files)

Example usage:
  bh clean *.scm
  bh clean -rq test.scm

Options:
  --recursive/-r  Also clean the dependencies of the specified
                  modules, and their dependencies, and so on
  --quiet/-q      Don't print anything

THE-END
)


(define help-install #<<THE-END
Usage: bh install [--quiet/-q] [--pretend/-p] [--verbose/-v]
                  [--compile/-c] [--ignore-dependencies/-D]
                  [--version=VERSION] PACKAGE-NAME ...

Installs packages.

Example usage:
  bh install sack
  bh install --pretend uuid
  bh install --version=v0.0.4 std
  bh install --compile=no srfi

Options:
  --quiet/-q      Don't print anything unless necessary
  --pretend/-p    Show the actions that would take place, but don't
                  actually perform them
  --verbose/-v    Verbose output
  --compile/-c    Control whether the package should be compiled on
                  installation or not.
  --ignore-dependencies/-D   Do not install packages that the
                             specified packages depend on. (Use at
                             your own risk)
  --version       Specify a particular version of the package to be
                  installed
THE-END
)

(define help-uninstall #<<THE-END
Usage: bh uninstall [--quiet/-q] [--pretend/-p] [--verbose/-v]
                    [--force/-f] [--ignore-dependencies/-D]
                    [--version=VERSION] PACKAGE-NAME ...

Uninstalls packages. By default it uninstalls the latest version of
the specified package. Unless otherwise specified, the command will
fail with an error if there are any other packages installed that
depend on the specified packages.

Example usage:
  bh uninstall --force srfi
  bh uninstall --pretend uuid
  bh uninstall --version=v0.0.4 std

Options:
  --quiet/-q      Don't print anything unless necessary
  --pretend/-p    Show the actions that would take place, but don't
                  actually perform them
  --verbose/-v    Verbose output
  --force/-f      Uninstall any packages that depend on the specified
                  packages
  --ignore-dependencies/-D   Uninstall the specified packages without
                             uninstalling any packages that depend on
                             them. (Use at your own risk)
  --version       Specify a particular version of the package to be
                  uninstalled
THE-END
)

(define help-list #<<THE-END
Usage: bh list [--quiet/-q]

Prints a list of all currently installed packages.

Example usage:
  bh list

Options:
  --quiet/-q      Print only the packages, one per line
THE-END
)

(define help-search #<<THE-END
Usage: bh search

This command is in a very early stage of development. Currently, the
only thing it does is to print a list of all available packages in the
remote package list.

Example usage:
  bh search

Options: There are currently no options to this command.
THE-END
)


(define help-deps #<<THE-END
Usage: bh deps [--quiet/-q] [--recursive/-r] FILENAME ...

Prints a list of the modules that one or more modules depend on.

Example usage:
  bh deps test.scm
  bh deps -rq test.scm

Options:
  --recursive/-r  Do a recursive search of the module's dependencies,
                  and print all dependencies found
  --quiet/-q      Print only the modules, one per line
THE-END
)

(define help-exported-names #<<THE-END
Usage: bh exported-names [--quiet/-q] FILENAME ...

Prints a list of exported names from a module. This command is
useful when adding an (export) form to a module; this command
creates the list that Black Hole implicitly creates for you, which
is useful as a starting point.

Example usage:
  bh exported-names test.scm

Options:
  --quiet/-q      Print only the names, one per line
THE-END
)


(define help-repl #<<THE-END
Usage: bh [repl] [--version/-v] [-e expr/--eval=expr] [--quiet/-q]

Opens up a REPL for interactive development.

This is the default command, so it is not necessary to type 'bh repl';
'bh' is sufficient.

Example usage:
  bh
  bh repl -q -e "(println \"Hello\")"

Options:
  --version/-v    Display the version and quit
  --eval/-e       Evaluate the given expression before opening the
                  REPL.
  --quiet/-q      Don't print the greeting message
THE-END
)
