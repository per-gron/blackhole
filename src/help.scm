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
  bh help modules      # Show help on modules
  bh help packages     # Show help on packages
  bh help commands     # Show a list of available commands
  bh help COMMAND      # Show detailed information about a specific
                         command
THE-END
)

(define help-modules #<<THE-END
  Black Hole modules

In Black Hole, modules are the basic building block. A module
corresponds directly to a .scm source code file. To create a new
module, create a .scm file.

Black Hole is designed to add as little extra syntax as possible from
plain R5RS code. A simple R5RS .scm file without any external
dependencies is without modification a valid Black Hole module.

  Importing and exporting names

By default, all defined functions, globals and macros are exported. To
control that, place an export form at the top of the file, that
enumerates the names that should be exported. For instance you could
create a file named "a-module.scm" with the following contents:

  (export a-procedure a-number)
  (define secret-number 1)
  (define (a-procedure var) (+ secret-number var))
  (define a-number 5)

For a module system to be useful, a module has to be able to use code
from other modules. That is done with the (import) form. (import)
takes one or more module identifiers. A module identifier can be
several things, but the most common kind of module identifier is a
symbol. (import a-module) will import the module in the
file "a-module.scm".

These paths are always relative to the directory where the module file
is located (or the working directory if in the REPL).
(import ../dir/module) imports the module that is found at
"../dir/module.scm"

  Interactive development

Black Hole is designed to allow interactive development: With Black
Hole, you can fully leverage the power of the REPL while coding. To be
able to do so efficiently, there are a couple of tools:

The most important one is to use an editor that is capable of running
a REPL and evaluating expression in it. With Emacs, you can evaluate
the expression that is where the cursor is with the command C-c C-e.

For this to work well, you need to be able to control in which scope
that expressions are evaluated. By default, you are in the REPL scope,
and if you define a function with the same name as a function in a
module, that function isn't overwritten, because the REPL scope is
distinct from that module's scope.

To be able to overwrite functions, the (module) special form is
used. To enter the scope of the module mod, type (module mod).
Subsequent definitions will be done in that module's scope. To get
back to the REPL scope, type (module).

It is also possible to choose which modules that you want to have
compiled, and which ones that should be run by the interpreter. For
more info on that, see the compile help page.
THE-END
)

(define help-packages #<<THE-END
  Black Hole packages

Black Hole includes a package system similar to Ruby Gems and such
tools. A Black Hole package is a tarball with the modules of the
package and a pkgfile that contains metadata about the package, like
version number, package author and maintainer.

  Central package list

The central list of packages is currently located at the GitHub
repository https://github.com/pereckerdal/bh-packages

To add a new package, please send an email with the package's tarball
to per dot eckerdal at gmail dot com.

  pkgfile

pkgfiles are s-expressions of the form

  (package (attribute value ...) ...)

The available attributes are
* Version: The package version. A pkgfile must contain the package's
  version. For instance
    (version v0.0.2)
* Maintainer: for instance
    (maintainer "Per Eckerdal <per dot eckerdal at gmail dot com>")
* Author: for instance
    (author "Per Eckerdal <per dot eckerdal at gmail dot com>")
* Homepage: for instance
    (homepage "http://github.com/pereckerdal/srfi")
* Description: A short description of the package. For instance
    (description "A collection of SRFI implementations")
* Keywords: A list of keywords (they should be symbols). For instance
    (keywords srfi util)
* License: A list of licenses (they should be symbols). For instance
    (license mit)
* Source directory: If the source is located in a subdirectory of
  the package, and you want to avoid having to type
  (import (pkg-name src/module)), you can specify it like this
    (source-directory "src")
* Exported modules: A list of the exported modules (they should be
  symbols)

  A quick guide to creating packages

1. Put all the modules that should be part of the package in a
   directory.
2. Create the pkgfile and put it in the package's root directory.
3. Create a tarball out of the directory.
4. Host the tarball somewhere on the 'net.
5. Send an email to per dot eckerdal at gmail dot com with the
   URL to the tarball.

If you have your package on GitHub, you don't need to worry about
creating or hosting the tarball, since GitHub does that automatically:
The tarball for a particular commit is available at
  http://github.com/[username]/[repo name]/tarball/[commit hash/tag]
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
