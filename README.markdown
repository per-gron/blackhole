# Black Hole

Black Hole is an R5RS compatible module system for Gambit which allows
you to easily import libraries into your code including macros, which
previously has been tricky on Gambit. Conversely you can also export
your own code and create your own libraries to fully modularise your
development.


# Installation

Black Hole installs itself in the Gambit interpreter and compiler as a
macro expander. The installation of Black Hole basically consists of
downloading it, compiling it, and making sure it is `load`ed at the
proper times. Compilation isn't necessary, and it can be loaded
manually from the REPL by `load`ing "build.scm". This approach is
merely for convenience:

* Download Black Hole
* Compile Black Hole by running `gsc build` from a terminal inside
  the Black Hole source directory.
* Make a symbolic link called `bsc` that points to `gsc` and put it
  in your `PATH`.
* Put this in your ~/.gambcini file, changing `blackhole-path` to
  point to where Black Hole is installed:

        (let ((blackhole-path "~~lib/modules"))
          (and (equal? (path-strip-directory (car (command-line))) "bsc")
               (load (path-expand "build" blackhole-path))
               (begin 
                 (set! module#ns-file (path-expand "ns.dat" blackhole-path))
                 (println "Loaded Black Hole."))))

* You can now run Black Hole with the command `bsc`


# Usage

In Black Hole, a module corresponds directly to a .scm source code
file. To create a new module, create a .scm file.

Black Hole is designed to add as little extra syntax as possible from
plain R5RS code. A simple R5RS .scm file without any external
dependencies is without modification a valid Black Hole module.

## Importing and exporting names

By default, all defined functions, globals and macros are exported. To
control that, place an `export` form at the top of the file, that
enumerates the name that should be exported. For instance you could
create a file called a-module.scm with the following contents:

    (export a-procedure a-number)
    
    (define secret-number 1)
    (define (a-procedure var) (+ secret-number var))
    (define a-number 5)

For a module system to be useful, a module has to be able to use code
from other modules. That is done with the `import` form. `import`
takes one or more *module identifiers*. A module identifier can be
several things, but the most common kind of module identifier is a
symbol. `(import a-module)` will import the module in the file
"a-module.scm".

These paths are always relative to the directory of the module (or the
working directory if in the REPL). `(import ../dir/module)` imports
the module that is found at "../dir/module.scm"

## Compiling modules

Modules are not compiled like plain Gambit with the `gsc` command;
They are compiled from the REPL:

* `(module-compile! 'test)` compiles the module found in "test.scm".
* `(module-compile/deps! 'test)` compiles the module found in
   "test.scm" and all its dependencies.
* `(module-compile-to-standalone "a.out" 'test) compiles the module
   found in "test.scm" to a standalone executable "a.out"

## Macros

In essence, Black Hole is a macro expander. Black Hole extends Gambit
with hygienic macros. It adds support for the special forms
`define-syntax`, `let-syntax`, `letrec-syntax`. Currently, Black Hole
supports macros through explicit renaming (`er-macro-transformer`),
syntactic closures (`sc-macro-transformer` and
`rsc-macro-transformer`) and R5RS `syntax-rules`.

It also adds a macro expansion function, `expand-macro`. It can be
used to inspect what is happening with the macro expansion:

    > (expand-macro '(let ((a 'hello)) a))
    (let ((1#a 'hello)) 1#a)
    > 

`define-macro` macros are supported, but beware that they might get
special identifier objects instead of just symbols when combined with
hygienic macros. Usage of the other forms of macros is strongly
recommended.


# Known issues

The syntactic tower is only partially implemented. This can lead to
confusion about what code is executed on compile-time versus
run-time. If you don't use macros, or only use `syntax-rules` macros,
these issues will never arise.


# More information / Contact

Please drop me a line at per.eckerdal@gmail.com or use the Gambit
mailing list if you have any questions.

The blackhole page at the Gambit wiki might also be of interest,
http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Black_Hole
