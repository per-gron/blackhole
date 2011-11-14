# Black Hole

Black Hole is an R5RS compatible module system for Gambit which allows
you to easily import libraries into your code including macros, which
previously has been tricky on Gambit. Conversely you can also export
your own code and create your own libraries to fully modularise your
development.


# Installation

Black Hole is a layer written on top of Gambit Scheme. Practical use
of Black Hole goes through its command line interface: `bh`. Here's
how to install Black Hole

* Download Black Hole
* Compile Black Hole by running `./compile.sh` from a terminal inside
  the Black Hole source directory.
* Make a symbolic link somewhere in your path that points
  to the newly created `bh` binary in the Black Hole source
  directory.
* You can now run Black Hole with the command `bh`. To see what you
  can do with the `bh` command, run `bh help`.


# Usage

In Black Hole, a module corresponds directly to a `.scm` source code
file. To create a new module, create a `.scm` file.

Black Hole is designed to add as little extra syntax as possible from
plain R5RS code. A simple R5RS .scm file without any external
dependencies is without modification a valid Black Hole module.

## Importing and exporting names

By default, all defined functions, globals and macros are exported. To
control that, place an `export` form at the top of the file, that
enumerates the names that should be exported. For instance you could
create a file named "a-module.scm" with the following contents:

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

These paths are always relative to the directory where the module file
is located (or the working directory if in the REPL).
`(import ../dir/module)` imports the module that is found at
"../dir/module.scm"

## Compiling modules

Modules are compiled using the `bh` command.

* `bh compile test.scm` compiles the module found in "test.scm".
* `bh compile -r test.scm` compiles the module found in
   "test.scm" and all its dependencies.
* `bh exe --output=a.out test.scm` compiles the module
   found in "test.scm" to a standalone executable "a.out"

## Macros

In essence, Black Hole is a macro expander. Black Hole extends Gambit
with hygienic macros. It adds support for the special forms
`define-syntax`, `let-syntax` and `letrec-syntax`. Currently, Black
Hole supports macros through explicit renaming
(`er-macro-transformer`), syntactic closures (`sc-macro-transformer`
and `rsc-macro-transformer`) and R5RS `syntax-rules`.

It also adds a macro expansion function, `expand-macro`. It can be
used to inspect what is happening with the macro expansion:

    > (expand-macro '(let ((a 'hello)) a))
    (let ((1#a 'hello)) 1#a)
    > 

`define-macro` macros are supported, but beware that they might get
special identifier objects instead of just symbols when combined with
hygienic macros. Usage of the other forms of macros is strongly
recommended.

# Development

If you want to contribute to Black Hole, either with small bugfix
patches or bigger things, feel free to do so! For bigger things, it's
good to keep in touch with me at per dot eckerdal at gmail dot com, to
make sure that we're on the same page.

I prefer getting patches through GitHub pull requests.

To actually work on the Black Hole code base, I strongly recommend
using the script `src/deventry.scm` instead of compiling the `bh`
binary over and over again. That file has instructions on how to use
it.

# More information / Contact

Please drop me a line at *per dot eckerdal at gmail dot com* or use
the Gambit mailing list if you have any questions.

The [Black Hole page at the Gambit
wiki](http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Black_Hole)
might also be of interest.
