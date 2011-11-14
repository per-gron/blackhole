;; This is a script that I use when I am working on the Black Hole
;; code base: Compiling the bh executable takes *loong* time, plus
;; it's much nicer to debug interpreted code. This is a way to open up
;; a Black Hole REPL with the Black Hole code interpreted.
;;
;; It is also possible to use this script to run Black Hole in
;; compiled mode, simply by compiling this file with `gsc deventry`
;;
;; To actually use it:
;;
;; 1. Put a symlink named bsc in your path that points to the gsc
;; binary
;;
;; 2. Set the BLACKHOLE_PATH environment variable to where your Black
;; Hole directory is.
;;
;; 3. Add the following code to ~/.gambcini:
;;  
;; (let ((blackhole-path (getenv "BLACKHOLE_PATH")))
;;   (and (equal? (path-strip-directory (car (command-line))) "bsc")
;;        (load (path-expand "src/deventry" blackhole-path))
;;        (begin
;;          (println "Loaded Black Hole."))))
;;
;; 4. Fire up the Black Hole REPL with the terminal command `bsc`

(##include "blackhole.scm")
(apply-hooks!)
