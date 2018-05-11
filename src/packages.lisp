;;;; packages.lisp
(in-package #:cl-user)

(defpackage #:chenyi.sys
  (:use #:cl)
  (:nicknames #:cy.sys #:chenyi/sys #:cy/sys)
  (:import-from #:alexandria #:define-constant)
  (:export
   ;;; Utilities
   #:ensure-double-float
   ;;; Constants
   #:+e+ #:+pi+ #:π #:+euler+ #:+eulergamma+ #:γ
   #:+catalan+ #:+golden+ #:φ
   #:+log2e+ #:+log10e+
   #:+sqrt-1/2+ #:+sqrt-2+ #:+sqrt-3+ #:+sqrt-pi+
   #:+pi/2+ #:+pi/4+ #:+1/pi+ #:+2/pi+
   #:+ln2+ #:+ln10+ #:+ln-pi+
   ;;; Infinities and Not-a-number
   #:infinity-p #:inf32 #:inf64 #:-inf32 #:-inf64 #:inf
   #:nan-p #:nan32 #:nan64 #:nan
   ;;; Elementary functions
   #:log1p/f64 #:log1p #:expm1/f64 #:expm1 
   #+abcl #:frexp/f32 #:frexp/f64 #:ldexp/f64
   #:*f64cmp-epsilon* #:f64cmp
   #:f64cmp< #:f64cmp<= #:f64cmp= #:f64cmp> #:f64cmp>=
   #:hypot/f64 #:hypot #:hypot3/f64 #:hypot3)
  (:documentation "This package contains basic definitions, including constants, elementary functions and utilities."))

(defpackage #:chenyi.special
  (:use #:cl)
  (:nicknames #:cy.special #:chenyi/special #:cy/special)
  (:export #:gamma #:beta)
  (:documentation "This package contains various implementations for special mathematical functions, e.g. Gamma function, Beta funciton."))

#-abcl
(defpackage #:chenyi.rng.dsfmt
  (:use #:cl)
  (:nicknames #:cy.rng.dsfmt #:chenyi/rng/dsfmt #:cy/rng/dsfmt)
  (:import-from #:cffi
                #:*foreign-library-directories*
                #:define-foreign-library
                #:foreign-library-loaded-p
                #:use-foreign-library
                #:defcvar
                #:defcfun)
  (:import-from #:trivial-download #:download)
  (:import-from #:trivial-extract #:extract-zip)
  (:export #:+DSFMT-MEXP+ #:+DSFMT-N+ #:+DSFMT-N32+ #:+DSFMT-N64+
           #:dsfmt-get-global-data
           #:dsfmt-get-min-array-size
           #:dsfmt-get-idstring
           #:dsfmt-gen-rand-all
           #:dsfmt-fill-array-close1-open2
           #:dsfmt-fill-array-open-close
           #:dsfmt-fill-array-close-open
           #:dsfmt-fill-array-open-open
           #:dsfmt-chk-init-gen-rand
           #:dsfmt-chk-init-by-array
           #:dsfmt-genrand-uint32
           #:dsfmt-genrand-close1-open2
           #:dsfmt-genrand-close-open
           #:dsfmt-genrand-open-close
           #:dsfmt-genrand-open-open
           #:dsfmt-gv-genrand-uint32
           #:dsfmt-gv-genrand-close1-open2
           #:dsfmt-gv-genrand-close-open
           #:dsfmt-gv-genrand-open-close
           #:dsfmt-gv-genrand-open-open
           #:dsfmt-gv-fill-array-open-close
           #:dsfmt-gv-fill-array-close-open
           #:dsfmt-gv-fill-array-open-open
           #:dsfmt-gv-fill-array-close1-open2
           #:dsfmt-gv-init-gen-rand
           #:dsfmt-gv-init-by-array
           #:dsfmt-init-gen-rand
           #:dsfmt-init-by-array)
  (:documentation "This package contains the binding of dSFMT."))

(defpackage #:chenyi.rng
  (:use #:cl)
  (:nicknames #:cy.rng #:chenyi/rng #:cy/rng)
  #-abcl (:documentation "This package contains various implementations for the Random Number Generator, including MersenneTwister by using dSFMT.")
  #+abcl (:documentation "This package contains various implementations for the Random Number Generator."))

(defpackage #:chenyi
  (:use #:cl)
  (:nicknames #:cy)
  (:import-from #:cl-reexport #:reexport-from)
  (:documentation "The main package for Chenyi."))
