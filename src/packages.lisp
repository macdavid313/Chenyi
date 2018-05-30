;;;; packages.lisp
(in-package #:cl-user)

(defpackage #:chenyi.sys
  (:use #:cl)
  (:nicknames #:cy.sys #:chenyi/sys #:cy/sys)
  (:import-from #:alexandria #:define-constant)
  (:export
   ;;; Utilities
   #:ensure-double-float
   ;;; Conditions
   #:domain-error #:domain-error-expected
   ;;; Types
   #:negative-fixnum #:nonostive-fixnum #:non-negative-fixnum #:positive-fixnum
   #:negative-integer #:nonostive-integer #:non-negative-integer #:positive-integer
   #:negative-rational #:nonostive-rational #:non-negative-rational #:positive-rational
   #:negative-ratio #:nonostive-ratio #:non-negative-ratio #:positive-ratio
   #:negative-real #:nonostive-real #:non-negative-real #:positive-real
   #:negative-float #:nonostive-float #:non-negative-float #:positive-float
   #:negative-short-float #:nonostive-short-float #:non-negative-short-float #:positive-short-float
   #:negative-single-float #:nonostive-single-float #:non-negative-single-float #:positive-single-float
   #:negative-double-float #:nonostive-double-float #:non-negative-double-float #:positive-double-float
   #:negative-long-float #:nonostive-long-float #:non-negative-long-float #:positive-long-float
   #:negative-fixnum-p #:non-postive-fixnum-p #:non-negative-fixnum-p #:positive-fixnum-p
   #:negative-integer-p #:non-postive-integer-p #:non-negative-integer-p #:positive-integer-p
   #:negative-rational-p #:non-postive-rational-p #:non-negative-rational-p #:positive-rational-p
   #:negative-ratio-p #:non-postive-ratio-p #:non-negative-ratio-p #:positive-ratio-p
   #:negative-real-p #:non-postive-real-p #:non-negative-real-p #:positive-real-p
   #:negative-float-p #:non-postive-float-p #:non-negative-float-p #:positive-float-p
   #:negative-short-float-p #:non-postive-short-float-p #:non-negative-short-float-p #:positive-short-float-p
   #:negative-single-float-p #:non-postive-single-float-p #:non-negative-single-float-p #:positive-single-float-p
   #:negative-double-float-p #:non-postive-double-float-p #:non-negative-double-float-p #:positive-double-float-p
   #:negative-long-float-p #:non-postive-long-float-p #:non-negative-long-float-p #:positive-long-float-p
   ;;; Constants
   #:+e+ #:+pi+ #:π #:+euler+ #:+eulergamma+ #:γ
   #:+catalan+ #:+golden+ #:φ
   #:+log2e+ #:+log10e+
   #:+sqrt-1/2+ #:+sqrt-2+ #:+sqrt-3+ #:+sqrt-pi+
   #:+pi/2+ #:+pi/4+ #:+1/pi+ #:+2/pi+
   #:+ln2+ #:+ln10+ #:+ln-pi+
   ;;; Infinities and Not-a-number
   #:infinity-p #:finity-p #:inf32 #:inf64 #:-inf32 #:-inf64 #:inf #:-inf
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
  (:import-from #:chenyi.sys
                #:+pi+
                #:domain-error #:domain-error-expected
                #:inf #:-inf #:inf64 #:-inf64 #:inf32 #:-inf32
                #:nan #:nan64 #:nan32
                #:infinity-p #:finity-p #:nan-p)
  #-abcl (:import-from #:cffi #:foreign-funcall)
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
