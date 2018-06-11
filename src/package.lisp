;;;; packages.lisp
(in-package #:cl-user)

(defpackage #:chenyi
  (:use #:cl)
  (:nicknames #:cy)
  (:import-from #:alexandria #:define-constant)
  ;; (:shadow #:acosh #:asinh #:atanh)
  (:export
   ;;; Utilities
   #:eps #:inf #:nan
   ;;; Conditions
   #:domain-error #:domain-error-expected
   ;;; Types
   #:float32 #:float64 #:complex/rational #:complex/f32 #:complex/f64
   #:float32-p #:float64-p #:complex/rational-p #:complex/f32-p #:complex/f64-p
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
   #:+e+ #:+pi+ #:+euler+ #:+eulergamma+
   #:+catalan+ #:+golden+
   #:+log2e+ #:+log10e+
   #:+sqrt-1/2+ #:+sqrt-2+ #:+sqrt-3+ #:+sqrt-pi+
   #:+pi/2+ #:+pi/4+ #:+1/pi+ #:+2/pi+
   #:+ln2+ #:+ln10+ #:+ln-pi+
   ;;; Infinities and Not-a-number
   #:infinity-p #:finity-p #:inf32 #:inf64 #:-inf32 #:-inf64 #:inf #:-inf
   #:nan-p #:nan32 #:nan64 #:nan
   ;;; Elementary functions
   #:expm1 #:log1p #:frexp #:ldexp
   #:*fcmp-epsilon* #:fcmp
   #:fcmp< #:fcmp<= #:fcmp= #:fcmp> #:fcmp>=
   ;; #:acosh #:asinh #:atanh
   #:hypot #:hypot3
   ;;; Special Functions
   #:factorial #:lfact #:gamma #:lgamma #:beta)
  (:documentation "This package contains basic definitions, including constants, elementary functions and utilities."))

(eval-when (:load-toplevel)
  (let ((type (lisp-implementation-type))
        (version (lisp-implementation-version)))
    #-(or abcl allegro ccl ecl lispworks sbcl)
    (error "Your lisp ~a-~a is not supported yet." type version)
    #+(and ecl (or windows darwin))
    (warn "Your lisp ~a-~a doesn't support the not-a-number value of type single-float (NaN32) natively."
          type version )))
