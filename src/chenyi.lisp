;;;; chenyi.lisp
(in-package #:cl-user)

(defpackage #:chenyi
  (:use #:cl)
  (:nicknames #:cy)
  (:import-from #:alexandria
                #:define-constant)
  (:export
   ;;; Constants
   #:+e+ #:+pi+ #:π #:+euler+ #:+eulergamma+ #:γ
   #:+catalan+ #:+golden+ #:φ
   #:+log2e+ #:+log10e+
   #:+sqrt-1/2+ #:+sqrt-2+ #:+sqrt-3+ #:+sqrt-pi+
   #:+pi/2+ #:+pi/4+ #:+1/pi+ #:+2/pi+
   #:+ln2+ #:+ln10+ #:+ln-pi+
   ;;; Infinities and Not-a-number
   #:float-infinity-p #:inf32 #:inf64 #:-inf32 #:-inf64 #:inf
   #:nan-p #:nan32 #:nan64 #:nan
   ;;; Elementary functions
   #:log1p/f64 #:log1p #:expm1/f64 #:expm1 
   #+abcl #:frexp/f32 #:frexp/f64 #:ldexp/f64
   #:*f64cmp-epsilon* #:f64cmp
   #:f64cmp< #:f64cmp<= #:f64cmp= #:f64cmp> #:f64cmp>=
   #:hypot/f64 #:hypot #:hypot3/f64 #:hypot3
   ))

(in-package #:chenyi)
