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
   ))

(in-package #:chenyi)
