;;;; chenyi.lisp
(in-package #:cl-user)
(defpackage #:chenyi-test
  (:use #:cl #:chenyi #:prove))
(in-package #:chenyi-test)

;;; actual testing code start from here ...
(plan nil)

(subtest "CONSTANTS"
  ;;; TYPE
  (dolist (val #-ecl (list inf32 -inf32 nan32)
               #+ecl (list inf32 -inf32))
    (is-type val 'single-float))
  (dolist (val (list +e+ +pi+ π +euler+ +eulergamma+ γ
                     +catalan+ +golden+ φ
                     +log2e+ +log10e+
                     +sqrt-1/2+ +sqrt-2+ +sqrt-3+ +sqrt-pi+
                     +pi/2+ +pi/4+ +1/pi+ +2/pi+
                     +ln2+ +ln10+ +ln-pi+ inf64 -inf64 inf))
    (is-type val 'double-float))
  ;;; EQUALITY
  (is +e+ (exp 1d0) :test '=)
  (ok (= +pi+ π 3.14159265358979323846d0)
      "(= +pi+ π 3.14159265358979323846d0)")
  (ok (= +euler+ +eulergamma+ γ 0.57721566490153286061d0)
      "(= +euler+ +eulergamma+ γ 0.57721566490153286061d0)")
  (is +catalan+ 0.91596559417721901505d0 :test '=)
  (ok (= +golden+ φ 1.61803398874989484820d0)
      "(= +golden+ φ 1.61803398874989484820d0)")
  (is +log2e+ (log (exp 1d0) 2d0) :test '=)
  (is +log10e+ (log (exp 1d0) 10d0) :test '=)
  (is +sqrt-1/2+ (sqrt 0.5d0) :test '=)
  (is +sqrt-3+ (sqrt 3d0) :test '=)
  (is +sqrt-pi+ (sqrt +pi+) :test '=)
  (ok (= +pi/2+ (* 2d0 +pi/4+) (/ +pi+ 2d0))
      "(= +pi/2+ (* 2d0 +pi/4+) (/ +pi+ 2d0))")
  (ok (= 1d0 (* +pi+ +1/pi+) (/ (* +pi+ +2/pi+) 2d0))
      "(= 1d0 (* +pi+ +1/pi+) (* +pi+ +2/pi+))")
  (is 2d0 (fround (exp +ln2+)) :test '=)
  (is 10 (fround (exp +ln10+)) :test '=)
  (ok (= +ln-pi+ (log +pi+))
      "(= +ln-pi+ (log +pi+))")
  (dolist (val (list inf inf32 inf64 -inf32 -inf64))
    (ok (float-infinity-p val) (format nil "(float-infinity-p ~A)" val)))
  #+ecl
  (is nan32 nil)
  (dolist (val #-ecl (list nan32 nan64 nan)
               #+ecl (list nan64 nan))
    (ok (nan-p val) (format nil "(nan-p ~A)" val))
    (ok (nan-p (+ val 1)) (format nil "(nan-p ~A)" val))
    (ok (nan-p (- val 2)) (format nil "(nan-p ~A)" val))
    (ok (nan-p (* val val)) (format nil "(nan-p ~A)" val))
    (ok (nan-p (/ val 10))) (format nil "(nan-p ~A)" val))
  ) ;; end of subtest "CONSTANTS"

(finalize)
