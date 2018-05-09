;;;; elementary.lisp
(in-package #:chenyi-test)

(plan nil)

(subtest "F64CMP"
  (ok (= 0 (f64cmp 0d0 0d0 double-float-epsilon))
      "(= 0 (f64cmp 0d0 0d0 double-float-epsilon))")
  (ok (= -1 (f64cmp 0d0 1d0 double-float-epsilon))
      "(= -1 (f64cmp 0d0 1d0 double-float-epsilon))")
  (ok (= 1 (f64cmp 1d0 0d0 double-float-epsilon))
      "(= 1 (f64cmp 1d0 0d0 double-float-epsilon))")
  (ok (= 0 (f64cmp 10d0 (+ 10d0 double-float-epsilon) double-float-epsilon))
      "(= 0 (f64cmp 10d0 (+ 10d0 double-float-epsilon) double-float-epsilon))")
  (ok (f64cmp< 0.1d0 0.2d0 0.3d0 0.4d0 0.9d0)
      "(f64cmp< 0.1d0 0.2d0 0.3d0 0.4d0 0.9d0)")
  (ok (f64cmp<= 0.1d0 0.2d0 0.3d0 (+ 0.3d0 double-float-epsilon) 0.4d0 0.9d0)
      "(f64cmp<= 0.1d0 0.2d0 0.3d0 (+ 0.3d0 double-float-epsilon) 0.4d0 0.9d0)")
  (ok (f64cmp= 0.1d0 0.1d0 0.1d0)
      "(f64cmp= 0.1d0 0.1d0 0.1d0)")
  (ok (not (f64cmp> 1.0d0 0.9d0 0.3d0 0.4d0 0.5d0))
      "(not (f64cmp> 1.0d0 0.9d0 0.3d0 0.4d0 0.5d0))")
  (ok (f64cmp>= 1.0d0 0.9d0 0.9d0)
      "(f64cmp>= 1.0d0 0.9d0 0.9d0)"))

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
  (is +e+ (exp 1d0) :test 'f64cmp=)
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
  (ok (f64cmp= +pi/2+ (* 2d0 +pi/4+) (/ +pi+ 2d0))
      "(f64cmp= +pi/2+ (* 2d0 +pi/4+) (/ +pi+ 2d0))")
  (ok (f64cmp= 1d0 (* +pi+ +1/pi+) (/ (* +pi+ +2/pi+) 2d0))
      "(f64cmp= 1d0 (* +pi+ +1/pi+) (* +pi+ +2/pi+))")
  (is 2d0 (exp +ln2+) :test 'f64cmp=)
  (is 10d0 (exp +ln10+) :test 'f64cmp=)
  (ok (f64cmp= +ln-pi+ (log +pi+))
      "(f64cmp= +ln-pi+ (log +pi+))")
  (dolist (val (list inf inf32 inf64 -inf32 -inf64))
    (ok (infinity-p val) (format nil "(infinity-p ~A)" val)))
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

(subtest "EXPM1"
  (let ((re 1d-15))
    (test-rel (expm1 0d0) 0 re "(expm1 0d0)")
    (test-rel (expm1 1d-10) 1.000000000050000000002d-10 re "(expm1 1d-10)")
    (test-rel (expm1 -1d-10) -9.999999999500000000017d-11 re "(expm1 -1d-10)")
    (test-rel (expm1 0.1) 0.1051709180756476248117078264902d0 re "(expm1 0.1)")
    (test-rel (expm1 -0.1) -0.09516258196404042683575094055356d0 re "(expm1 -0.1)")
    (test-rel (expm1 10) 22025.465794806716516957900645284d0 re "(expm1 10)")
    (test-rel (expm1 -10) -0.99995460007023751514846440848444d0 re "(expm1 -10)")))

(subtest "FUNCTIONS"
  (ok (- (log1p 0.5d0) (log 1.5d0))
      "(f64cmp= (log1p 0.5d0) (log 1.5d0))")
  (ok (- (expm1 0.2345d0) (- (exp 0.2345d0) 1d0))
      "(f64cmp= (expm1 0.2345d0) (- (exp 0.2345d0) 1d0))")
  #+abcl
  (ok (multiple-value-bind (x exp) (frexp/f32 10.0)
        (and (= x 0.625) (= exp 4)))
      "(frexp/f32 10.0)")
  (ok (multiple-value-bind (x exp) (frexp/f64 10d0)
        (and (= x 0.625d0) (= exp 4)))
      "(frexp/f64 10d0)")
  (ok (f64cmp= (ldexp/f64 0.625d0 4) 10d0)
      "(ldexp/f64 0.625d0 4)")
  (ok (f64cmp= (hypot 3 4d0) 5d0)
      "(f64cmp= (hypot 3 4d0) 5d0)")
  (ok (f64cmp= (hypot3 3d0 4 (sqrt 11d0)) 6d0)
      "(f64cmp= (hypot3 3d0 4 (sqrt 11d0)) 6d0)"))
  
(finalize)
