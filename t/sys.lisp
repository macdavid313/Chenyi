;;;; elementary.lisp
(in-package #:chenyi-test)

(plan nil)

(subtest "fcmp"
  (let ((x +pi+)
        (y (/ 22d0 7d0)))
    (loop for i from 0 below 10
       do (let* ((tol (expt 10 (- i)))
                 (res (fcmp x y tol)))
            (ok (= res (- (if (>= i 4) 1 0)))
                (format nil "(fcmp ~a ~a ~a)" x y tol))))
    (loop for i from 0 below 10
       do (let* ((tol (expt 10 (- i)))
                 (res (fcmp y x tol)))
            (ok (= res (if (>= i 4) 1 0))
                (format nil "(fcmp ~a ~a ~a)" y x tol)))))
  (ok (= 0 (fcmp 0d0 0d0 double-float-epsilon))
      "(= 0 (fcmp 0d0 0d0 double-float-epsilon))")
  (ok (= -1 (fcmp 0d0 1d0 double-float-epsilon))
      "(= -1 (fcmp 0d0 1d0 double-float-epsilon))")
  (ok (= 1 (fcmp 1d0 0d0 double-float-epsilon))
      "(= 1 (fcmp 1d0 0d0 double-float-epsilon))")
  (ok (= 0 (fcmp 10d0 (+ 10d0 double-float-epsilon) double-float-epsilon))
      "(= 0 (fcmp 10d0 (+ 10d0 double-float-epsilon) double-float-epsilon))")
  (ok (fcmp< 0.1d0 0.2d0 0.3d0 0.4d0 0.9d0)
      "(fcmp< 0.1d0 0.2d0 0.3d0 0.4d0 0.9d0)")
  (ok (fcmp<= 0.1d0 0.2d0 0.3d0 (+ 0.3d0 double-float-epsilon) 0.4d0 0.9d0)
      "(fcmp<= 0.1d0 0.2d0 0.3d0 (+ 0.3d0 double-float-epsilon) 0.4d0 0.9d0)")
  (ok (fcmp= 0.1d0 0.1d0 0.1d0)
      "(fcmp= 0.1d0 0.1d0 0.1d0)")
  (ok (not (fcmp> 1.0d0 0.9d0 0.3d0 0.4d0 0.5d0))
      "(not (fcmp> 1.0d0 0.9d0 0.3d0 0.4d0 0.5d0))")
  (ok (fcmp>= 1.0d0 0.9d0 0.9d0)
      "(fcmp>= 1.0d0 0.9d0 0.9d0)")
  ) ;; end of subtest "fcmp"

(subtest "constants"
  ;;; TYPE
  (dolist (val (list inf32 -inf32 #+linux nan32))
    (is-type val 'single-float))
  (dolist (val (list +e+ +pi+ +euler+ +eulergamma+
                     +catalan+ +golden+
                     +log2e+ +log10e+
                     +sqrt-1/2+ +sqrt-2+ +sqrt-3+ +sqrt-pi+
                     +pi/2+ +pi/4+ +1/pi+ +2/pi+
                     +ln2+ +ln10+ +ln-pi+ inf64 -inf64 inf))
    (is-type val 'double-float))
  ;;; EQUALITY
  (is +e+ (exp 1d0) :test 'fcmp=)
  (ok (= +pi+ 3.14159265358979323846d0)
      "(= +pi+ 3.14159265358979323846d0)")
  (ok (= +euler+ +eulergamma+ 0.57721566490153286061d0)
      "(= +euler+ +eulergamma+ 0.57721566490153286061d0)")
  (is +catalan+ 0.91596559417721901505d0 :test '=)
  (ok (= +golden+ 1.61803398874989484820d0)
      "(= +golden+ 1.61803398874989484820d0)")
  (is +log2e+ (log (exp 1d0) 2d0) :test '=)
  (is +log10e+ (log (exp 1d0) 10d0) :test '=)
  (is +sqrt-1/2+ (sqrt 0.5d0) :test '=)
  (is +sqrt-3+ (sqrt 3d0) :test '=)
  (is +sqrt-pi+ (sqrt +pi+) :test '=)
  (ok (fcmp= +pi/2+ (* 2d0 +pi/4+) (/ +pi+ 2d0))
      "(fcmp= +pi/2+ (* 2d0 +pi/4+) (/ +pi+ 2d0))")
  (ok (fcmp= 1d0 (* +pi+ +1/pi+) (/ (* +pi+ +2/pi+) 2d0))
      "(fcmp= 1d0 (* +pi+ +1/pi+) (* +pi+ +2/pi+))")
  (is 2d0 (exp +ln2+) :test 'fcmp=)
  (is 10d0 (exp +ln10+) :test 'fcmp=)
  (ok (fcmp= +ln-pi+ (log +pi+))
      "(fcmp= +ln-pi+ (log +pi+))")
  (dolist (val (list inf inf32 inf64 -inf32 -inf64))
    (ok (infinity-p val) (format nil "(infinity-p ~A)" val)))
  (dolist (val (list #+linux nan32 nan64 nan))
    (ok (nan-p val) (format nil "(nan-p ~A)" val))
    (ok (nan-p (+ val 1)) (format nil "(nan-p ~A)" val))
    (ok (nan-p (- val 2)) (format nil "(nan-p ~A)" val))
    (ok (nan-p (* val val)) (format nil "(nan-p ~A)" val))
    (ok (nan-p (/ val 10))) (format nil "(nan-p ~A)" val))
  ) ;; end of subtest "CONSTANTS"

(subtest "expm1"
  (let ((re 1d-15))
    (test-rel (expm1 0d0) 0 re "(expm1 0d0)")
    (test-rel (expm1 1d-10) 1.000000000050000000002d-10 re "(expm1 1d-10)")
    (test-rel (expm1 -1d-10) -9.999999999500000000017d-11 re "(expm1 -1d-10)")
    (test-rel (expm1 0.1d0) 0.1051709180756476248117078264902d0 re "(expm1 0.1d0)")
    (test-rel (expm1 -0.1d0) -0.09516258196404042683575094055356d0 re "(expm1 -0.1d0)")
    (test-rel (expm1 10) 22025.465794806716516957900645284d0 re "(expm1 10)")
    (test-rel (expm1 -10) -0.99995460007023751514846440848444d0 re "(expm1 -10)")))

(subtest "log1p"
  (let ((re 1d-15)
        (data '((0d0 . 0d0)
                (1d-10 . 9.9999999995000000000333333333308d-11)
                (0.1d0 . 0.095310179804324860043952123280765d0)
                (10d0 . 2.3978952727983705440619435779651d0))))
    (loop for (arg . expected) in data
       do (test-rel (log1p arg) expected re
                    (format nil "(log1p ~a)" arg)))))

(subtest "ldexp"
  (let ((re 1d-15)
        (data `((,+pi+ -2 ,+pi/4+)
                (1d0 2 4.000000d0)
                (0d0 2 0d0)
                (9.99999999999999999d-01 1024 ,most-positive-double-float)
                (1d308 -2000 8.7098098162172166755761d-295)
                (,least-positive-double-float 2000 2.554675596204441378334779940d294))))
    (loop for (x exponent expected) in data
       do (test-rel (ldexp x exponent) expected re
                    (format nil "(ldexp ~a ~a)" x exponent))))
  (let ((i 0) y
        (x least-positive-double-float)
        (expected 2.554675596204441378334779940d294))
    (setq x (/ x 2))
    (while (> x 0)
      (incf i)
      (setq y (ldexp x (+ i 2000)))
      (test-rel y expected 1d-15 (format nil "(ldexp ~a ~a)" x (+ i 2000)))
      (setq x (/ x 2)))))

(subtest "frexp"
  (let ((re 1d-15)
        (data `((0d0 0 0)
                (,+pi+ ,+pi/4+ 2)
                (2d0 0.5d0 2)
                (,(/ 1d0 4d0) 0.5d0 -1)
                (,(- (/ 1d0 4d0)
                     (* 4d0 double-float-epsilon))
                  0.999999999999996447d0 -2)
                (,most-positive-double-float 9.999999999999998890d-1 1024)
                (,most-negative-double-float -9.999999999999998890d-1 1024)
                (,least-positive-double-float 0.5d0 -1021)
                (,least-negative-double-float -0.5d0 -1021))))
    (loop for (arg expected-x expected-exponent) in data
       do (multiple-value-bind (x exponent) (frexp arg)
            (test-rel x expected-x re (format nil "(frexp ~a) fraction" arg))
            (ok (= exponent expected-exponent) (format nil "(frexp ~a) exponent" arg))))
    (let ((i 0) (y 0d0) (e 0) (x least-positive-double-float)
          (y-expected 0.5d0) (e-expected -1021))
      (setq x (/ x 2d0))
      (while (> x 0)
        (decf e-expected)
        (incf i)
        (multiple-value-setq (y e) (frexp x))
        (test-rel y y-expected re (format nil "(frexp ~a) fraction" x))
        (ok (= e e-expected) (format nil "(frexp ~a) exponent" x))
        (setq x (/ x 2d0))))))

;; (subtest "invhyp"
;;   (let ((re 1d-15))
;;     ;; tests for acosh
;;     (test-rel (acosh 1d0) 0d0 re "(acosh 1d0)")
;;     (test-rel (acosh 1.1d0) 4.435682543851151891329110663525d-1 re "(acosh 1.1d0)")
;;     (test-rel (acosh 10d0) 2.9932228461263808979126677137742d0 re "(acosh 10d0)")
;;     (test-rel (acosh 1d10) 2.3718998110500402149594646668302d1 re "(acosh 1d10)")
;;     ;; tests for asinh
;;     (test-rel (asinh 0d0) 0d0 re "(asinh 0d0)")
;;     (test-rel (asinh 1d-10) 9.9999999999999999999833333333346d-11 re "(asinh 1d-10)")
;;     (test-rel (asinh -1d-10) -9.9999999999999999999833333333346d-11 re "(asinh -1d-10)")
;;     (test-rel (asinh 0.1d0) 9.983407889920756332730312470477d-2 re "(asinh 0.1d0)")
;;     (test-rel (asinh -0.1d0) -9.983407889920756332730312470477d-2 re "(asinh -0.1d0)")
;;     (test-rel (asinh 1d0)  8.8137358701954302523260932497979d-1 re "(asinh 1d0)")
;;     (test-rel (asinh -1d0)  -8.8137358701954302523260932497979d-1 re "(asinh -1d0)")
;;     (test-rel (asinh 10d0) 2.9982229502979697388465955375965d0 re "(asinh 10d0)")
;;     (test-rel (asinh -10d0) -2.9982229502979697388465955375965d0 re "(asinh -10d0)")
;;     (test-rel (asinh 1d10) 2.3718998110500402149599646668302d1 re "(asinh 1d10)")
;;     (test-rel (asinh -1d10) -2.3718998110500402149599646668302d1 re "(asinh -1d10)")
;;     ;; tests for atanh
;;     (test-rel (atanh 0d0) 0d0 re "(atanh 0d0)")
;;     (test-rel (atanh 1d-20) 1d-20 re "(atanh 1d-20)")
;;     (test-rel (atanh -1d-20) -1d-20 re "(atanh -1d-20)")
;;     (test-rel (atanh 0.1d0) 1.0033534773107558063572655206004d-1 re "(atanh 0.1d0)")
;;     (test-rel (atanh -0.1d0) -1.0033534773107558063572655206004d-1 re "(atanh -0.1d0)")
;;     (test-rel (atanh 0.9d0) 1.4722194895832202300045137159439d0 re "(atanh 0.9d0)")
;;     (test-rel (atanh -0.9d0) -1.4722194895832202300045137159439d0 re "(atanh -0.9d0)")))
  
(finalize)
