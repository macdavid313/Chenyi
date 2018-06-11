;;;; chenyi.lisp
(in-package #:cl-user)
(defpackage #:chenyi-test
  (:use #:cl #:chenyi #:prove))
  ;; (:shadowing-import-from #:chenyi #:acosh #:asinh #:atanh)
(in-package #:chenyi-test)

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

#+(or allegro lispworks)
(setf *enable-colors* nil)

(defun test-rel (got expected relative-error &optional description)
  (let (status)
    (cond ((or (or (nan-p got) (nan-p expected))
               (or (infinity-p got) (infinity-p expected)))
           (setq status (or (and (nan-p got) (nan-p expected))
                            (and (infinity-p got) (infinity-p expected)))))
          ((or (< 0 expected least-positive-double-float)
               (< least-negative-double-float expected 0))
           (setq status t))
          ((not (zerop expected))
           (setq status (<= (/ (abs (- got expected))
                              (abs expected))
                           relative-error)))
          (t (setq status (<= (abs got) relative-error))))
    (ok status description)))

(defun test-rel/complex (got expected relative-error &optional description)
  (let ((r-got (realpart got))
        (i-got (imagpart got))
        (r-expected (realpart expected))
        (i-expected (imagpart expected)))
    (test-rel r-got r-expected relative-error (format nil "~a realpart --> ~a" description r-got))
    (test-rel i-got i-expected relative-error (format nil "~a imagpart --> ~a" description i-got))))

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
  (ok (= 0 (fcmp 0d0 0d0))
      "(= 0 (fcmp 0d0 0d0))")
  (ok (= -1 (fcmp 0d0 1d0))
      "(= -1 (fcmp 0d0 1d0))")
  (ok (= 1 (fcmp 1d0 0d0))
      "(= 1 (fcmp 1d0 0d0))")
  (ok (= 0 (fcmp 10d0 (+ 10d0 double-float-epsilon)))
      "(= 0 (fcmp 10d0 (+ 10d0 double-float-epsilon)))")
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
    (test-rel (expm1 0d0) 0 re "(expm1 0d0) --> 0d0")
    (test-rel (expm1 1d-10) 1.000000000050000000002d-10 re "(expm1 1d-10)")
    (test-rel (expm1 -1d-10) -9.999999999500000000017d-11 re "(expm1 -1d-10)")
    (test-rel (expm1 0.1d0) 0.1051709180756476248117078264902d0 re "(expm1 0.1d0)")
    (test-rel (expm1 -0.1d0) -0.09516258196404042683575094055356d0 re "(expm1 -0.1d0)")
    (test-rel (expm1 10) 22025.465794806716516957900645284d0 re "(expm1 10)")
    (test-rel (expm1 -10) -0.99995460007023751514846440848444d0 re "(expm1 -10)")
    (test-rel (expm1 1000f0) inf32 re "(expm1 1000)")
    (test-rel (expm1 1000) inf re "(expm1 1000)")
    (test-rel (expm1 nan) nan re "(expm1 NaN)")
    (test-rel (expm1 inf32) inf re "(expm1 Inf32)")
    (test-rel (expm1 inf) inf re "(expm1 Inf)")
    (test-rel/complex (expm1 #C(10 20)) #C(8987.60557600688d0 20108.957337683332d0) re "(expm1 #C(10 20))")
    (test-rel/complex (expm1 #C(-1000 -20)) #C(-1d0 0d0) re "(expm1 #C(-1000 -20))")
    (test-rel/complex (expm1 #C(-1000 -20)) #C(-1d0 0d0) re "(expm1 #C(-1d0 0d0))")
    (test-rel/complex (expm1 #C(1000 -20)) (complex inf -inf) re "(expm1 #C(1000 -20))")
    (test-rel/complex (expm1 #C(1000 -2000)) (complex -inf -inf) re "(expm1 #C(1000 -2000))")))

(subtest "log1p"
  (let ((re 1d-15)
        (data '((0d0 . 0d0)
                (1d-10 . 9.9999999995000000000333333333308d-11)
                (0.1d0 . 0.095310179804324860043952123280765d0)
                (10d0 . 2.3978952727983705440619435779651d0))))
    (loop for (arg . expected) in data
       do (test-rel (log1p arg) expected re
                    (format nil "(log1p ~a) --> ~a" arg (log1p arg))))
    (test-rel (log1p nan) nan re "(log1p NaN)")
    (test-rel (log1p inf32) inf re "(log1p Inf32)")
    (test-rel (log1p inf) inf re "(log1p Inf)")
    (test-rel (log1p least-positive-double-float) 4.9406564584124654d-324 re
              "(log1p least-positive-double-float) --> 4.9406564584124654d-324")
    (test-rel/complex (log1p most-negative-double-float) #C(709.782712893384d0 3.141592653589793d0) re
                      "(log1p most-negative-double-float) --> #C(709.782712893384d0 3.141592653589793d0)")
    (ok (infinity-p (log1p -1f0)) "(log1p -1f0) --> Inf32")
    (ok (infinity-p (log1p -1d0)) "(log1p -1d0) --> Inf")
    (test-rel/complex (log1p -100) #C(4.59511985013459d0 3.141592653589793d0) re
                      "(log1p #C(-100 0)) --> #C(4.59511985013459d0 3.141592653589793d0)")))
  

(subtest "ldexp"
  (let ((re 1d-15)
        (data `((,+pi+ -2 ,+pi/4+)
                (1d0 2 4.000000d0)
                (0d0 2 0d0)
                (0.9999999999999999d0 1024 1.7976931348623157d308)
                (1d308 -2000 8.7098098162172166755761d-295)
                (4.9406564584124654d-324 2000 5.67251933470834d278))))
    (loop for (x exponent expected) in data
       do (test-rel (ldexp x exponent) expected re
                    (format nil "(ldexp ~a ~a) --> ~a" x exponent (ldexp x exponent)))))
  (let ((i 0) y
        (x 2.2250738585072014d-308)
        (expected 2.554675596204441378334779940d294))
    (setq x (/ x 2))
    (while (> x 0)
      (incf i)
      (setq y (ldexp x (+ i 2000)))
      (test-rel y expected 1d-15 (format nil "(ldexp ~a ~a) --> ~a" x (+ i 2000) y))
      (setq x (/ x 2)))))

(subtest "frexp"
  (let ((re 1d-15)
        (data `((0d0 0 0)
                (,+pi+ ,+pi/4+ 2)
                (2d0 0.5d0 2)
                (,(/ 1d0 4d0) 0.5d0 -1)
                (,(- (/ 1d0 4d0)
                     (* 8d0 double-float-epsilon))
                  0.999999999999996447d0 -2)
                (1.7976931348623157d308 9.999999999999998890d-1 1024)
                (-1.7976931348623157d308 -9.999999999999998890d-1 1024)
                (4.9406564584124654d-324 0.5d0 -1073)
                (-4.9406564584124654d-324 -0.5d0 -1073))))
    (loop for (arg expected-x expected-exponent) in data
       do (multiple-value-bind (x exponent) (frexp arg)
            (test-rel x expected-x re (format nil "(frexp ~a) fraction --> ~a" arg x))
            (ok (= exponent expected-exponent) (format nil "(frexp ~a) exponent --> ~a" arg exponent))))
    (let ((i 0) (y 0d0) (e 0) (x 2.2250738585072014d-308)
          (y-expected 0.5d0) (e-expected -1021))
      (setq x (/ x 2d0))
      (while (> x 0)
        (decf e-expected)
        (incf i)
        (multiple-value-setq (y e) (frexp x))
        (test-rel y y-expected re (format nil "(frexp ~a) fraction --> ~a" x y))
        (ok (= e e-expected) (format nil "(frexp ~a) exponent --> ~a" x e))
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

(subtest "hypot"
  (let ((re 1d-15)
        (data (list '(0 0 0)
                    '(1d-10 1d-10 1.414213562373095048801688d-10)
                    '(1d-38 1d-38 1.414213562373095048801688d-38)
                    '(1d-10 -1d0 1.000000000000000000005d0)
                    '(-1d0 1d-10 1.000000000000000000005d0)
                    '(1d307 1d301 1.000000000000499999999999d307)
                    '(1d301 1d307 1.000000000000499999999999d307)
                    '(1d307 1d307 1.414213562373095048801688d307)
                    (list inf 1.2d0 inf)
                    (list -inf 1.2d0 inf)
                    (list 1.2d0 inf inf)
                    (list 1.2d0 -inf inf)
                    (list 1.2d0 nan nan)
                    (list nan 1.2d0 nan)
                    (list nan nan nan)
                    (list inf nan inf)
                    (list -inf nan inf)
                    (list nan inf inf)
                    (list nan -inf inf))))
    (loop for (x y expected) in data
       do (let ((res (hypot x y)))
            (test-rel res expected re (format nil "(hypot ~a ~a) --> ~a" x y res))))))

(subtest "hypot3"
  (let ((re 1d-15)
        (data '((0d0 0d0 0d0 0d0)
                (1d-10 1d-10 1d-10 1.732050807568877293527446d-10)
                (1d-38 1d-38 1d-38 1.732050807568877293527446d-38)
                (1d-10 1d-10 -1d0 1.000000000000000000099d0)
                (1d-10 -1d0 1d-10 1.000000000000000000099)
                (-1d0 1d-10 1d-10 1.000000000000000000099)
                (1d307 1d301 1d301 1.0000000000009999999999995d307)
                (1d307 1d307 1d307 1.732050807568877293527446d307)
                (1d307 1d-307 1d-307 1d307))))
    (loop for (x y z expected) in data
       do (let ((res (hypot3 x y z)))
            (test-rel res expected re (format nil "(hypot3 ~a ~a ~a) --> ~a" x y z res))))))

(subtest "factorial"
  (let ((data '((0 . 1)
                (10 . 3628800)
                (20 . 2432902008176640000)
                (30 . 265252859812191058636308480000000)
                (40 . 815915283247897734345611269596115894272000000000)
                (50 . 30414093201713378043612608166064768844377641568960512000000000000))))
    (loop for (n . expected) in data
       do (let ((res (factorial n)))
            (ok (= res expected) (format nil "(factorial ~a) --> ~a" n res)))))
  (ok (nan-p (factorial nan)) "(factorial nan) --> NaN")
  (ok (infinity-p (factorial inf)) "(factorial inf) --> Inf")
  ;; (is-condition (factorial -inf) 'domain-error "(factorial -inf) --> 'domain-error")
  ;; (is-condition (factorial 10.123d0) 'domain-error "(factorial 10.123d0) --> 'domain-error")
  ;; (is-condition (factorial -1) 'domain-error "(factorial -1) --> 'domain-error")
  )

(subtest "lfact"
  (let ((data '((0  . 0d0)
                (10 . 15.104412573075518d0)
                (20 . 42.335616460753485d0)
                (30 . 74.65823634883017d0)
                (40 . 110.3206397147574d0)
                (50 . 148.47776695177302d0))))
    (loop for (n . expected) in data
       do (let ((res (lfact n)))
            (test-rel res expected 1d-15 (format nil "(lfact ~a) --> ~a" n res)))))
  (ok (nan-p (lfact nan)) "(lfact nan) --> NaN")
  (ok (infinity-p (lfact inf)) "(lfact inf) --> Inf")
  ;; (is-condition (lfact -inf) 'domain-error "(lfact -inf) --> 'domain-error")
  ;; (is-condition (lfact 10.123d0) 'domain-error "(lfact 10.123d0) --> 'domain-error")
  ;; (is-condition (lfact -1) 'domain-error "(lfact -1) --> 'domain-error")
  )

(subtest "gamma"
  (let ((re 1d-15)
        (data '((1 . 1)
                (1.5d0 . 0.886226925452758d0)
                (2 . 1)
                (3 . 2)
                (3.235d0 . 2.510765325008696d0)
                (100.567d0 . 1.2690263959887244d157))))
    (loop for (x . expected) in data
       do (let ((res (gamma x)))
            (test-rel res expected re (format nil "(gamma ~a) --> ~a" x res))))
    (ok (nan-p (gamma nan)) "(gamma nan) --> NaN")
    (ok (infinity-p (gamma inf)) "(gamma inf) --> Inf")
    (ok (infinity-p (gamma 0)) "(gamma 0) --> Inf")
    (ok (infinity-p (gamma 1000.567d0)) "(gamma 1000.567d0) --> Inf")
    ;; (is-condition (gamma -2) 'domain-error "(gamma -2) --> 'domain-error")
    ;; (is-condition (gamma -inf) 'domain-error "(gamma -inf) --> 'domain-error")
    ))

(subtest "lgamma"
  (let ((re 1d-15)
        (data1 `((0 . ,inf)
                 (0.0 . ,inf32)
                 (-1 . ,inf)
                 (1 . 0d0)
                 (2 . 0d0)
                 (30 . 71.25703896716801d0)
                 (100 . 359.13420536957545d0)
                 (1.5d0 . -0.12078223763524522d0)
                 (-1.5d0 . 0.8600470153764809d0)
                 (2.5d0 . 0.2846828704729192d0)
                 (-2.5d0 . -0.05624371649767412d0)
                 (100.235d0 . 360.2155206969818d0)
                 (-100.235d0 . -363.2823168843038d0)))
        (data2 `(;; FIXME!
                 #+sbcl (#C(0d0 0d0) . ,(complex inf -0d0))                 
                 #+sbcl (#C(0d0 -0d0) . ,(complex inf 0d0))
                 #+sbcl (#C(-0d0 0d0) . ,(complex inf -3.141592653589793d0))
                 #+sbcl (#C(-0d0 -0d0) . ,(complex inf 3.141592653589793d0))
                 (#C(10d0 2d0) . #C(12.593014835793278d0 4.518047609564626d0))
                 (#C(2d0 10d0) . #C(-11.330171929826642d0 15.274040648533632d0))
                 (#C(10d0 10d0) . #C(8.236131750448719d0 23.948703413782038d0))
                 (#C(-1.2d0 -8d0) . #C(-15.194045700225523d0 -5.790875904190495d0))
                 (#C(0.005d0 12) . #C(-19.160643327703145d0 17.03459685339347d0))
                 (#C(-0.1d0 12) . #C(-19.421637802762646d0 16.864873197046347d0))
                 (#C(1.0005d0 0.005d0) . #C(-0.000308948757239074d0 -0.002881917473758175d0))
                 (#C(1.005d0 0.0005d0) . #C(-0.0028657706901617386d0 -0.0002845104062565174d0))
                 (#C(2.0005d0 0.005d0) . #C(0.00020341363784547278d0 0.002115542172011452d0))
                 (#C(2.005d0 0.0005d0) . #C(0.0021218945801748006d0 0.0002130019905065691d0))
                 (#C(6.5d0 6.5d0) . #C(2.6305178506969393d0 12.62542374782836d0))
                 (#C(4.5d0 3.5d0) . #C(1.0793119609666935d0 5.227382227157847d0)))))
    (loop for (x . expected) in data1
       do (let ((got (lgamma x)))
            (test-rel got expected re (format nil "(lgamma ~a) --> ~a" x got))))
    (loop for (x . expected) in data2
       do (let ((got (lgamma x)))
            (test-rel/complex got expected re (format nil "(lgamma ~a)" x))))))
                 
(finalize)
