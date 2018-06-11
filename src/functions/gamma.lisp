;;;; gamma.lisp
(in-package #:chenyi)

(declaim (inline %lgamma-asymptotic))

(defvar %factorial-table%
  (let* ((lst '((0 .  1)
                (1 .  1)
                (2 .  2)
                (3 .  6)
                (4 .  24)
                (5 .  120)
                (6 .  720)
                (7 .  5040)
                (8 .  40320)               
                (9 .  362880)
                (10 . 3628800)
                (11 . 39916800)
                (12 . 479001600)
                (13 . 6227020800)
                (14 . 87178291200)
                (15 . 1307674368000)
                (16 . 20922789888000)
                (17 . 355687428096000)
                (18 . 6402373705728000)
                (19 . 121645100408832000)
                (20 . 2432902008176640000)
                (21 . 51090942171709440000)
                (22 . 1124000727777607680000)
                (23 . 25852016738884976640000)
                (24 . 620448401733239439360000)
                (25 . 15511210043330985984000000)
                (26 . 403291461126605635584000000)
                (27 . 10888869450418352160768000000)
                (28 . 304888344611713860501504000000)
                (29 . 8841761993739701954543616000000)
                (30 . 265252859812191058636308480000000)
                (31 . 8222838654177922817725562880000000)
                (32 . 263130836933693530167218012160000000)
                (33 . 8683317618811886495518194401280000000)))
         (table (make-hash-table :test 'eql)))
    (dolist (cons lst table)
      (setf (gethash (car cons) table) (cdr cons))))
  "A cache that stores factorials when x <= 33.")

(defun factorial (n)
  "Compute the Factorial of n. Signal a domain-error if n is not a non-negative integer."
  (declare (optimize speed (safety 1) (space 0)))
  (cond ((numberp n)
         (cond ((or (nan-p n) (and (infinity-p n) (plusp n))) n)
               ((non-negative-integer-p n)
                (multiple-value-bind (val val-exists-p)
                    (gethash n %factorial-table%)
                  (if* val-exists-p
                      then val
                      else (let ((res (* n (factorial (- n 1)))))
                             (setf (gethash n %factorial-table%) res)
                             res))))
               (t (error 'domain-error :operation "factorial" :expect "Non-Negative Integer"))))
        (t (error 'domain-error :operation "factorial" :expect "Non-Negative Integer"))))

(defun lfact (n)
  (declare (optimize speed (safety 1) (space 0)))
  (cond ((numberp n)
         (cond ((or (nan-p n) (and (infinity-p n) (plusp n))) n)
               ((non-negative-integer-p n)
                ;; (log (factorial n) +e+))
                (lgamma (+ 1 n)))
               (t (error 'domain-error :operation "lfact" :expect "Non-Negative Integer"))))
        (t (error 'domain-error :operation "lfact" :expect "Non-Negative Integer"))))

#+(and cffi (or darwin linux))
(progn
  (declaim (inline %gamma/f32 %gamma/f64))
  
  (defun %gamma/f32 (x)
    (declare (type float32 x)
             (optimize speed (safety 0) (space 0)))
    (cffi:foreign-funcall "tgammaf" :float x :float))
  
  (defun %gamma/f64 (x)
    (declare (type float64 x)
             (optimize speed (safety 0) (space 0)))
    (cffi:foreign-funcall "tgamma" :double x :double))

  (defun gamma (x)
    "Compute the Gamma function of x.
Signal domain-error if x < 0; Return Inf if x equals to 0 or a floating-point-overflow condition is signaled."
    (handler-case
        (cond ((numberp x)
               (cond ((or (nan-p x) (and (infinity-p x) (plusp x))) x)
                     ((zerop x) (inf x))
                     ((minusp x)
                      (error 'domain-error :operation "gamma" :expect "Non-Negative Real or Complex"))
                     (t (typecase x
                          (integer (factorial (- x 1)))
                          (float32 (%gamma/f32 x))
                          (float64 (%gamma/f64 x))
                          (rational (gamma (float x 0d0)))
                          (complex/f64 (exp (%lgamma/complex-f64 x)))
                          (complex (exp (%lgamma/complex-f64 (coerce x 'complex/f64))))))))
              (t (error 'domain-error :operation "gamma" :expect "Non-Negative Real or Complex")))
      (floating-point-overflow (c)
        (declare (ignore c))
        inf)))
  
  ) ;; end of progn

(define-compiler-macro gamma (&whole form &environment env x)
  (cond ((constantp x env)
         (gamma (constant-form-value x env)))
        (t form)))

#+(and cffi (or darwin linux))
(progn
  (declaim (inline %lgamma-r/f32 %lgamma-r/f64))
  
  (defun %lgamma-r/f32 (x)
    (declare (type single-float x)
             (optimize speed (safety 1) (space 0)))
    (cffi:with-foreign-object (signp :int)
      (cffi:foreign-funcall "lgammaf_r" :float x :pointer signp :float)))
  
  (defun %lgamma-r/f64 (x)
    (declare (type double-float x)
             (optimize speed (safety 1) (space 0)))
    (cffi:with-foreign-object (signp :int)
      (cffi:foreign-funcall "lgamma_r" :double x :pointer signp :double)))
  
  (defun lgamma (x)
    (handler-case
        (cond ((or (nan-p x) (infinity-p x)) x)
              (t (typecase x
                   (float32 (cond ((zerop x) inf32)
                                  (t (%lgamma-r/f32 x))))
                   (float64 (cond ((zerop x) inf)
                                  (t (%lgamma-r/f64 x))))
                   (rational (cond ((zerop x) inf)
                                   (t (%lgamma-r/f64 (float x 0d0)))))
                   (complex/f32 (let* ((r (float (realpart x) 0d0))
                                       (i (float (imagpart x) 0d0))
                                       (res (%lgamma/complex-f64 (complex r i))))
                                  (complex (float (realpart res) 0f0)
                                           (float (imagpart res) 0f0))))
                   (complex/f64 (%lgamma/complex-f64 x))
                   (complex (%lgamma/complex-f64
                             (let ((r (float (realpart x) 0d0))
                                   (i (float (imagpart x) 0d0)))
                               (complex r i))))
                   (t (error 'domain-error :operation "lgamma" :expect "Number")))))
      ((or floating-point-overflow division-by-zero) (c) 
        (declare (ignore c))
       (typecase x
         (float32 inf32)
         (t inf64)))))
  ) ;; end of progn

(defun %lgamma-asymptotic (z)
  (declare (type (complex double-float) z)
           (optimize speed (safety 0) (space 0)))
  (let* ((zinv #C(0d0 0d0))
         (%t #C(0d0 0d0)))
    (declare (type complex/f64 zinv %t)
             (dynamic-extent zinv %t))
    (setq zinv (/ #C(1d0 0d0) z))
    (setq %t (* zinv zinv))
    ;; coefficients are bernoulli[2:n+1] .// (2*(1:n).*(2*(1:n) - 1))
    (+ (- (* (- z 0.5d0) (log z))
          z)
       9.1893853320467274178032927d-01
       (* zinv (evalpoly (the complex/f64 %t)
                          8.3333333333333333333333368d-02 -2.7777777777777777777777776d-03 
                          7.9365079365079365079365075d-04 -5.9523809523809523809523806d-04 
                          8.4175084175084175084175104d-04 -1.9175269175269175269175262d-03 
                          6.4102564102564102564102561d-03 -2.9550653594771241830065352d-02)))))

;;; from Julia code: https://github.com/JuliaLang/julia/blob/d55cadc350d426a95fd967121ba77494d08364c8/base/special/gamma.jl#L65
;;; Compute the logΓ(z) function using a combination of the asymptotic series,
;;; the Taylor series around z=1 and z=2, the reflection formula, and the shift formula.
;;; Many details of these techniques are discussed in D. E. G. Hare,
;;; "Computing the principal branch of log-Gamma," J. Algorithms 25, pp. 221-236 (1997),
;;; and similar techniques are used (in a somewhat different way) by the
;;; SciPy loggamma function.  The key identities are also described
;;; at http://functions.wolfram.com/GammaBetaErf/LogGamma/
(defun %lgamma/complex-f64 (z)
  (declare (type complex/f64 z)
           (optimize speed (safety 0) (space 0)))
  (let* ((x 0d0) (y 0d0) (yabs 0d0)
         (w #C(0d0 0d0)))
    (declare (type float64 x y yabs)
             (type complex/f64 w)
             (dynamic-extent x y yabs w))
    (setq x (realpart z)
          y (imagpart z)
          yabs (abs y))
    (cond ((or (infinity-p x)
               (infinity-p y))
           (cond ((and (finity-p x) (infinity-p y))
                  ;; return Complex(x, x > 0 ? (y == 0 ? y : copysign(Inf, y)) : copysign(Inf, -y))
                  (complex x (the float64 (if (plusp x)
                                              (if (zerop y) y (inf y))
                                              (inf (- y))))))
                 ((and (infinity-p x) (finity-p y))
                  (complex -inf64 y))
                 (t (complex nan nan))))
          ((or (> x 7d0) (> yabs 7d0))
           (%lgamma-asymptotic z))
          (;; use reflection formula to transform to x > 0
           (< x 0.1d0)           
           (if (and (zerop x) (zerop y))
               ;; return Complex(Inf, signbit(x) ? copysign(oftype(x, pi), -y) : -y)
               (complex inf (if (signbit x)
                                (if (zerop (ieee-floats:encode-float64 (- y)))
                                    +pi+
                                    (- +pi+))
                                (- y)))
               (- (complex 1.1447298858494001741434262d0
                           (- (* (if (minusp y)
                                     -6.2831853071795864769252842d0
                                     6.2831853071795864769252842d0)
                                 (float (floor (the (double-float * 0.3d0)
                                                    (+ 0.25d0 (* 0.5d0 x)))
                                               1d0)
                                        0d0))))
                  (the complex/f64 (log (sin (* +pi+ z))))
                  (the complex/f64 (%lgamma/complex-f64 (- 1d0 z))))))
          (;; abs(x - 1) + yabs < 0.1
           (< (+ (abs (- x 1d0)) yabs) 0.1d0)
           ;; taylor series around zero at z=1
           ;; ... coefficients are [-eulergamma; [(-1)^k * zeta(k)/k for k in 2:15]]
           (setq w (complex (- x 1d0) y))
           (* w (evalpoly w -5.7721566490153286060651188d-01 8.2246703342411321823620794d-01 
                          -4.0068563438653142846657956d-01 2.705808084277845478790009d-01 
                          -2.0738555102867398526627303d-01 1.6955717699740818995241986d-01 
                          -1.4404989676884611811997107d-01 1.2550966952474304242233559d-01 
                          -1.1133426586956469049087244d-01 1.000994575127818085337147d-01 
                          -9.0954017145829042232609344d-02 8.3353840546109004024886499d-02 
                          -7.6932516411352191472827157d-02 7.1432946295361336059232779d-02 
                          -6.6668705882420468032903454d-02)))
          (;; abs(x - 2) + yabs < 0.1
           (< (+ (abs (- x 2d0)) yabs) 0.1d0)
           ;; taylor series around zero at z=2
           ;; ... coefficients are [1-eulergamma; [(-1)^k * (zeta(k)-1)/k for k in 2:12]]
           (setq w (complex (- x 2d0) y))
           (* w (evalpoly w 4.2278433509846713939348812d-01 3.2246703342411321823620794d-01 
                          -6.7352301053198095133246196d-02 2.0580808427784547879000897d-02 
                          -7.3855510286739852662729527d-03 2.8905103307415232857531201d-03 
                          -1.1927539117032609771139825d-03 5.0966952474304242233558822d-04 
                          -2.2315475845357937976132853d-04 9.9457512781808533714662972d-05 
                          -4.4926236738133141700224489d-05 2.0507212775670691553131246d-05)))
          (t ;; use recurrence relation lgamma(z) = lgamma(z+1) - log(z) to shift to x > 7 for asymptotic series
           (let ((shiftprod #C(0d0 0d0))
                 (sb nil) (sb* nil)
                 (signflips 0)
                 (shift #C(0d0 0d0)))
             (declare (type complex/f64 shiftprod shift)
                      (type (or t nil) sb sb*)
                      (type fixnum signflips)
                      (dynamic-extent shiftprod sb sb* signflips shift))
             (setq x         (+ x 1d0)
                   shiftprod (complex x yabs))
             (while (<= x 7d0)
               (setq shiftprod (* shiftprod (complex x yabs)))
               (setq sb* (signbit (imagpart shiftprod)))
               (when (and sb* (not (eql sb sb*)))
                 (incf signflips 1))
               (setq sb sb*)
               (setq x (+ x 1d0)))
             (setq shift (log shiftprod +e+))
             (if (signbit y)
                 (setq shift (complex (realpart shift)
                                      (- (* signflips -6.2831853071795864769252842d0)
                                         (imagpart shift))))
                 (setq shift (complex (realpart shift)
                                      (+ (imagpart shift)
                                         (* signflips 6.2831853071795864769252842d0)))))
             (- (the complex/f64 (%lgamma-asymptotic (complex x y)))
                shift))))))
