;;;; gamma.lisp
(in-package #:chenyi)

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
  "Compute the Factorial of n.
Signal a domain-error if n is not a non-negative integer."
  (declare (optimize speed (safety 0) (space 0)))
  (cond ((not (non-negative-integer-p n))
         (error 'domain-error :operation "Factorial" :expect "Non-Negative Integer"))
        ((<= n 33)
         (multiple-value-bind (res res-existed-p)
             (gethash n %factorial-table%)
           (declare (ignorable res-existed-p))
           res))
        (t (loop for i from 34 to n
              with product = (gethash 33 %factorial-table%)
              do (setf product (* product i))
              finally (return product)))))

#+(cffi (or darwin linx))
(defun %gamma/f32 (x)
  (declare (type single-float x)
           (dynamic-extent x)
           (optimize speed (safety 0) (space 0)))
  (cond ((minusp x) (error 'domain-error :operation "Gamma"))
        (t (cffi:foreign-funcall "tgammaf" :float x :float))))
  
#+(cffi (or darwin linx))
(defun %gamma/f64 (x)
  (declare (type double-float x)
           (dynamic-extent x)
           (optimize speed (safety 0) (space 0)))
  (cond ((minusp x) (error 'domain-error :operation "Gamma"))
        (t (cffi:foreign-funcall "tgamma" :double x :double))))

(defun gamma (x)
  "Compute the Gamma function of x.
Signal domain-error if x < 0; Return Inf if x equals to 0 or a floating-point-overflow condition is signaled."
  (handler-case
      (cond ((zerop x) inf)
            (t (typecase x
                 (integer (factorial (- x 1)))
                 (float32 (%gamma/f32 x))
                 (float64 (%gamma/f64 x))
                 (rational (%gamma/f64 (float x 0d0)))
                 ;; (complex (exp (%lgamma/complex-f64
                 ;;                (let ((p (realpart x))
                 ;;                      (q (imagpart x)))
                 ;;                  (cy.sys:ensure-double-float (p q)
                 ;;                                              (complex p q))))))
                 (t (error 'domain-error :operation "Gamma" :expect "Number")))))
    (floating-point-overflow (c)
      (declare (ignore c))
      inf)))

(define-compiler-macro gamma (&whole form &environment env x)
  (cond ((constantp x env)
         (gamma (constant-form-value x env)))
        (t form)))

#+cffi
(defun %lgamma-r/f32 (x)
  (declare (type single-float x)
           (dynamic-extent x)
           (optimize speed (safety 1) (space 0)))
  (cffi:with-foreign-object (signp :int)
    (values (foreign-funcall "lgammaf_r" :float x :pointer signp :float)
            (cffi:mem-ref signp :int))))

#+cffi
(defun %lgamma-r/f64 (x)
  (declare (type double-float x)
           (dynamic-extent x)
           (optimize speed (safety 1) (space 0)))
  (cffi:with-foreign-object (signp :int)
    (values (foreign-funcall "lgamma_r" :double x :pointer signp :double)
            (cffi:mem-ref signp :int))))

(defun lgamma (x)
  (etypecase x
    (integer (log (factorial x)))
    (single-float (%lgamma-r/f32 x))
    (double-float (%lgamma-r/f64 x))    
    (real (%lgamma-r/f64 (coerce x 'double-float)))
    (complex (%lgamma/complex-f64
              (let ((p (realpart x))
                    (q (imagpart x)))
                (cy.sys:ensure-double-float (p q)
                  (complex p q)))))))

(defun lfact (x)
  (declare (dynamic-extent x)
           (optimize speed (safety 0) (space 0)))
  (typecase x
    (non-negative-integer
     (log (coerce (factorial x) 'double-float)))
    (t (error 'domain-error :operation "lfact"))))

(defmacro %evalpoly (x &rest ps)
  `(+ ,@(do ((res nil)
             (lst ps (cdr lst))
             (exp 0d0 (+ exp 1d0)))
            ((null lst) (nreverse res))
          (push `(* ,(car lst) (expt ,x ,exp))
                res))))

(defun lgamma-asymptotic (z)
  (declare (type (complex double-float) z)
           (dynamic-extent z)
           (optimize speed (safety 1) (space 0)))
  (let* ((zinv (/ #C(1d0 0d0) z))
         (%t (* zinv zinv)))
    (declare (type (complex double-float) zinv %t))
    ;; coefficients are bernoulli[2:n+1] .// (2*(1:n).*(2*(1:n) - 1))
    (+ (- (* (- z 0.5d0) (log z))
          z)
       9.1893853320467274178032927d-01
       (* zinv (%evalpoly (the (complex double-float) %t)
                          8.3333333333333333333333368d-02 -2.7777777777777777777777776d-03 
                          7.9365079365079365079365075d-04 -5.9523809523809523809523806d-04 
                          8.4175084175084175084175104d-04 -1.9175269175269175269175262d-03 
                          6.4102564102564102564102561d-03 -2.9550653594771241830065352d-02)))))

(defun %lgamma/complex-f64 (z)
  (declare (type (complex double-float) z)
           (dynamic-extent z)
           (optimize speed (safety 1) (space 0)))
  (let* ((x (realpart z))
         (y (imagpart z))
         (yabs (abs y))
         (w #C(0d0 0d0)))
    (declare (type double-float x y yabs)
             (type (complex double-float) w)
             (dynamic-extent w))
    (cond ((or (infinity-p x)
               (infinity-p y))
           (cond ((and (finity-p x) (infinity-p y))
                  ;; return Complex(x, x > 0 ? (y == 0 ? y : copysign(Inf, y)) : copysign(Inf, -y))
                  (complex x (if (plusp x)
                                 (if (zerop y)
                                     y
                                     (if (plusp y) inf64 -inf64))
                                 (if (plusp y) -inf64 inf64))))
                 ((and (infinity-p x) (finity-p y))
                  (complex -inf64 y))
                 (t (complex nan nan))))
          ((or (> x 7d0) (> yabs 7d0))
           (lgamma-asymptotic z))
          ((< x 0.1d0)
           ;; use reflection formula to transform to x > 0
           (if (and (zerop x) (zerop y))
               (complex inf 0)
               (- (complex 1.1447298858494001741434262
                           (- (* (if (plusp y)
                                     6.2831853071795864769252842d0
                                     -6.2831853071795864769252842d0)
                                 (floor (+ 0.25d0 (* 0.5d0 x))))))
                  (log (sin (* +pi+ z)))
                  (%lgamma/complex-f64 (- 1d0 z)))))
          (;; abs(x - 1) + yabs < 0.1
           (< (+ (abs (- x 1d0)) yabs) 0.1d0)
           ;; taylor series around zero at z=1
           ;; ... coefficients are [-eulergamma; [(-1)^k * zeta(k)/k for k in 2:15]]
           (setq w (complex (- x 1d0) y))
           (* w (%evalpoly w -5.7721566490153286060651188d-01 8.2246703342411321823620794d-01 
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
           (* w (%evalpoly w 4.2278433509846713939348812d-01 3.2246703342411321823620794d-01 
                           -6.7352301053198095133246196d-02 2.0580808427784547879000897d-02 
                           -7.3855510286739852662729527d-03 2.8905103307415232857531201d-03 
                           -1.1927539117032609771139825d-03 5.0966952474304242233558822d-04 
                           -2.2315475845357937976132853d-04 9.9457512781808533714662972d-05 
                           -4.4926236738133141700224489d-05 2.0507212775670691553131246d-05)))
          (t ;; use recurrence relation lgamma(z) = lgamma(z+1) - log(z) to shift to x > 7 for asymptotic series
           (let ((shiftprod (complex x yabs))
                 sb sb* 
                 (signflips 0)
                 (shift #C(0d0 0d0)))
             (declare (type (complex double-float) shiftprod shift)
                      (type (or t nil) sb sb*)
                      (type fixnum signflips)
                      (dynamic-extent sb sb* signflips shift))
             (incf x 1d0)
             (do ()
                 ((> x 7d0))
               (setq shiftprod (* shiftprod (complex x yabs)))
               (if (minusp (imagpart shiftprod))
                   (setq sb* t)
                   (setq sb* nil))
               (incf signflips (if (and sb* (not (eq sb sb*))) 1 0)) ; signflips += sb′ & (sb′ != sb)
               (setq sb sb*)
               (incf x 1d0))
             (setq shift (log shiftprod))
             (if (minusp y)
                 (setq shift (complex (realpart shift)
                                      (- (* signflips -6.2831853071795864769252842d0)
                                         (imagpart shift))))
                 (setq shift (complex (realpart shift)
                                      (+ (imagpart shift)
                                         (* signflips 6.2831853071795864769252842d0)))))
             (- (the (complex double-float) (lgamma-asymptotic (complex x y))) shift))))))
