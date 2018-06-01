;;;; hypot.lisp
(in-package #:chenyi.sys)

(declaim (inline hypot hypot3))

(defun %hypot/f64 (x y)
  (declare (type double-float x y)
           (dynamic-extent x y)
           (optimize speed (safety 0) (space 0)))
  (let ((xabs 0d0) (yabs 0d0)
        (min 0d0) (max 0d0)
        (u 0d0))
    (declare (type double-float xabs yabs min max u)
             (dynamic-extent xabs yabs min max u))
    (if (or (infinity-p x) (infinity-p y))
        inf
        (progn
          (setq xabs (abs x))
          (setq yabs (abs y))
          (if (< xabs yabs)
              (progn (setq min xabs)
                     (setq max yabs))
              (progn (setq min yabs)
                     (setq max xabs)))
          (if (zerop min)
              max
              (progn
                (setq u (/ min max))
                (* max (sqrt (+ 1d0 (* u u))))))))))

(defun hypot (x y)
  "It computes the value of sqrt(x^2 + y^2) in a way that avoids overflow (when x and y are real numbers)."
  (declare (optimize speed))
  (cond ((and (typep x 'real) (typep y 'real))
         (ensure-double-float (x y)
           (declare (type double-float x y))
           (%hypot/f64 x y)))
        ((and (typep x 'number) (typep y 'number))
         (sqrt (+ (* x x) (* y y))))
        (t (error 'domain-error :operation "hypot" :expect "Number"))))

(define-compiler-macro hypot (&whole form &environment env x y)
  (cond ((and (constantp x env) (constantp y env)
              (numberp x) (numberp y))
         (hypot x y))
        (t form)))

(defun %hypot3/f64 (x y z)
  (declare (type double-float x y z)
           (dynamic-extent x y z)
           (optimize speed (safety 0) (space 0)))
  (let ((xabs (abs x)) (yabs (abs y))
        (zabs (abs z)) (w 0d0))
    (declare (type double-float xabs yabs zabs w)
             (dynamic-extent w))
    (setq w (max xabs yabs zabs))
    (if (zerop w)
        0d0
        (* w (sqrt (+ (* (/ xabs w) (/ xabs w))
                      (* (/ yabs w) (/ yabs w))
                      (* (/ zabs w) (/ zabs w))))))))

(defun hypot3 (x y z)
  "It computes the value of sqrt(x^2 + y^2 + z^2) in a way that avoids overflow (when x and y are real numbers)."
  (declare (optimize speed))
  (cond ((and (typep x 'real) (typep y 'real) (typep z 'real))
         (ensure-double-float (x y z)
           (%hypot3/f64 x y z)))
        ((and (typep x 'number) (typep y 'number) (typep z 'number))
         (sqrt (+ (* x x) (* y y) (* z z))))
        (t (error 'domain-error :operation "hypot3" :expect "Number"))))

(define-compiler-macro hypot3 (&whole form &environment env x y z)
  (cond ((and (constantp x env) (constantp y env) (constantp z env)
              (numberp x) (numberp y) (numberp z))
         (hypot3 x y z))
        (t form)))
