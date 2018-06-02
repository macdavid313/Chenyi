;;;; hypot.lisp
(in-package #:chenyi.sys)

(declaim (inline %hypot/f64 hypot %hypot3/f64 hypot3))

(defun %hypot/f64 (x y)
  (declare (type double-float x y)
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
  (unless (and (numberp x) (numberp y))
    (error 'domain-error :operation "hypot" :expect "Number"))
  (handler-case 
      (cond ((and (typep x 'double-float) (typep y 'double-float))
             (%hypot/f64 x y))
            ((and (realp x) (realp y))
             (%hypot/f64 (float x 0d0) (float y 0d0)))
            (t (sqrt (+ (* x x) (* y y)))))
    (floating-point-overflow (c)
      (declare (ignore c))
      inf)))

(define-compiler-macro hypot (&whole form &environment env x y)
  (cond ((and (constantp x env) (constantp y env)
              (numberp x) (numberp y))
         (hypot x y))
        (t form)))

(defun %hypot3/f64 (x y z)
  (declare (type double-float x y z)
           (optimize speed (safety 0) (space 0)))
  (let ((xabs 0d0) (yabs 0d0)
        (zabs 0d0) (w 0d0))
    (declare (type double-float xabs yabs zabs w)
             (dynamic-extent xabs yabs zabs w))
    (setq xabs (abs x)
          yabs (abs y)
          zabs (abs z))
    (setq w (max xabs yabs zabs))
    (if (zerop w)
        0d0
        (* w (sqrt (+ (* (/ xabs w) (/ xabs w))
                      (* (/ yabs w) (/ yabs w))
                      (* (/ zabs w) (/ zabs w))))))))

(defun hypot3 (x y z)
  "It computes the value of sqrt(x^2 + y^2 + z^2) in a way that avoids overflow (when x and y are real numbers)."
  (unless (and (numberp x) (numberp y) (numberp z))
    (error 'domain-error :operation "hypot3" :expect "Number"))
  (handler-case
      (cond ((and (typep x 'double-float) (typep y 'double-float) (typep z 'double-float))
             (%hypot3/f64 x y z))
            ((and (realp x) (realp y) (realp z))
             (%hypot3/f64 (float x 0d0) (float y 0d0) (float z 0d0)))
            (t (sqrt (+ (* x x) (* y y) (* z z)))))
    (floating-point-overflow (c)
      (declare (ignore c))
      inf)))
  
(define-compiler-macro hypot3 (&whole form &environment env x y z)
  (cond ((and (constantp x env) (constantp y env) (constantp z env)
              (numberp x) (numberp y) (numberp z))
         (hypot3 x y z))
        (t form)))
