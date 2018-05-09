;;;; functions
(in-package #:chenyi)

(declaim (inline log1p expm1 frexp/f64 ldexp/f64 fcmp hypot hypot3)
         (type double-float *fcmp-epsilon*))

;;; log1p
(defun log1p/f64 (x)
  (declare (type double-float x)
           (dynamic-extent x)
           (optimize speed (safety 0) (space 0)))
  (let ((y 0d0) (z 0d0))
    (declare (type double-float y z)
             (dynamic-extent y z))
    (setq y (+ x 1d0))
    (setq z (- y 1d0))
    ;; cancels errors with IEEE arithmetic
    (- (log y) (/ (- z x) y))))

(defun log1p (x)
  "This function computes the value of log(1+x) in a way that is accurate for small x."
  (declare (type number x))
  (cond ((typep x 'real)
         (ensure-double-float (x) (log1p/f64 x)))
        (t (log (+ x 1)))))

;;; expm1
(defun expm1/f64 (x)
  (declare (type double-float x)
           (dynamic-extent x)
           (optimize speed (safety 0) (space 0)))
  (if (< (abs x) +ln2+)
      ;; Compute the taylor series S = x + (1/2!) x^2 + (1/3!) x^3 + ...
      (let ((i 1d0)
            (term (/ x 1d0))
            (sum x))
        (declare (type double-float i term sum))
        (do ()
            ((<= (abs term) (* (abs sum) double-float-epsilon)) sum)
          (setq i (+ 1d0 i))
          (setq term (* term (/ x i)))
          (setq sum (+ sum term))))
      (- (exp x) 1d0)))

(defun expm1 (x)
  "This function computes the value of exp(x) - 1 in a way that is accurate for small x."
  (declare (type number x))
  (cond ((typep x 'real)
         (ensure-double-float (x) (expm1/f64 x)))
        (t (- (exp x) 1))))

;;; hypot
(defun hypot/f64 (x y)
  (declare (type double-float x y)
           (dynamic-extent x y)
           (optimize speed (safety 0) (space 0)))
  (let ((xabs 0d0) (yabs 0d0)
        (min 0d0) (max 0d0)
        (u 0d0))
    (declare (type double-float xabs yabs min max u)
             (dynamic-extent xabs yabs min max u))
    (if (or (float-infinity-p x) (float-infinity-p y))
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
  (declare (type number x y))
  (let ((lst (list x y)))
    (declare (dynamic-extent lst))
    (if (every (lambda (x) (typep x 'real)) lst)
        (ensure-double-float (x y)
          (hypot/f64 x y))
        (sqrt (+ (* x x) (* y y))))))

(defun hypot3/f64 (x y z)
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
  (declare (type number x y z))
  (let ((lst (list x y z)))
    (declare (dynamic-extent lst))
    (if (every (lambda (x) (typep x 'real)) lst)
        (ensure-double-float (x y z)
          (hypot3/f64 x y z))
        (sqrt (+ (* x x) (* y y) (* z z))))))      

#+abcl
(defun frexp/f32 (x)
  (declare (type single-float x))
  (let* (($math (java:jclass "java.lang.Math"))
         ($float (java:jclass "float"))
         ($method (java:jmethod $math "getExponent" $float))
         exponent)
    (declare (dynamic-extent $math $float $method exponent))
    (setq exponent (+ 1 (java:jcall $method $math x)))
    (values (/ x (expt 2 exponent)) exponent)))

#+abcl
(defun frexp/f64 (x)
  "This function splits the number x into its normalized fraction f and exponent e,
such that x = f * 2^e and 0.5 <= f < 1. It calls the static function java.lang.Math.getExponent, therefore, as shown in the signature, x could be either single-float or double-float."
  (declare (type float x))
  (let* (($math (java:jclass "java.lang.Math"))
         ($double (java:jclass "double"))
         ($method (java:jmethod $math "getExponent" $double))
         exponent)
    (declare (dynamic-extent $math $double $method exponent))
    (setq exponent (+ 1 (java:jcall $method $math x)))
    (values (/ x (expt 2 exponent)) exponent)))

#-abcl
(defun frexp/f64 (x)
  "This function splits the number x into its normalized fraction f and exponent e,
such that x = f * 2^e and 0.5 <= f < 1. It uses the C function from math.h"
  (declare (type double-float x))
  (cffi:with-foreign-object (exponent :int)
    (values (cffi:foreign-funcall "frexp" :double x :pointer exponent :double)
            (cffi:mem-ref exponent :int))))

#+abcl
(defun ldexp/f64 (x exponent)
  "This function computes the value of x * 2 ^ exponent."
  ;; FIXME: this needs to be updated into a proper implementation
  (declare (type double-float x)
           (type fixnum exponent))
  (* x (expt 2d0 exponent)))

#-abcl
(defun ldexp/f64 (x exponent)
  "This function computes the value of x * 2 ^ exponent. It uses the C function from math.h"
  (declare (type double-float x)
           (type fixnum exponent))
  (cffi:foreign-funcall "ldexp" :double x :int exponent :double))

;;; It is sometimes useful to be able to compare two floating point numbers approximately, to allow for rounding and truncation errors.
;;; Based on fcmp 1.2.2 Copyright (c) 1998-2000 Theodore C. Belding
;;; University of Michigan Center for the Study of Complex Systems
;;; Ted.Belding@umich.edu
(defun f64cmp (x1 x2 epsilon)
  "This function determines whether x1 and x2 (must both be double-float) are approximately equal to a relative accuracy epsilon."
  (declare (type double-float x1 x2)
           (type real epsilon)
           (dynamic-extent x1 x2 epsilon)
           (optimize speed (safety 0) (space 0)))
  (let ((max 0d0)
        (exponent 0)
        (delta 0d0)
        (difference 0d0))
    (declare (type fixnum exponent)
             (type double-float max delta difference)
             (dynamic-extent max exponent delta difference))
    ;; Find exponent of largest absolute value
    (setq max (max (abs x1) (abs x2)))
    (multiple-value-bind (x exp) (frexp/f64 max)
      (declare (ignore x))
      (setq exponent exp))
    ;; Form a neighborhood of size  2 * delta
    (setq delta (ldexp/f64 epsilon exponent))
    (setq difference (- x1 x2))
    (cond ((> difference delta) 1)
          ((< difference (- delta)) -1)
          (t 0))))

(defvar *f64cmp-epsilon* double-float-epsilon "The default epsilon used by fcmp.")

(defun f64cmp< (number &rest more-numbers)
  "Return t if multiple double-floats are in monotonically increasing order, otherwise return nil. Note that it returns t if there is only one value."
  (flet ((cmp (x y epsilon)
           (= -1 (f64cmp x y epsilon))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp< nil)))
           finally (return t)))))

(defun f64cmp<= (number &rest more-numbers)
  "Return t if multiple double-floats are in monotonically nondecreasing order, otherwise return nil. Note that it returns t if there is only one value."
  (flet ((cmp (x y epsilon)
           (let ((res (f64cmp x y epsilon)))
             (or (= res -1) (zerop res)))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp<= nil)))
           finally (return t)))))

(defun f64cmp= (number &rest more-numbers)
  "Return t if multiple double-floats are approximately the same in value, otherwise return nil. Note that it returns t if there is only one value."
  (flet ((cmp (x y epsilon)
           (zerop (f64cmp x y epsilon))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp= nil)))
           finally (return t)))))

(defun f64cmp> (number &rest more-numbers)
  "Return t if multiple double-floats are in monotonically decreasing order, otherwise return nil. Note that it returns t if there is only one value."  
  (flet ((cmp (x y epsilon)
           (= 1 (f64cmp x y epsilon))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp> nil)))
           finally (return t)))))

(defun f64cmp>= (number &rest more-numbers)
  "Return t if multiple double-floats are in monotonically nonincreasing order, otherwise return nil. Note that it returns t if there is only one value."    
  (flet ((cmp (x y epsilon)
           (let ((res (f64cmp x y epsilon)))
             (or (= res 1) (zerop res)))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp>= nil)))
           finally (return t)))))
