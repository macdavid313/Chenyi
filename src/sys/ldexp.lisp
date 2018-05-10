;;;; ldexp.lisp
(in-package #:chenyi.sys)

(declaim (inline ldexp/f64))

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
