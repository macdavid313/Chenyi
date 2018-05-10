;;;; frexp.lisp
(in-package #:chenyi.sys)

(declaim (inline frexp/64)
         #+abcl (inline frexp/f32))

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
