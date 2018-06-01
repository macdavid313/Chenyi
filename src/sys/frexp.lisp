;;;; frexp.lisp
(in-package #:chenyi.sys)

(declaim (inline frexp))

#+windows
(defun %frexp/f64 (x)
  (declare (type double-float x)
           (optimize speed (safety 0) (space 0)))
  (let ((mantissa 0d0) (exp 0))
    (declare (type double-float mantissa)
             (type fixnum exp)
             (dynamic-extent mantissa exp))
    (unless (zerop x)
      (setq exp (+ 1 (floor (log (abs x) 2d0)))))
    (setq mantissa (* x (expt 2d0 (- exp))))
    (values mantissa exp)))

#+(and cffi (not windows))
(defun %frexp/f64 (x)
  "This function splits the number x into its normalized fraction f and exponent e,
such that x = f * 2^e and 0.5 <= f < 1. It uses the C function from math.h"
  (declare (type double-float x)
           (optimize speed (safety 0) (space 0)))
  (cffi:with-foreign-object (exponent :int)
    (values (cffi:foreign-funcall "frexp" :double x :pointer exponent :double)
            (cffi:mem-ref exponent :int))))

(defun frexp (x)
  (declare (optimize speed (safety 0) (space 0)))
  (typecase x
    (double-float (%frexp/f64 x))
    (real (%frexp/f64 (coerce x 'double-float)))
    (t (error 'domain-error :operation "frexp"))))

(define-compiler-macro frexp (&whole form &environment env x)
  (cond ((constantp x env)
         (typecase x
           (double-float `(%frexp/f64 ,x))
           (real `(%frexp/f64 ,(coerce x 'double-float)))
           (t (error 'domain-error :operation "frexp"))))
        (t form)))
