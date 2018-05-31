;;;; frexp-abcl.lisp
(in-package #:chenyi.sys)

(declaim (inline frexp))

(defun %frexp/f32 (x)
  (declare (type single-float x)
           (dynamic-extent x)
           (optimize speed (safety 0) (space 0)))
  (let* (($math (java:jclass "java.lang.Math"))
         ($float (java:jclass "float"))
         ($method (java:jmethod $math "getExponent" $float))
         exponent)
    (declare (dynamic-extent $math $float $method exponent))
    (setq exponent (+ 1 (java:jcall $method $math x)))
    (values (/ x (expt 2 exponent)) exponent)))

(defun %frexp/f64 (x)
  (declare (type double-float x)
           (dynamic-extent x)
           (optimize speed (safety 0) (space 0)))
  (let* (($math (java:jclass "java.lang.Math"))
         ($double (java:jclass "double"))
         ($method (java:jmethod $math "getExponent" $double))
         exponent)
    (declare (dynamic-extent $math $double $method exponent))
    (setq exponent (+ 1 (java:jcall $method $math x)))
    (values (/ x (expt 2 exponent)) exponent)))

(defun frexp (x)
  "This function splits the number x into its normalized fraction f and exponent e, such that x = f * 2^e and 0.5 <= f < 1."
  (declare (dynamic-extent x))
  (typecase x
    (single-float (%frexp/f32 x))
    (double-float (%frexp/f64 x))
    (real (%frexp/f64 (coerce x 'double-float)))
    (t (error 'domain-error :operation "frexp"))))

(define-compiler-macro frexp (&whole form &environment env x)
  (cond ((constantp x env)
         (typecase x
           (single-float `(%frexp/f32 ,x))
           (double-float `(%frexp/f64 ,x))
           (real `(%frexp/f64 ,(coerce x 'double-float)))
           (t (error 'domain-error :operation "frexp"))))
        (t form)))
