;;;; ldexp.lisp
(in-package #:chenyi.sys)

(declaim (inline %ldexp/f64 ldexp))

#+windows
(defun %ldexp/f64 (x exponent)
  ;; FIXME: this probably needs to be updated into a more sophisticated implementation
  (declare (type double-float x)
           (type fixnum exponent)
           (dynamic-extent x exponent)
           (optimize speed (safety 0) (space 0)))
  (* x (expt 2d0 exponent)))

#+(and cffi (not windows))
(defun %ldexp/f64 (x exponent)
  (declare (type double-float x)
           (type fixnum exponent)
           (optimize speed (safety 0) (space 0)))
  (cffi:foreign-funcall "ldexp" :double x :int exponent :double))

(defun ldexp (x exponent)
  "This function computes the value of x * 2 ^ exponent."
  (unless (typep exponent 'fixnum)
    (error 'domain-error :operation "ldexp"))
  (if (zerop x)
      0d0
      (handler-case (typecase x
                      (double-float (%ldexp/f64 x exponent))
                      (real (%ldexp/f64 (coerce x 'double-float) exponent))
                      (t (error 'domain-error :operation "ldexp")))
        (floating-point-overflow (c)
          (declare (ignore c))
          (if (plusp x) inf -inf)))))
  
(define-compiler-macro ldexp (&whole form &environment env x exponent)
  (cond ((and (constantp x env) (constantp exponent))
         (ldexp x exponent))
        (t form)))
