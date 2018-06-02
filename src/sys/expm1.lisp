;;;; expm1.lisp
(in-package #:chenyi.sys)

(declaim (inline %expm1/f64 expm1))

(defun %expm1/f64 (x)
  (declare (type double-float x)
           (optimize speed (safety 0) (space 0)))
  (cond ((< (abs x) +ln2+)
         ;; Compute the taylor series S = x + (1/2!) x^2 + (1/3!) x^3 + ...
         (let ((i 1d0)
               (term (/ x 1d0))
               (sum x))
           (declare (type double-float i term sum))
           (do ()
               ((<= (abs term) (* (abs sum) double-float-epsilon)) sum)
             (setq i (+ 1d0 i))
             (setq term (* term (/ x i)))
             (setq sum (+ sum term)))))
        (t (- (exp x) 1d0))))

(defun expm1 (x)
  "This function computes the value of exp(x) - 1 in a way that is accurate for small x."
  (unless (numberp x)
    (error 'domain-error :operation "expm1" :expect "Number"))
  (handler-case (typecase x
                  (double-float (%expm1/f64 x))
                  (real (%expm1/f64 (float x 0d0)))
                  (complex (- (exp x) 1)))
    (floating-point-overflow (c)
      (declare (ignore c))
      inf)))

(define-compiler-macro expm1 (&whole form &environment env x)
  (cond ((and (constantp x env) (numberp x))
         (expm1 x))
        (t form)))
