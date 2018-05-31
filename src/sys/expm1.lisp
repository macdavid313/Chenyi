;;;; expm1.lisp
(in-package #:chenyi.sys)

(declaim (inline expm1))

(defun %expm1/f64 (x)
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
  (typecase x
    (number (ensure-double-float (x)
              (typecase x
                (real (%expm1/f64 x))
                (t (- (exp x) 1)))))
    (t (error 'domain-error :operation "expm1"))))

(define-compiler-macro expm1 (&whole form &environment env x)
  (cond ((constantp x env) (expm1 x))
        (t form)))
