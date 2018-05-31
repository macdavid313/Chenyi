;;;; log1p.lisp
(in-package #:chenyi.sys)

(declaim (inline log1p))

(defun %log1p/f64 (x)
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
  (typecase x
    (number (ensure-double-float (x)
              (%log1p/f64 x)))
    (t (error 'domain-error :operation "log1p"))))

(define-compiler-macro log1p (&whole form &environment env x)
  (cond ((constantp x env) (log1p x))
        (t form)))
