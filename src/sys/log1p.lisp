;;;; log1p.lisp
(in-package #:chenyi.sys)

(declaim (inline %log1p/f64 log1p))

(defun %log1p/f64 (x)
  (declare (type double-float x)
           (optimize speed (safety 0) (space 0)))
  (let ((y 0d0) (z 0d0))
    (declare (type double-float y z)
             (dynamic-extent y z))
    (setq y (+ x 1d0))
    (setq z (- y 1d0))
    ;; cancels errors with IEEE arithmetic
    (- (the double-float (log y)) (/ (- z x) y))))

(defun log1p (x)
  "This function computes the value of log(1+x) in a way that is accurate for small x."
  (unless (numberp x)
    (error 'domain-error :operation "log1p" :expect "Number"))
  (cond ((= x -1) -inf)
        ((and (typep x 'double-float) (> x -1d0))
         (%log1p/f64 x))
        ((and (typep x 'real) (> x -1))
         (%log1p/f64 (float x 0d0)))
        (t (log (+ x 1)))))

(define-compiler-macro log1p (&whole form &environment env x)
  (cond ((and (constantp x env) (numberp x))
         (log1p x))
        (t form)))
