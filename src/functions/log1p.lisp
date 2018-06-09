;;;; log1p.lisp
(in-package #:chenyi)

#+(and cffi (or darwin linux))
(progn
  (defun %log1p/f32 (x)
    (declare (type float32 x)
             (optimize speed (safety 0) (space 0)))
    (cffi:foreign-funcall "log1pf" :float x :float))
  
  (defun %log1p/f64 (x)
    (declare (type float64 x)
             (optimize speed (safety 0) (space 0)))
    (cffi:foreign-funcall "log1p" :double x :double))

  (defun log1p (x)
    "This function computes the value of log(1+x)"
    (cond ((and (numberp x) (= x -1)) -inf)
          (t (typecase x
               (float32 (%log1p/f32 x))
               (float64 (%log1p/f64 x))
               (rational (%log1p/f64 (float x 0d0)))
               ((complex rational) (let* ((r (realpart x))
                                          (i (imagpart x))
                                          (x (complex (float r 0d0)
                                                      (float i 0d0))))
                                     (log (+ 1 x))))
               (complex (log (+ x 1)))
               (t (error 'domain-error :operation "log1p" :expect "Number"))))))
  ) ;; end of progn

#-(and cffi (or darwin linux))
(progn
  (declaim (inline %log1p/float))
  (defun %log1p/float (x)
    (declare (type float x)
             (optimize speed (safety 1) (space 0)))
    (cond ((= x -1) (inf x))
          ((< x -1) (the (complex float) (log (+ x 1))))
          (t (let* ((y (+ x 1))
                    (z (- y 1)))
               (declare (dynamic-extent y z))
               ;; cancels errors with IEEE arithmetic
               (- (log y) (/ (- z x) y))))))
  
  (defun log1p (x)
    "This function computes the value of log(1+x) in a way that is accurate for small x."
    (cond ((and (numberp x) (= x -1)) -inf)
          (t (typecase x
               ((or float32 float64) (%log1p/float x))
               (rational (%log1p/float (float x 0d0)))
               ((complex rational) (let* ((r (realpart x))
                                          (i (imagpart x))
                                          (x (complex (float r 0d0)
                                                      (float i 0d0))))
                                     (log (+ 1 x))))
               (complex (log (+ x 1)))
               (t (error 'domain-error :operation "log1p" :expect "Number"))))))
  ) ;; end of progn

(define-compiler-macro log1p (&whole form &environment env x)
  (cond ((constantp x env)
         (log1p (constant-form-value x env)))
        (t form)))
