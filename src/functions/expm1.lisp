;;;; expm1.lisp
(in-package #:chenyi)

#+(and cffi (or darwin linux))
(progn
  (declaim (inline %expm1/f32 %expm1/f64))
  
  (defun %expm1/f32 (x)
    (declare (type float32 x)
             (optimize speed (safety 0) (space 0)))
    (cffi:foreign-funcall "expm1f" :float x :float))
  
  (defun %expm1/f64 (x)
    (declare (type float64 x)
             (optimize speed (safety 0) (space 0)))
    (cffi:foreign-funcall "expm1" :double x :double))
  
  (defun expm1 (x)
    "Accurately compute e^x - 1"
    (handler-case
        (cond ((or (nan-p x) (infinity-p x)) x)
              (t (typecase x
                   (float32 (%expm1/f32 x))
                   (float64 (%expm1/f64 x))
                   (rational (%expm1/f64 (float x 0d0)))
                   (complex/rational (let* ((r (realpart x))
                                            (i (imagpart x))
                                            (x (complex (float r 0d0)
                                                        (float i 0d0))))
                                       (- (exp x) 1)))
                   (complex (- (exp x) 1))
                   (t (error 'domain-error :operation "expm1" :expect "Number")))))
      (floating-point-overflow (c)
        (declare (ignore c))
        (typecase x
          (real (inf x))
          (complex/rational (inf (cis (float (imagpart x) 0d0))))
          (complex (inf (cis (imagpart x))))))))
  ) ;; end of progn

#-(and cffi (or darwin linux))
(progn
  (defun %expm1/float (x)
    (declare (type float x)
             (optimize speed (safety 1) (space 0)))
    (declare (type float one)
             (dynamic-extent one))
    (cond ((< (abs x) (float #.(log 2) x))
           (do* ((i 1 (+ i 1))
                 (term (/ x 1) (* term (/ x i)))
                 (sum x (+ sum term)))
                ((<= (abs term) (* (abs sum) (eps x)))
                 sum)
             (declare (type float i term sum)
                      (dynamic-extent i term sum))))
          (t (- (exp x) 1))))
  
  (defun expm1 (x)
    "Accurately compute e^x - 1"
    (handler-case (typecase x
                    ((or float32 float64) (%expm1/float x))
                    (rational (%expm1/float (float x 0d0)))
                    (complex/rational (let* ((r (realpart x))
                                               (i (imagpart x))
                                               (x (complex (float r 0d0)
                                                           (float i 0d0))))
                                          (- (exp x) 1)))
                    (complex (- (exp x) 1))
                    (t (error 'domain-error :operation "expm1" :expect "Number")))
      (floating-point-overflow (c)
        (declare (ignore c))
        (typecase x
          (real (inf x))
          (complex/rational (inf (cis (float (imagpart x) 0d0))))
          (complex (inf (cis (imagpart x))))))))
  ) ;; end of progn
                             
(define-compiler-macro expm1 (&whole form &environment env x)
  (cond ((constantp x env)
         (expm1 (constant-form-value x env)))
        (t form)))
