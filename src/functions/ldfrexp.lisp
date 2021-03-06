;;;; ldfrexp.lisp
(in-package #:chenyi)

#+(and cffi (or darwin linux))
(progn
  (defun frexp (x)
    (cffi:with-foreign-object (e :int)
      (typecase x
        (float32 (values (cffi:foreign-funcall "frexpf" :float x :pointer e :float)
                         (cffi:mem-ref e :int)))
        (float64 (values (cffi:foreign-funcall "frexp" :double x :pointer e :double)
                         (cffi:mem-ref e :int)))
        (real (values (cffi:foreign-funcall "frexp" :double (float x 0d0) :pointer e :double)
                      (cffi:mem-ref e :int)))
        (t (error 'domain-error :operation "frexp" :expect "Real")))))

  (defun ldexp (x e)
    (declare (type (integer #.(- (expt 2 32)) #.(- (expt 2 32) 1)) e))
    (typecase x
      (float32 (cffi:foreign-funcall "ldexpf" :float x :int e :float))
      (float64 (cffi:foreign-funcall "ldexp" :double x :int e :double))
      (real (cffi:foreign-funcall "ldexp" :double (float x 0d0) :int e :double))
      (t (error 'domain-error :operation "ldexp" :expect "Real"))))
  ) ;; end of progn

;;; FIXME: frexp needs to be reviewed!
#-(and cffi (or darwin linux))
(progn
  (declaim (inline %ldexp/f64 ldexp %frexp/f64 frexp)
           (type fixnum %max-exp% %min-exp%))
  ;; (define-constant %max-exp% #.(ceiling (log most-positive-double-float 2d0)) :test '=)
  ;; (define-constant %min-exp% #.(+ 1 (ceiling (log least-positive-double-float 2d0))) :test '=)
  (define-constant %max-exp% 1024 :test '=)
  (define-constant %min-exp% -1021 :test '=)
  
  (defun %frexp/f64 (x)
    (declare (type double-float x)
             (optimize speed (safety 0) (space 0)))
    (let ((xabs 0d0))
      (declare (type double-float xabs)
               (dynamic-extent xabs))
      (setq xabs (abs x))
      (cond ((zerop x) (values x 0))
            ((or (infinity-p x) (nan-p x)) (values x 0))
            ((and (>= xabs 0.5d0) (< xabs 1d0))
             (values x 0))
            (t (let ((ei 0) (f 0d0))
                 (declare (type (integer -2000 2000) ei)
                          (type double-float f)
                          (dynamic-extent ei f))
                 (setq ei (coerce (ceiling (log xabs 2d0)) 'integer))
                 ;; Prevent underflow and overflow of 2^(-ei), viz. ei < 0 && ei <= max || ei > 0 && ei >= min
                 (when (< ei %min-exp%) (setq ei %min-exp%))
                 (when (> ei (- %min-exp%)) (setq ei (- %min-exp%)))
                 (setq f (handler-case (* x (expt 2d0 (- ei)))
                           (floating-point-overflow (c)
                             (declare (ignore c))
                             (if (minusp x) -inf inf))))
                 (cond ((infinity-p f) (values f 0)) ;; <-- this should not happen
                       (t (while (>= (abs f) 1d0)
                            (incf ei)
                            (setq f (/ f 2d0)))
                          (while (< 0d0 (abs f) 0.5d0)
                            (decf ei)
                            (setq f (* f 2d0)))
                          (values f ei))))))))

  (defun frexp (x)
    "This function splits the number x into its normalized fraction f and exponent e, such that x = f * 2^e and 0.5 <= f < 1."
    (typecase x
      (double-float (%frexp/f64 x))
      (real (%frexp/f64 (float x 0d0)))
      (t (error 'domain-error :operation "frexp" :expect "Real"))))

  (defun %ldexp/f64 (x exponent)
    (declare (type double-float x)
             (type integer exponent)
             (optimize speed (safety 0) (space 0)))
    (if (zerop x)
        x
        (let ((y 0d0) (ex 0)
              (e2 0))
          (declare (type double-float y)
                   (type (integer -2000 2000) ex)
                   (type integer e2)
                   (dynamic-extent y ex e2))
          (multiple-value-setq (y ex) (%frexp/f64 x))
          (setq e2 (+ exponent ex))
          (cond ((>= e2 %max-exp%)
                 (setq y (* y (expt 2d0 (+ 1 (- e2 %max-exp%))))
                       e2 (- %max-exp% 1)))
                ((<= e2 %min-exp%)
                 (setq y (expt 2d0 (- e2 %min-exp% 1))
                       e2 (+ 1 %min-exp%))))
          (* y (expt 2d0 e2)))))

  (defun ldexp (x exponent)
    "This function computes the value of x * 2 ^ exponent."
    (unless (and (realp x) (integerp exponent))
      (error 'domain-error :operation "ldexp" "Real and Integer"))
    (if (zerop x)
        0d0
        (handler-case (typecase x
                        (double-float (%ldexp/f64 x exponent))
                        (real (%ldexp/f64 (float x 0d0) exponent)))
          (floating-point-overflow (c)
            (declare (ignore c))
            (if (plusp x) inf -inf)))))
  ) ;; end of progn

(define-compiler-macro frexp (&whole form &environment env x)
  (cond ((constantp x env)
         `(frexp ,(constant-form-value x env)))
        (t form)))

(define-compiler-macro ldexp (&whole form &environment env x e)
  (cond ((and (constantp x env) (constantp e env))
         (ldexp (constant-form-value x env) (constant-form-value e env)))
        (t form)))
