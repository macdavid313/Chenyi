;;;; macro.lisp
(in-package #:chenyi)

(defmacro ensure-double-float (vars &body body)
  `(let ,(mapcar (lambda (var)
                   `(,var (etypecase ,var
                            (double-float ,var)
                            (real (coerce ,var 'double-float)))))
                 vars)
     ,@body))
