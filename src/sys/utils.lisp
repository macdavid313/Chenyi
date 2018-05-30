;;;; utils.lisp
(in-package #:chenyi.sys)

(defmacro ensure-double-float (vars &body body)
  "Take a list of varibles, ensure each to be of type double-float and throw an error if one is not of type real."
  (alexandria:with-gensyms (r i)
    `(let ,(mapcar (lambda (var)
                     `(,var (etypecase ,var
                              (double-float ,var)
                              (real (coerce ,var 'double-float))
                              (complex (let ((,r (realpart ,var))
                                             (,i (imagpart ,var)))
                                         (complex (coerce ,r 'double-float)
                                                  (coerce ,i 'double-float)))))))
                   vars)
       ,@body)))
