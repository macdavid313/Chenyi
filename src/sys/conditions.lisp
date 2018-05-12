;;;; conditions.lisp
(in-package #:chenyi.sys)

(define-condition domain-error (arithmetic-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Domain Error signalled. ")
             (when (arithmetic-error-operation condition)
               (format stream "Operation was ~S."
                       (arithmetic-error-operation condition))))))
