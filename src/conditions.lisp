;;;; conditions.lisp
(in-package #:chenyi)

(define-condition domain-error (arithmetic-error)
  ((expect :initarg :expect :type simple-string :initform nil
           :reader domain-error-expect))
  (:report (lambda (condition stream)
             (format stream "Domain Error signalled. ")
             (when (arithmetic-error-operation condition)
               (format stream "Operation was ~S. "
                       (arithmetic-error-operation condition)))
             (when (domain-error-expect condition)
               (format stream "~&This operation expects the domain: ~A."
                       (domain-error-expect condition))))))
