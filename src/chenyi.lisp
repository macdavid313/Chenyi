;;;; chenyi.lisp
(in-package #:chenyi)

(eval-when (:load-toplevel :execute)
  (dolist (pg '(chenyi.sys #|chenyi.special chenyi.rng|#))
    (if (eq pg 'chenyi.sys)
        (reexport-from pg :exclude '(#:if* #:while))
        (reexport-from pg))))
