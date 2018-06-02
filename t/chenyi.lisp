;;;; chenyi.lisp
(in-package #:cl-user)
(defpackage #:chenyi-test
  (:use #:cl #:chenyi #:prove)
  (:import-from #:cy.sys #:while))
(in-package #:chenyi-test)

#+(or allegro lispworks)
(setf *enable-colors* nil)

(defun test-rel (got expected relative-error &optional description)
  (let (status)
    (cond ((or (or (nan-p got) (nan-p expected))
               (or (infinity-p got) (infinity-p expected)))
           (setq status nil))
          ((or (< 0 expected least-positive-double-float)
               (< least-negative-double-float expected 0))
           (setq status t))
          ((not (zerop expected))
           (setq status (<= (/ (abs (- got expected))
                              (abs expected))
                           relative-error)))
          (t (setq status (<= (abs got) relative-error))))
    (ok status description)))
