#|
  This file is a part of Chenyi project.
  Copyright (c) 2018 David Gu (macdavid313@gmail.com)
|#

#|
  A Math Library for Common Lisp

  Author: David Gu (macdavid313@gmail.com)
|#

(in-package :cl-user)
(defpackage #:chenyi-asd
  (:use #:cl #:asdf))
(in-package #:chenyi-asd)

(defsystem #:chenyi
  :version "0.1"
  :author "David Gu"
  :license ""
  :depends-on (#:alexandria)
  :components ((:module "src"
                :components
                ((:file "chenyi")))
               (:module "src/elementary"
                :components
                ((:file "constants")
                 (:file "functions"))))
  :description "A Math Library for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op chenyi-test))))
