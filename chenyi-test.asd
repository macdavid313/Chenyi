#|
  This file is a part of Chenyi project.
  Copyright (c) 2018 David Gu (macdavid313@gmail.com)
|#

#|
  A Math Library for Common Lisp

  Author: David Gu (macdavid313@gmail.com)
|#

(in-package :cl-user)
(defpackage #:chenyi-test-asd
  (:use #:cl #:asdf))
(in-package #:chenyi-test-asd)

(defsystem chenyi-test
  :author "David Gu"
  :license ""
  :depends-on (#:chenyi
               #:prove)
  :components ((:module "t"
                :components
                ((:file "chenyi")
                 (:test-file "sys"))))
  :description "Test system for chenyi"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
