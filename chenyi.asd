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
  :depends-on (#:alexandria
               #:cl-reexport
               #-abcl #:cffi
               #-abcl #:trivial-download
               #-abcl #:trivial-extract)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "chenyi")))
               (:module "src/sys"
                :components
                ((:file "utils")
                 (:file "constants")
                 (:file "expm1")
                 (:file "log1p")
                 (:file "frexp")
                 (:file "ldexp")
                 (:file "hypot")
                 (:file "fcmp")))
               (:module "src/special"
                :components
                ((:file "gamma")
                 (:file "beta")))
               (:module "src/rng"
                :components               
                (#-abcl
                 (:module "libdSFMT"
                  :components     
                  ((:file "lib")
                   (:static-file "wrapper.c")
                   (:file "wrapper")
                   (:file "libdSFMT")))
                 (:file "rand48"))))
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

(eval-when (:load-toplevel :execute)
  (let ((type (lisp-implementation-type))
        (version (lisp-implementation-version)))
    #-(or abcl allegro ccl cmucl ecl lispworks sbcl)
    (error "Your lisp ~a-~a may not be well supported yet." type version)
    #+abcl
    (warn "Your lisp ~a-~a doesn't support dSFMT yet." type version)
    #+ecl
    (warn "Your lisp ~a-~a doesn't support NaN of type single-float yet." type version)))
