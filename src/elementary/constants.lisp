;;;; constants.lisp
(in-package #:chenyi)

(declaim (type single-float inf32 -inf32 nan32)
         (type double-float
               +e+ +pi+ π +euler+ +eulergamma+ γ
               +catalan+ +golden+ φ
               +log2e+ +log10e+
               +sqrt-1/2+ +sqrt-2+ +sqrt-3+ +sqrt-pi+
               +pi/2+ +pi/4+ +1/pi+ +2/pi+
               +ln2+ +ln10+ +ln-pi+
               inf64 -inf64 nan64 inf nan)
         (inline float-infinity-p nan-p))

(define-constant
    +e+ #.(exp 1d0) :test '=
    :documentation "The base of exponentials.")

(define-constant
    +pi+ 3.14159265358979323846d0 :test '=
    :documentation "The constant pi.")

(define-constant
    π 3.14159265358979323846d0 :test '=
    :documentation "The constant pi.")

(define-constant
    +euler+ 0.57721566490153286061d0 :test '=
    :documentation "Euler's constant.")

(define-constant
    +eulergamma+ 0.57721566490153286061d0 :test '=
    :documentation "Euler's constant.")

(define-constant
    γ 0.57721566490153286061d0 :test '=
    :documentation "Euler's constant.")

(define-constant
    +catalan+ 0.91596559417721901505d0 :test '=
    :documentation "Catalan's constant.")

(define-constant
    +golden+ 1.61803398874989484820d0 :test '=
    :documentation "The golden ratio.")

(define-constant
    φ 1.61803398874989484820d0 :test '=
    :documentation "The golden ratio.")

;;; some constants from GSL library
;;; https://www.gnu.org/software/gsl/doc/html/math.html#mathematical-constants
(define-constant
    +log2e+ #.(log (exp 1d0) 2d0) :test '=
    :documentation "The base-2 logarithm of e.")

(define-constant
    +log10e+ #.(log (exp 1d0) 10d0) :test '=
    :documentation "The base-10 logarithm of e.")

(define-constant
    +sqrt-2+ #.(sqrt 2d0) :test '=
    :documentation "The square root of two.")

(define-constant
    +sqrt-1/2+ #.(sqrt 0.5d0) :test '=
    :documentation "The square root of one-half.")

(define-constant
    +sqrt-3+ #.(sqrt 3d0) :test '=
    :documentation "The square root of three.")

(define-constant
    +pi/2+ #.(/ 3.14159265358979323846d0 2d0) :test '=
    :documentation "Pi divided by two.")

(define-constant
    +pi/4+ #.(/ 3.14159265358979323846d0 4d0) :test '=
    :documentation "Pi divided by four.")

(define-constant
    +sqrt-pi+ #.(sqrt 3.14159265358979323846d0) :test '=
    :documentation "The square root of pi.")

(define-constant
    +1/pi+ #.(/ 1d0 3.14159265358979323846d0) :test '=
    :documentation "The reciprocal of pi.")

(define-constant
    +2/pi+ #.(/ 2d0 3.14159265358979323846d0) :test '=
    :documentation "Twice the reciprocal of pi.")

(define-constant
    +ln10+ #.(log 10d0) :test '=
    :documentation "The natural logarithm of ten.")

(define-constant
    +ln2+ #.(log 2d0) :test '=
    :documentation "The natural logarithm of two.")

(define-constant
    +ln-pi+ #.(log 3.14159265358979323846d0) :test '=
    :documentation "The natural logarithm of pi.")

;;; Infinities and Not-a-number
#+lispworks
(progn 
  (declaim (inline %float-infinity-p))
  (defun %float-infinity-p (f)
    ;; FIXME!
    ;; this doesn't look right ...
    (etypecase f
      (single-float (or (= 1F++0 f) (= -1F++0 f)))
      (double-float (or (= 1D++0 f) (= -1F++0 f))))))

(defun float-infinity-p (f)
  (declare (type float f)
           #+lispworks (inline %float-infinity-p)
           (optimize speed (safety 0) (space 0)))
  (and (funcall #+abcl 'system::float-infinity-p
                #+allegro 'excl:infinityp
                #+ccl 'ccl::infinity-p
                #+(or cmucl ecl) 'ext:float-infinity-p
                #+lispworks '%float-infinity-p
                #+sbcl 'sb-ext:float-infinity-p f)
       t))

(defvar inf32
  #+(or abcl cmucl ecl) ext:single-float-positive-infinity
  #+allegro excl:*infinity-single*
  #+ccl (coerce ccl::double-float-positive-infinity 'single-float)
  #+lispworks +1F++0
  #+sbcl sb-kernel::single-float-positive-infinity
  "Positive infinity of type single-float.")

(defvar -inf32
  #+(or abcl cmucl ecl) ext:single-float-negative-infinity
  #+allegro excl:*negative-infinity-single*
  #+ccl (coerce ccl::double-float-negative-infinity 'single-float)
  #+lispworks -1F++0
  #+sbcl sb-kernel::single-float-negative-infinity
  "Negative infinity of type single-float.")

(defvar inf64
  #+(or abcl cmucl ecl) ext:double-float-positive-infinity
  #+allegro excl:*infinity-double*
  #+ccl ccl::double-float-positive-infinity
  #+lispworks +1D++0
  #+sbcl sb-kernel::double-float-positive-infinity
  "Positive infinity of type double-float.")

(defvar -inf64
  #+(or abcl cmucl ecl) ext:double-float-negative-infinity
  #+allegro excl:*negative-infinity-double*
  #+ccl ccl::double-float-negative-infinity
  #+lispworks -1D++0
  #+sbcl sb-kernel::double-float-negative-infinity
  "Negative infinity of type double-float.")

(defvar inf inf64 "Positive infinity of type double-float.")

#+ccl
(progn
  (declaim (inline %nan-p))
  (defun %nan-p (n)
    (and (not (float-infinity-p n))
         (ccl::nan-or-infinity-p n))))

(defun nan-p (n)
  (declare #+ccl (inline %nan-p)
           (optimize speed (safety 0) (space 0)))
  (and (funcall #+abcl 'system:float-nan-p
                #+allegro 'excl:nanp
                #+ccl '%nan-p
                #+(or cmucl ecl) 'ext:float-nan-p
                #+lispworks 'system::nan-p
                #+sbcl 'sb-ext:float-nan-p n)
       t))

(defvar nan32
    #+abcl (system:make-single-float #x7fc00000)
    #+allegro excl:*nan-single*
    #+ccl (coerce ccl::double-float-nan 'single-float)
    #+cmucl (kernel:make-single-float #x7fc00000)
    #+ecl (prog1 nil (warn "Your Lisp ~A-~A doesn't support the not-a-number value of type single-float natively. So nan32 has been set to nil."
                           (lisp-implementation-type) (lisp-implementation-version)))
    #+lispworks system::*single-float-nan*
    #+sbcl (sb-kernel::make-single-float #x7fc00000)
    "The not-a-number value of type single-float.")

(defvar nan64
    #+abcl (system:make-double-float #x7ff8000000000000)
    #+allegro excl:*nan-double*
    #+ccl ccl::double-float-nan
    #+cmucl (kernel:make-double-float #x7ff80000 0)
    #+ecl (ext:nan)
    #+lispworks system::*double-float-nan*
    #+sbcl (sb-kernel::make-double-float #x7ff80000 0)
    "The not-a-number value of type double-float.")

(defvar nan nan64 "The not-a-number value of type double-float.")
