;;;; types.lisp
(in-package #:chenyi)

(declaim (inline float32-p float64-p complex/f32-p complex/f64-p)
         (inline negative-fixnum-p non-postive-fixnum-p non-negative-fixnum-p positive-fixnum-p
                 negative-integer-p non-postive-integer-p non-negative-integer-p positive-integer-p
                 negative-rational-p non-postive-rational-p non-negative-rational-p positive-rational-p
                 ratiop ratio-plusp ratio-minusp
                 negative-ratio-p non-postive-ratio-p non-negative-ratio-p positive-ratio-p
                 negative-real-p non-postive-real-p non-negative-real-p positive-real-p
                 negative-float-p non-postive-float-p non-negative-float-p positive-float-p
                 negative-short-float-p non-postive-short-float-p non-negative-short-float-p positive-short-float-p
                 negative-single-float-p non-postive-single-float-p non-negative-single-float-p positive-single-float-p
                 negative-double-float-p non-postive-double-float-p non-negative-double-float-p positive-double-float-p
                 negative-long-float-p non-postive-long-float-p non-negative-long-float-p positive-long-float-p))

;;; Synonyms
(deftype float32 () 'single-float)
(defun float32-p (x) (typep x 'float32))
(deftype float64 () 'double-float)
(defun float64-p (x) (typep x 'float64))
(deftype complex/f32 () '(complex single-float))
(defun complex/f32-p (x) (typep x 'complex/f32))
(deftype complex/f64 () '(complex double-float))
(defun complex/f64-p (x) (typep x 'complex/f64))

;;; Sub-interval Numerical Types for Common Lisp
;;; https://common-lisp.net/project/cdr/document/5/extra-num-types.html
#|
  * negative-T
  * non-positive-T
  * non-negative-T
  * positive-T
  * array-index
  Where T is one of fixnum, integer, rational, ratio, real, float, short-float, single-float, double-float, long-float.
|#

;;; Fixnum
(deftype negative-fixnum () `(integer ,most-negative-fixnum -1))

(deftype non-positive-fixnum () `(integer ,most-negative-fixnum 0))

(deftype non-negative-fixnum () `(integer 0 , most-positive-fixnum))

(deftype positive-fixnum () `(integer 1 ,most-positive-fixnum))

(defun negative-fixnum-p (object)
  (typep object 'negative-fixnum))

(defun non-positive-fixnum-p (object)
  (typep object 'non-positive-fixnum))

(defun non-negative-fixnum-p (object)
  (typep object 'non-negative-fixnum))

(defun positive-fixnum-p (object)
  (typep object 'positive-fixnum))

;;; Integer
(deftype negative-integer () '(integer * -1))

(deftype non-positive-integer () '(integer * 0))

(deftype non-negative-integer () '(integer 0 *))

(deftype positive-integer () '(integer 1 *))

(defun negative-integer-p (object)
  (typep object 'negative-integer))

(defun non-positive-integer-p (object)
  (typep object 'non-positive-integer))

(defun non-negative-integer-p (object)
  (typep object 'non-negative-integer))

(defun positive-integer-p (object)
  (typep object 'positive-integer))

;;; Rational
(deftype negative-rational () '(rational * (0)))

(deftype non-positive-rational () '(rational * 0))

(deftype non-negative-rational () '(rational 0 *))

(deftype positive-rational () '(rational (0) *))

(defun negative-rational-p (object)
  (typep object 'negative-rational))

(defun non-positive-rational-p (object)
  (typep object 'non-positive-rational))

(defun non-negative-rational-p (object)
  (typep object 'non-negative-rational))

(defun positive-rational-p (object)
  (typep object 'positive-rational))

;;; Ratio
(defun ratiop (x)
  (and (rationalp x)
        (> (denominator x) 1)))

(defun ratio-plusp (x)
  (and (ratiop x) (plusp x)))

(defun ratio-minusp (x)
  (and (ratiop x) (minusp x)))


(deftype negative-ratio () '(satisfies ratio-minusp))

(deftype non-positive-ratio () 'negative-ratio)

(deftype non-negative-ratio () 'positive-ratio)

(deftype positive-ratio () '(satisfies ratio-plusp))

(defun negative-ratio-p (object)
  (typep object 'negative-ratio))

(defun non-positive-ratio-p (object)
  (typep object 'non-positive-ratio))

(defun non-negative-ratio-p (object)
  (typep object 'non-negative-ratio))

(defun positive-ratio-p (object)
  (typep object 'positive-ratio))


;;; Real
(deftype negative-real () '(real * (0)))

(deftype non-positive-real () '(real * 0))

(deftype non-negative-real () '(real 0 *))

(deftype positive-real () '(real (0) *))

(defun negative-real-p (object)
  (typep object 'negative-real))

(defun non-positive-real-p (object)
  (typep object 'non-positive-real))

(defun non-negative-real-p (object)
  (typep object 'non-negative-real))

(defun positive-real-p (object)
  (typep object 'positive-real))

;;; Float
;;; -- float
(deftype negative-float () '(float * (0.0e0)))

(deftype non-positive-float () '(float * 0.0e0))

(deftype non-negative-float () '(float 0.0e0 *))

(deftype positive-float () '(float (0.0e0) *))

(defun negative-float-p (object)
  (typep object 'negative-float))

(defun non-positive-float-p (object)
  (typep object 'non-positive-float))

(defun non-negative-float-p (object)
  (typep object 'non-negative-float))

(defun positive-float-p (object)
  (typep object 'positive-float))

;;; -- short-float
(deftype negative-short-float () '(short-float * (0.0s0)))

(deftype non-positive-short-float () '(short-float * 0.0s0))

(deftype non-negative-short-float () '(short-float 0.0s0 *))

(deftype positive-short-float () '(short-float (0.0s0) *))

(defun negative-short-float-p (object)
  (typep object 'negative-short-float))

(defun non-positive-short-float-p (object)
  (typep object 'non-positive-short-float))

(defun non-negative-short-float-p (object)
  (typep object 'non-negative-short-float))

(defun positive-short-float-p (object)
  (typep object 'positive-short-float))

;;; -- single-float
(deftype negative-single-float () '(single-float * (0.0f0)))

(deftype non-positive-single-float () '(single-float * 0.0f0))

(deftype non-negative-single-float () '(single-float 0.0f0 *))

(deftype positive-single-float () '(single-float (0.0f0) *))

(defun negative-single-float-p (object)
  (typep object 'negative-single-float))

(defun non-positive-single-float-p (object)
  (typep object 'non-positive-single-float))

(defun non-negative-single-float-p (object)
  (typep object 'non-negative-single-float))

(defun positive-single-float-p (object)
  (typep object 'positive-single-float))

;;; -- double-float
(deftype negative-double-float () '(double-float * (0.0d0)))

(deftype non-positive-double-float () '(double-float * 0.0d0))

(deftype non-negative-double-float () '(double-float 0.0d0 *))

(deftype positive-double-float () '(double-float (0.0d0) *))

(defun negative-double-float-p (object)
(typep object 'negative-double-float))

(defun non-positive-double-float-p (object)
  (typep object 'non-positive-double-float))

(defun non-negative-double-float-p (object)
  (typep object 'non-negative-double-float))

(defun positive-double-float-p (object)
  (typep object 'positive-double-float))

;;; -- long-float
(deftype negative-long-float () '(long-float * (0.0l0)))

(deftype non-positive-long-float () '(long-float * 0.0l0))

(deftype non-negative-long-float () '(long-float 0.0l0 *))

(deftype positive-long-float () '(long-float (0.0l0) *))

(defun negative-long-float-p (object)
  (typep object 'negative-long-float))

(defun non-positive-long-float-p (object)
  (typep object 'non-positive-long-float))

(defun non-negative-long-float-p (object)
  (typep object 'non-negative-long-float))

(defun positive-long-float-p (object)
  (typep object 'positive-long-float))

;;; Array-Index
(deftype array-index () `(integer 0 (,array-dimension-limit)))

(defun array-index-p (object)
  (typep object 'array-index))
