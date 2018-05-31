;;;; fcmp.lisp
;;; It is sometimes useful to be able to compare two floating point numbers approximately, to allow for rounding and truncation errors.
;;; Based on fcmp 1.2.2 Copyright (c) 1998-2000 Theodore C. Belding
;;; University of Michigan Center for the Study of Complex Systems
;;; Ted.Belding@umich.edu
(in-package #:chenyi.sys)

(declaim (inline fcmp)
         (type double-float *fcmp-epsilon*))

(defun f64cmp (x1 x2 epsilon)
  "This function determines whether x1 and x2 (must both be double-float) are approximately equal to a relative accuracy epsilon."
  (declare (type double-float x1 x2)
           (type real epsilon)
           (dynamic-extent x1 x2 epsilon)
           (optimize speed (safety 0) (space 0)))
  (let ((max 0d0)
        (exponent 0)
        (delta 0d0)
        (difference 0d0))
    (declare (type fixnum exponent)
             (type double-float max delta difference)
             (dynamic-extent max exponent delta difference))
    ;; Find exponent of largest absolute value
    (setq max (max (abs x1) (abs x2)))
    (multiple-value-bind (x exp) (frexp max)
      (declare (ignore x))
      (setq exponent exp))
    ;; Form a neighborhood of size  2 * delta
    (setq delta (ldexp epsilon exponent))
    (setq difference (- x1 x2))
    (cond ((> difference delta) 1)
          ((< difference (- delta)) -1)
          (t 0))))

(defvar *f64cmp-epsilon* double-float-epsilon "The default epsilon used by fcmp.")

(defun f64cmp< (number &rest more-numbers)
  "Return t if multiple double-floats are in monotonically increasing order, otherwise return nil. Note that it returns t if there is only one value."
  (flet ((cmp (x y epsilon)
           (= -1 (f64cmp x y epsilon))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp< nil)))
           finally (return t)))))

(defun f64cmp<= (number &rest more-numbers)
  "Return t if multiple double-floats are in monotonically nondecreasing order, otherwise return nil. Note that it returns t if there is only one value."
  (flet ((cmp (x y epsilon)
           (let ((res (f64cmp x y epsilon)))
             (or (= res -1) (zerop res)))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp<= nil)))
           finally (return t)))))

(defun f64cmp= (number &rest more-numbers)
  "Return t if multiple double-floats are approximately the same in value, otherwise return nil. Note that it returns t if there is only one value."
  (flet ((cmp (x y epsilon)
           (zerop (f64cmp x y epsilon))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp= nil)))
           finally (return t)))))

(defun f64cmp> (number &rest more-numbers)
  "Return t if multiple double-floats are in monotonically decreasing order, otherwise return nil. Note that it returns t if there is only one value."  
  (flet ((cmp (x y epsilon)
           (= 1 (f64cmp x y epsilon))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp> nil)))
           finally (return t)))))

(defun f64cmp>= (number &rest more-numbers)
  "Return t if multiple double-floats are in monotonically nonincreasing order, otherwise return nil. Note that it returns t if there is only one value."    
  (flet ((cmp (x y epsilon)
           (let ((res (f64cmp x y epsilon)))
             (or (= res 1) (zerop res)))))
    (if (null more-numbers)
        t
        (loop for lst = (cons number more-numbers) then (cdr lst)
           while (cdr lst) do
             (let ((x (first lst))
                   (y (second lst)))
               (declare (type double-float x y))
               (unless (cmp x y *f64cmp-epsilon*)
                 (return-from f64cmp>= nil)))
           finally (return t)))))
