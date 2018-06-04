;;;; utils.lisp
(in-package #:chenyi.sys)

(declaim (inline eps))

;;;; The IF* macro placed in the public domain by John Foderaro. 
;;;; See: http://www.franz.com/~jkf/ifstar.txt
(defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
  (do ((xx (reverse args) (cdr xx))
       (state :init)
       (elseseen nil)
       (totalcol nil)
       (lookat nil nil)
       (col nil))
      ((null xx)
       (cond ((eq state :compl)
              `(cond ,@totalcol))
             (t (error "if*: illegal form ~s" args))))
    (cond ((and (symbolp (car xx))
                (member (symbol-name (car xx))
                        if*-keyword-list
                        :test #'string-equal))
           (setq lookat (symbol-name (car xx)))))

    (cond ((eq state :init)
           (cond (lookat (cond ((string-equal lookat "thenret")
                                (setq col nil
                                      state :then))
                               (t (error
                                   "if*: bad keyword ~a" lookat))))
                 (t (setq state :col
                          col nil)
                    (push (car xx) col))))
          ((eq state :col)
           (cond (lookat
                  (cond ((string-equal lookat "else")
                         (cond (elseseen
                                (error
                                 "if*: multiples elses")))
                         (setq elseseen t)
                         (setq state :init)
                         (push `(t ,@col) totalcol))
                        ((string-equal lookat "then")
                         (setq state :then))
                        (t (error "if*: bad keyword ~s"
                                  lookat))))
                 (t (push (car xx) col))))
          ((eq state :then)
           (cond (lookat
                  (error
                   "if*: keyword ~s at the wrong place " (car xx)))
                 (t (setq state :compl)
                    (push `(,(car xx) ,@col) totalcol))))
          ((eq state :compl)
           (cond ((not (string-equal lookat "elseif"))
                  (error "if*: missing elseif clause ")))
           (setq state :init)))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro ensure-double-float (vars &body body)
  "Take a list of varibles, ensure each to be of type double-float and throw an error if one is not of type real."
  `(let ,(mapcar (lambda (var)
                   (cond ((consp var) var)
                         (t `(,var (etypecase ,var
                                     (double-float ,var)
                                     (real (float ,var 0d0)))))))
                 vars)
     (declare ,@(mapcar (lambda (var) `(type double-float ,var)) vars)
              (optimize speed (safety 0) (space 0)))
     ,@body))

(defmacro ensure-consistent-complex-float (vars &body body)
  "Take a list of varibles, ensure each to be of type double-float and throw an error if one is not of type real."
  (alexandria:with-gensyms (r i)
    `(let ,(mapcar (lambda (var)
                     (cond ((consp var) var)
                           (t `(,var (typecase ,var
                                       ((complex single-float)
                                        ,`(the (complex single-float) ,var))
                                       ((complex double-float)
                                        ,`(the (complex double-float) ,var))
                                       (t (let ((,r (realpart ,var))
                                                (,i (imagpart ,var)))
                                            ,`(the (complex double-float)
                                                   (complex (float ,r 0d0)
                                                            (float ,i 0d0))))))))))
                   vars)
       (declare (optimize speed (safety 0) (space 0)))
       ,@body)))
                                                
(defun eps (&optional (proto 0d0))
  (typecase proto
    (float32 single-float-epsilon)
    (float64 double-float-epsilon)))

(defun inf (&optional (proto 1d0))
  (declare (type number proto)
           (optimize speed (safety 0) (space 0)))
  (typecase proto
    (float32 (if (plusp proto) inf32 -inf32))
    (float64 (if (plusp proto) inf -inf))
    (real (if (plusp proto) inf -inf))
    (complex/f32 (let ((r (realpart proto))
                       (i (imagpart proto)))
                   (declare (type float32 r i))
                   (complex (the float32 (inf r))
                            (the float32 (inf i)))))
    (complex/f64 (let ((r (realpart proto))
                       (i (imagpart proto)))
                   (declare (type float64 r i))
                   (complex (the float64 (inf r))
                            (the float64 (inf i)))))
    (complex (let ((r (realpart proto))
                   (i (imagpart proto)))
               (setq r (float r 0d0))
               (setq i (float i 0d0))
               (complex (the float64 (inf r))
                        (the float64 (inf i)))))))

(defun constant-form-value (form &optional env)
  #+allegro (sys:constant-value form env)
  #+ccl (ccl::eval-constant form)
  #+cmucl (eval:internal-eval form t env)
  #+ecl (ext:constant-form-value form env)
  #+(or abcl lispworks) (cl:eval (cl:macroexpand form env))
  #+sbcl (sb-int:constant-form-value form env))
