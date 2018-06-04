;;;; utils.lisp
(in-package #:chenyi.sys)

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
                                                
