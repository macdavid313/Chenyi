;;;; dSFMT.lisp
(in-package #:chenyi.rng)

(defvar *project-rng-path*
  (merge-pathnames "src/rng/" (asdf:system-source-directory 'chenyi)))

(defvar *dsfmt-zip-path*
  (merge-pathnames "dSFMT-src.zip" *project-rng-path*))

(defvar *dsfmt-path*
  (merge-pathnames "dSFMT/" *project-rng-path*))

(defvar *dsfmt-src-url*
  "http://www.math.sci.hiroshima-u.ac.jp/%7Em-mat/MT/SFMT/dSFMT-src-2.2.3.zip")

(defun download-dsfmt-src-zip ()
  (download *dsfmt-src-url* *dsfmt-zip-path*))

(defmacro with-dsfmt-zip (&body body)
  `(unwind-protect (progn (download-dsfmt-src-zip)
                          ,@body)
     (uiop:delete-file-if-exists *dsfmt-zip-path*)))

(defun prepare-dsfmt-src ()
  (with-dsfmt-zip
    (when (uiop:directory-exists-p *dsfmt-path*)
      (uiop:delete-directory-tree *dsfmt-path* :validate t :if-does-not-exist :ignore))
    (extract-zip *dsfmt-zip-path*)
    (rename-file (merge-pathnames "dSFMT-src-2.2.3/" *project-rng-path*)
                 *dsfmt-path*)
    (format t "dSFMT 2.2.3 has been successfully downloaded.~%")))
