;;;; dSFMT.lisp
(in-package #:chenyi.rng.dsfmt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %project-root-path% (asdf:system-source-directory 'chenyi))

  (defvar %dsfmt-zip-path%)
  
  (defvar %dsfmt-src-path% (merge-pathnames "dSFMT/" %project-root-path%)
    "The location where source code of dSFMT has been actually saved.")
  
  (defvar %project-libdsfmt-path%
    (merge-pathnames (concatenate 'string "libdSFMT." #+linux "so"
                                  #+darwin "dylib" #+win "dll")
                     %dsfmt-src-path%)
    "The location where libdSFMT has been actually saved.")

  (defvar %project-libdsfmt-wrap-path%
    (merge-pathnames (concatenate 'string "libdSFMT-wrap." #+linux "so"
                                  #+darwin "dylib" #+win "dll")
                     %dsfmt-src-path%)
    "The location where libdSFMT-wrap has been actually saved.")

  (defvar *dsfmt-src-url*
    "http://www.math.sci.hiroshima-u.ac.jp/%7Em-mat/MT/SFMT/dSFMT-src-2.2.3.zip"
    "The url for downloading dSFmt.")

  (defvar *cc* (or (uiop:getenv "CC") "cc"))

  (defvar *cc-flags* '("-DNDEBUG" "-DDSFMT_MEXP=19937" "-fPIC"
                       "-DDSFMT_DO_NOT_USE_OLD_NAMES" "-O3" "-finline-functions"
                       "-fomit-frame-pointer" "-fno-strict-aliasing" "--param"
                       "max-inline-insns-single=1800" "-Wmissing-prototypes" "-Wall"
                       "-std=c99" "-shared" #+x86-64 "-msse2" #+x86-64 "-DHAVE_SSE2"))
  
  (defmacro with-dsfmt-src (&body body)
    `(let ((%dsfmt-zip-path% ,(merge-pathnames "dSFMT-src-2.2.3.zip" %project-root-path%)))
       (unwind-protect
            (progn (download *dsfmt-src-url* %dsfmt-zip-path%)
                   (extract-zip %dsfmt-zip-path%)
                   (rename-file (merge-pathnames "dSFMT-src-2.2.3/" %project-root-path%)
                                %dsfmt-src-path%)
                   (format t "~&dSFMT 2.2.3 has been successfully downloaded and extracted to ~A.~%" %dsfmt-src-path%)
                    ,@body)
         (uiop:delete-file-if-exists %dsfmt-zip-path%))))

  (defun prepare-libdsfmt ()
    (with-dsfmt-src
      (let* ((c-file (merge-pathnames "dSFMT.c" %dsfmt-src-path%))
             (cmd/compile (append (list *cc*) *cc-flags*
                                  (list (namestring c-file) "-o" (namestring %project-libdsfmt-path%))))
             (cmd/check (list "make" "-C" (namestring %dsfmt-src-path%)
                              "std-check" "sse2-check"))
             (cmd/clean (list "make" "-C" (namestring %dsfmt-src-path%) "clean")))
        (uiop:run-program cmd/compile :output t)
        (uiop:run-program cmd/check :output t)
        (uiop:run-program cmd/clean :output t)
        (format t "~&libdSFMT has been successfully compiled, checked and saved to ~A.~%" %project-libdsfmt-path%))))

  (defun prepare-libdsfmt-wrap ()
    (let* ((c-file (merge-pathnames "dSFMT.c" %dsfmt-src-path%))
           (c-wrapper-file (merge-pathnames "src/rng/dSFMT/wrapper.c" %project-root-path%))
           (cmd/compile (append (list *cc*) *cc-flags*
                                (list (concatenate 'string "-I" (namestring %dsfmt-src-path%)))
                                (list (namestring c-file) (namestring c-wrapper-file)
                                      "-o" (namestring %project-libdsfmt-wrap-path%)))))
      (uiop:run-program cmd/compile :output t)
      (format t "~&libdSFMT-wrap has been successfully compiled, checked and saved to ~A.~%" %project-libdsfmt-wrap-path%)))

  ;; if libdSFMT not found, then prepare it.
  (unless (probe-file %project-libdsfmt-path%)
    (prepare-libdsfmt))
  
  ;; if libdSFMT-wrap not found, then prepare it
  (unless (probe-file %project-libdsfmt-wrap-path%)
    (prepare-libdsfmt-wrap))
  
  ;; add '/lib' to CFFI's search path
  (progn
    (pushnew %dsfmt-src-path% *foreign-library-directories* :test 'equal)
    (pushnew :dsfmt *features*)
    (pushnew :dsfmt-2.2.3 *features*))
  ) ;; end of eval-when

(eval-when (:load-toplevel)
  (define-foreign-library libdsfmt
    (:darwin (:or "libdsfmt.dylib"))
    (:unix  (:or "libdsfmt.so"))
    (:win32 "libdsfmt.dll")
    (t (:default "libdsfmt")))
  (unless (foreign-library-loaded-p 'libdsfmt)
    (use-foreign-library libdsfmt))
  
  (define-foreign-library libdsfmt-wrap
    (:darwin (:or "libdsfmt-wrap.dylib"))
    (:unix  (:or "libdsfmt-wrap.so"))
    (:win32 "libdsfmt-wrap.dll")
    (t (:default "libdsfmt-wrap")))
  (unless (foreign-library-loaded-p 'libdsfmt-wrap)
    (use-foreign-library libdsfmt-wrap)))

;; (eval-when (:compile-toplevel :load-toplevel)
;;   (defcfun ("dsfmt_get_min_array_size" get-min-array-size) :int
;;     "This function returns the minimum size of array used for 'fill-array' functions.")
;;   ) ;; end of eval-when
