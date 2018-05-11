;;;; wrapper.lisp
(in-package #:chenyi.rng.dsfmt)

;;;; Global variable
(defcvar ("DSFMT_MEXP_WRAP" +DSFMT-MEXP+ :library libdsfmt-wrap) :int
  "Mersenne Exponent. The period of the sequence is a multiple of 2^+DSFMT-MEXP+ - 1.")

(defcvar ("DSFMT_N_WRAP" +DSFMT-N+ :library libdsfmt-wrap) :int
  "DSFMT generator has an internal state array of 128-bit integers, and +DSFMT-N+ is its size.")

(defcvar ("DSFMT_N32_WRAP" +DSFMT-N32+ :library libdsfmt-wrap) :int
  "+DSFMT-N32+ is the size of internal state array when regarded as an array of 32-bit integers.")

(defcvar ("DSFMT_N64_WRAP" +DSFMT-N64+ :library libdsfmt-wrap) :int
  "+DSFMT-N64+ is the size of internal state array when regarded as an array of 64-bit integers.")

;;; Functions
(defcfun ("dsfmt_get_min_array_size" :library libdsfmt) :int
  "This function returns the minimum size of array used for 'fill-array' functions.")

(defcfun ("dsfmt_gen_rand_all" :library libdsfmt) :void
  "This function fills the internal state array with double precision floating point pseudorandom numbers of the IEEE 754 format."
  (dsfmt :pointer))

(defcfun ("dsfmt_fill_array_close1_open2" :library libdsfmt) :void
  "This function generates double precision floating point pseudorandom numbers which distribute in the range [1, 2) to the specified array by one call. The number of pseudorandom numbers is specified by the argument size, which must be at least (+DSFMT-MEXP+ / 128) * 2 and a multiple of two. The function get-min-array-size returns this minimum size. The generation by this function is much faster than the following fill-array-xxx functions.
For initialization, dsfmt-init-gen-rand or dsfmt-init-by-array must be called before the first call of this function. This function can not be used after calling genrand-xxx functions, without initialization."
  (dsfmt :pointer) (array :pointer) (size :int))

(defcfun ("dsfmt_fill_array_open_close" :library libdsfmt) :void
  "This function generates double precision floating point pseudorandom numbers which distribute in the range (0, 1] to the specified array by one call. This function is the same as fill-array-close1-open2 except the distribution range."
  (dsfmt :pointer) (array :pointer) (size :int))

(defcfun ("dsfmt_fill_array_close_open" :library libdsfmt) :void
  "This function generates double precision floating point pseudorandom numbers which distribute in the range [0, 1) to the specified array by one call. This function is the same as fill-array-close1-open2 except the distribution range."
  (dsfmt :pointer) (array :pointer) (size :int))

(defcfun ("dsfmt_fill_array_open_open" :library libdsfmt) :void
  "This function generates double precision floating point pseudorandom numbers which distribute in the range (0, 1) to the specified array by one call. This function is the same as fill-array-close1-open2 except the distribution range."
  (dsfmt :pointer) (array :pointer) (size :int))

(defcfun ("dsfmt_chk_init_gen_rand" :library libdsfmt) :void
  "This function initializes the internal state array with a 32-bit integer seed."
  (dsfmt :pointer) (seed :unsigned-int) (mexp :int))

(defcfun ("dsfmt_chk_init_by_array" :library libdsfmt) :void
  "This function initializes the internal state array, with an array of 32-bit integers used as the seeds."
  (dsfmt :pointer) (init-key :pointer) (key-length :int) (mexp :int))

(defcfun ("dsfmt_get_idstring" :library libdsfmt) :string
  "This function returns the identification string. The string shows the Mersenne exponent, and all parameters of this generator.")

;;; Definitions from wrapper.c
(defmacro %define-dsfmt-api (name return-type &rest arglist)
  (flet ((to-lisp-name ()
           (cl-ppcre:regex-replace-all "_" name "-")))
    `(defcfun (,(concatenate 'string name "_wrap")
                ,(intern (string-upcase (to-lisp-name)) 'chenyi.rng)
                :library libdsfmt-wrap) ,return-type ,@arglist)))

(%define-dsfmt-api "dsfmt_get_global_data" :pointer)

(%define-dsfmt-api "dsfmt_genrand_uint32" :uint32 (dsfmt :pointer)) 

(%define-dsfmt-api "dsfmt_genrand_close1_open2" :double (dsfmt :pointer))

(%define-dsfmt-api "dsfmt_genrand_close_open" :double (dsfmt :pointer))

(%define-dsfmt-api "dsfmt_genrand_open_close" :double (dsfmt :pointer))

(%define-dsfmt-api "dsfmt_genrand_open_open" :double (dsfmt :pointer))

(%define-dsfmt-api "dsfmt_gv_genrand_uint32" :uint32)

(%define-dsfmt-api "dsfmt_gv_genrand_close1_open2" :double)

(%define-dsfmt-api "dsfmt_gv_genrand_close_open" :double)

(%define-dsfmt-api "dsfmt_gv_genrand_open_close" :double)

(%define-dsfmt-api "dsfmt_gv_genrand_open_open" :double)

(%define-dsfmt-api "dsfmt_gv_fill_array_open_close" :void
                   (array :pointer) (size :int))

(%define-dsfmt-api "dsfmt_gv_fill_array_close_open" :void
                   (array :pointer) (size :int))

(%define-dsfmt-api "dsfmt_gv_fill_array_open_open" :void
                   (array :pointer) (size :int))

(%define-dsfmt-api "dsfmt_gv_fill_array_close1_open2" :void
                   (array :pointer) (size :int))

(%define-dsfmt-api "dsfmt_gv_init_gen_rand" :void (seed :uint32))

(%define-dsfmt-api "dsfmt_gv_init_by_array" :void
                   (init-key :pointer) (key-length :int))

(%define-dsfmt-api "dsfmt_init_gen_rand" :void
                   (dsfmt :pointer) (seed :uint32))

(%define-dsfmt-api "dsfmt_init_by_array" :void
                   (dsfmt :pointer) (init-key :pointer) (key-length :int))
