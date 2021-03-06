;;;; cl-openssl.lisp
;;;; Lisp wrapper for an OpenSSL library

(in-package #:learn-crypto-bitch)

;;;; INTERFACE

(defparameter +aes-key-sizes+ '(16 24 32)
  "List of all possible AES key sizes in octets(bytes) count.")

(defconstant +aes-block-size+ 16
  "AES block size in octets(bytes) count.")

(defconstant +aes-round-key-size+ 16
  "AES round key size in octets(bytes).")

(defconstant +aes-word32-count-per-round-key+ (/ +aes-round-key-size+ 4)
  "Count of 32-bit words to hold round key.")

(defconstant +aes-max-rounds-count+ 14
  "Maximum count of rounds defined by AES specification.")

(defconstant +aes-max-count-of-round-keys+ (1+ +aes-max-rounds-count+)
  "Maximum count of round keys defined by AES
specification(a separate 128-bit round key for each round plus one more).")

(defconstant +bits-per-byte+ 8
  "Count of bits per byte(octet).")

;;;; IMPLEMENTATION

;;; define and load 'openssl' library before using

(cffi:define-foreign-library openssl
  (:unix (:or "libssl.so.1.0.0"
              "libssl.so.1.0.2"))
  (t (:default "libssl3")))

(cffi:use-foreign-library openssl)

;;; define types

(cffi:defcstruct aes-key
  (rd-key :uint32 :count #.(* +aes-max-count-of-round-keys+
                              +aes-word32-count-per-round-key+))
  (rounds :int))

(cffi:define-foreign-type byte-vector-type ()
  ((count :reader byte-vector-count :initarg :count))
  (:actual-type :pointer))

(cffi:define-parse-method byte-vector (&key (count 1))
  (make-instance 'byte-vector-type :count count))

(defmethod cffi:translate-to-foreign (vector (type byte-vector-type))
  (let* ((count (byte-vector-count type))
         (real-count (if vector (length vector) count)))
    (cffi:foreign-alloc :uchar :count real-count
                        :initial-contents vector)))

(defmethod cffi:translate-from-foreign (pointer (type byte-vector-type))
  (let ((count (byte-vector-count type)))
    (loop :with res = (make-array count :fill-pointer 0)
       :for i :from 0 :below count
       :do (vector-push (cffi:mem-aref pointer :uchar i) res)
       :finally (return res))))

(defmethod cffi:free-translated-object (pointer (type byte-vector-type) param)
  (declare (ignore param))
  (cffi:foreign-free pointer))

(defmethod cffi:expand-to-foreign-dyn (value var body (type byte-vector-type))
  (let ((count (byte-vector-count type))
        (real-count (gensym)))
    `(let* ((,real-count (if ,value (length ,value) ,count))
            (,var (cffi:foreign-alloc :uchar :count ,real-count
                                      :initial-contents ,value)))
       (unwind-protect
            (progn ,@body)
         (cffi:foreign-free ,var)))))

(defmethod cffi:expand-from-foreign (form (type byte-vector-type))
  (let ((count (byte-vector-count type))
        (i (gensym)) (value (gensym)) (res (gensym)))
    `(loop :with ,value = ,form :and
        :with ,res = (make-array ,count :fill-pointer 0)
        :for ,i :from 0 :below ,count
        :do (vector-push (cffi:mem-aref ,value :uchar ,i) ,res)
        :finally (return ,res))))

;;; define functions

(cffi:defcfun ("AES_set_encrypt_key" ffi-aes-set-encrypt-key) :int
  (user-key byte-vector)
  (bits-count :int)
  (key (:pointer (:struct aes-key))))

(cffi:defcfun ("AES_set_decrypt_key" ffi-aes-set-decrypt-key) :int
  (user-key byte-vector)
  (bits-count :int)
  (key (:pointer (:struct aes-key))))

(cffi:defcfun ("AES_encrypt" ffi-aes-encrypt) :void
  (in (:pointer :unsigned-char))
  (out (:pointer :unsigned-char))
  (key (:pointer (:struct aes-key))))

(cffi:defcfun ("AES_decrypt" ffi-aes-decrypt) :void
  (in (:pointer :unsigned-char))
  (out (:pointer :unsigned-char))
  (key (:pointer (:struct aes-key))))

;;;; INTERFACE

(defun aes-set-key (user-key aes-key-mem &key (action :encrypt))
  "Create AES internal(per each round) key using a USER-KEY
value(vector of bytes) and allocated memory AES-KEY-MEM enough
to hold AES-KEY structure. ACTION allows to choose which
key should be created:
1) encryption(:encrypt, default mode);
2) decryption(:decrypt).
Returns generated AES-KEY."
  (let* ((key-size (length user-key))
         (key-size-bits (* key-size +bits-per-byte+)))
    (if (find key-size +aes-key-sizes+)
        (progn (ecase action
                 (:encrypt (ffi-aes-set-encrypt-key user-key
                                                    key-size-bits
                                                    aes-key-mem))
                 (:decrypt (ffi-aes-set-decrypt-key user-key
                                                    key-size-bits
                                                    aes-key-mem)))
               aes-key-mem)
        (error "Wrong user-key size ~d! ~
Should be equal to 16, 24 or 32!" key-size))))

(defun aes-handle-block (block key &key (action :encrypt))
  "Handle 128-bit(16-byte) BLOCK(vector of bytes) using
an internal(per each round) KEY. ACTION allows to choose
which mode should be initiated:
1) encryption(:encrypt, default mode);
2) decryption(:decrypt).
Returns handled BLOCK."
  (let ((block-size (length block)))
    (if (= block-size +aes-block-size+)
        (cffi:with-foreign-object (ffi-block :uchar block-size)
          ;; init ffi-block with block values
          (loop :for i :from 0 :below block-size
             :for curr = (aref block i)
             :do (setf (cffi:mem-aref ffi-block :uchar i) curr))
          (ecase action
            (:encrypt (ffi-aes-encrypt ffi-block ffi-block key))
            (:decrypt (ffi-aes-decrypt ffi-block ffi-block key)))
          ;; convert result from C array to Lisp vector
          (loop :with result = (make-array block-size :fill-pointer 0)
             :for i :from 0 :below block-size
             :do (vector-push (cffi:mem-aref ffi-block :uchar i) result)
             :finally (return result)))
        (error "Wrong block size ~d! Should be equal to 16!" block-size))))

;;;; IMPLEMENTATION
