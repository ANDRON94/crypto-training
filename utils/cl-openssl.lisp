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

;;; define functions

(cffi:defcfun ("AES_set_encrypt_key" ffi-aes-set-encrypt-key) :int
  (user-key (:pointer :unsigned-char))
  (bits-count :int)
  (key (:pointer (:struct aes-key))))

(cffi:defcfun ("AES_set_decrypt_key" ffi-aes-set-decrypt-key) :int
  (user-key (:pointer :unsigned-char))
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
  "Create AES internal(per each round) key using a USER-KEY value
and allocated memory AES-KEY-MEM enough to hold AES-KEY structure.
ACTION allows to choose which key should be created:
1) encryption(:encrypt, default mode);
2) decryption(:decrypt).
Returns generated AES-KEY."
  (let* ((key-size (length user-key))
         (key-size-bits (* key-size +bits-per-byte+)))
    (if (find key-size +aes-key-sizes+)
        (cffi:with-foreign-object (ffi-user-key :uchar key-size)
          ;; init ffi-user-key with user-key values
          (loop :for i :from 0 :below key-size
             :for curr = (aref user-key i)
             :do (setf (cffi:mem-aref ffi-user-key :uchar i) curr))
          (ecase action
            (:encrypt (ffi-aes-set-encrypt-key ffi-user-key
                                               key-size-bits
                                               aes-key-mem))
            (:decrypt (ffi-aes-set-decrypt-key ffi-user-key
                                               key-size-bits
                                               aes-key-mem)))
          aes-key-mem)
        (error "Wrong user-key size ~d! ~
Should be equal to 16, 24 or 32!" key-size))))

(defun aes-encrypt-block (plain-block key)
  ;; TODO!
  nil)

(defun aes-decrypt-block (cipher-block key)
  ;; TODO!
  nil)

;;;; IMPLEMENTATION
