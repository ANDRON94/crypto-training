;;;; cl-openssl.lisp
;;;; Lisp wrapper for an OpenSSL library

(in-package #:learn-crypto-bitch)

;;;; INTERFACE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +aes-key-sizes+ '(16 24 32)
    "List of all possible AES key sizes in octets(bytes) count.")

  (defparameter +aes-block-size+ 16
    "AES block size in octets(bytes) count.")

  (defparameter +aes-round-key-size+ 16
    "AES round key size in octets(bytes).")

  (defparameter +aes-word32-count-per-round-key+ (/ +aes-round-key-size+ 4)
    "Count of 32-bit words to hold round key.")

  (defparameter +aes-max-rounds-count+ 14
    "Maximum count of rounds defined by AES specification.")

  (defparameter +aes-max-count-of-round-keys+ (1+ +aes-max-rounds-count+)
    "Maximum count of round keys defined by AES
specification(a separate 128-bit round key for each round plus one more)."))

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

(defun make-aes-encryption-key (user-key)
  "Create AES encryption key using 'key schedule'
procedure based on USER-KEY.
USER-KEY is a vector of bytes"
  ;; TODO!
  nil)

(defun make-aes-decryption-key (user-key)
  ;; TODO!
  nil)

(defun release-aes-key (key)
  ;; TODO!
  nil)

(defun aes-encrypt-block (plain-block key)
  ;; TODO!
  nil)

(defun aes-decrypt-block (cipher-block key)
  ;; TODO!
  nil)

;;;; IMPLEMENTATION
