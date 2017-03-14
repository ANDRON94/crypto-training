;;;; cl-openssl.lisp
;;;; Lisp wrapper for an OpenSSL library

(in-package #:learn-crypto-bitch)

;;;; IMPLEMENTATION

;;; define and load 'openssl' library before using

(cffi:define-foreign-library openssl
  (:unix (:or "libssl.so.1.0.2"))
  (t (:default "libssl3")))

(cffi:use-foreign-library openssl)

;;; define types

(cffi:defcstruct aes-key
  (rd-key (:pointer :unsigned-int))
  (rounds :int))

;;; define functions

(cffi:defcfun ("AES_set_encrypt_key" ffi-aes-set-encrypt-key) :int
  (user-key (:pointer :unsigned-char))
  (bits-count :int)
  (key (:pointer :aes-key)))

(cffi:defcfun ("AES_set_decrypt_key" ffi-aes-set-decrypt-key) :int
  (user-key (:pointer :unsigned-char))
  (bits-count :int)
  (key (:pointer :aes-key)))

(cffi:defcfun ("AES_encrypt" ffi-aes-encrypt) :void
  (in (:pointer :unsigned-char))
  (out (:pointer :unsigned-char))
  (key (:pointer :aes-key)))

(cffi:defcfun ("AES_decrypt" ffi-aes-decrypt) :void
  (in (:pointer :unsigned-char))
  (out (:pointer :unsigned-char))
  (key (:pointer :aes-key)))

;;;; INTERFACE

(defparameter +AES-KEY-SIZES+ '(16 24 32)
  "List of all possible AES key sizes in octets(bytes) count.")

(defparameter +AES-BLOCK-SIZE+ 16
  "AES block size in octets(bytes) count")

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
