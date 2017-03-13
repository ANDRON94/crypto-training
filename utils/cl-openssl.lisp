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
  (rd-key :pointer)
  (rounds :int))

;;; define functions

(cffi:defcfun ("AES_set_encrypt_key" ffi-aes-set-encrypt-key) :int
  (user-key :pointer)
  (bits-count :int)
  (key :pointer))

(cffi:defcfun ("AES_set_decrypt_key" ffi-aes-set-decrypt-key) :int
  (user-key :pointer)
  (bits-count :int)
  (key :pointer))

(cffi:defcfun ("AES_encrypt" ffi-aes-encrypt) :void
  (in :pointer)
  (out :pointer)
  (key :pointer))

(cffi:defcfun ("AES_decrypt" ffi-aes-decrypt) :void
  (in :pointer)
  (out :pointer)
  (key :pointer))

;;;; INTERFACE

(defun make-aes-encryption-key (user-key bits-count) ; Maybe remove 'bits-count' parameter!
  ;; TODO!
  nil)

(defun make-aes-decryption-key (user-key bits-count) ; Maybe remove 'bits-count' parameter!
  ;; TODO!
  nil)

(defun aes-encrypt-block (plain-block key)
  ;; TODO!
  nil)

(defun aes-decrypt-block (cipher-block key)
  ;; TODO!
  nil)

;;;; IMPLEMENTATION
