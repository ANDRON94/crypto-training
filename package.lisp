;;;; package.lisp
;;;; Definition of 'learn-crypto-bitch' package(interface, dependencies)

(in-package #:cl-user)

(defpackage #:learn-crypto-bitch
  (:use #:cl)
  (:export ;; Utils
           ;; general
           #:find-comparisons
           #:visit-all-permutations
           ;; helper-data
           #:english-letters
           #:digits
           #:*english-letters*
           #:*digits*
           #:*punctuation-signs*
           #:*alphabet*
           ;; transformation
           #:hex-string->byte-vector
           #:hex-strings->byte-vectors
           #:byte-vector->char-vector
           #:xor-vectors
           #:xor-vector-with-scalar
           #:xor-vectors-with-vector
           ;; special
           #:decompose-xored-letter
           #:decompose-xor-comparison
           #:decompose-xor-comparisons
           ;; cl-openssl
           #:make-aes-encryption-key
           #:make-aes-decryption-key
           #:aes-encrypt-block
           #:aes-decrypt-block
           ;; Lesson1
           ;; TODO: think!
           ;; Lesson2
           ;; TODO: think!
           ))
