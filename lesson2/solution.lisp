;;;; solution.lisp
;;;; Solution for lesson #2

(in-package #:learn-crypto-bitch)

;;; INTERFACE

(defgeneric aes-encrypt (input mode action)
  (:documentation "Make encryption/decryption of INPUT byte vector
using a AES block cipher.
MODE specify in which mode encryption will be done:
1) cipher block chain(:cbc);
2) random counter(:ctr).
ACTION specify which process will be executed:
1) encryption(:encrypt);
2) decryption(:decrypt)."))

;;; IMPLEMENTATION

(defmethod aes-encrypt (input (mode (eql :cbc)) (action (eql :encrypt)))
  :cbc-encrypt)

(defmethod aes-encrypt (input (mode (eql :cbc)) (action (eql :decrypt)))
  :cbc-decrypt)

(defmethod aes-encrypt (input (mode (eql :ctr)) (action (eql :encrypt)))
  :ctr-encrypt)

(defmethod aes-encrypt (input (mode (eql :ctr)) (action (eql :decrypt)))
  :ctr-decrypt)

