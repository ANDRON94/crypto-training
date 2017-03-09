;;;; transformation.lisp
;;;; Transformation between data types, presentations, etc.

(in-package #:learn-crypto-bitch)

;;; INTERFACE

(defun hex-string->ascii-vector (hex-string)
  "Convert string of hex values to a vector
of ASCII codes."
  (let ((len (length hex-string)))
    (loop :for i :from 0 :below len :by 2
       :with result = (make-array (/ len 2) :fill-pointer 0)
       :do (vector-push (parse-integer hex-string :start i :end (+ i 2)
                                       :radix 16)
                        result)
       :finally (return result))))

(defun hex-strings->ascii-vectors (messages)
  "Convert several hex strings to a several vectors
of ASCII codes."
  (map 'vector #'hex-string->ascii-vector messages))

(defun ascii-vector->char-vector (ascii-vector)
  "Convert vector of ASCII codes to a vectorS
of characters."
  (map 'vector #'(lambda (ascii) (code-char ascii))
       ascii-vector))

(defun xor-ascii-vectors (vector1 vector2)
  "Find xor of two vectors(element by element)."
  (map 'vector #'(lambda (x y) (logxor x y))
       vector1 vector2))

(defun xor-ascii-vector-with-code (ascii-vector code)
  "Find xor of each element from ASCII-VECTOR with CODE."
  (map 'vector #'(lambda (ascii-code) (logxor ascii-code code))
       ascii-vector))

(defun xor-ascii-vectors-with-codes (ascii-vectors codes)
  "Find xor of each vector from ASCII-VECTORS with each code
from CODES."
  (map 'vector #'xor-ascii-vector-with-code ascii-vectors codes))

;;; IMPLEMENTATION
