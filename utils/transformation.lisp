;;;; transformation.lisp
;;;; Transformation between data types, presentations, etc.

(in-package #:learn-crypto-bitch)

;;; INTERFACE

(defun hex-string->byte-vector (hex-string)
  "Convert string of hex values to a vector
of ASCII codes."
  (let ((len (length hex-string)))
    (loop :with result = (make-array (/ len 2) :fill-pointer 0)
       :for i :from 0 :below len :by 2
       :do (vector-push (parse-integer hex-string :start i :end (+ i 2)
                                       :radix 16)
                        result)
       :finally (return result))))

(defun hex-strings->byte-vectors (messages)
  "Convert several hex strings to a several vectors
of ASCII codes."
  (map 'vector #'hex-string->byte-vector messages))

(defun byte-vector->char-vector (byte-vector)
  "Convert vector of ASCII codes to a vectorS
of characters."
  (map 'vector #'(lambda (ascii) (code-char ascii))
       byte-vector))

(defun xor-vectors (vector1 vector2)
  "Find xor of two vectors(element by element)."
  (map 'vector #'(lambda (x y) (logxor x y))
       vector1 vector2))

(defun xor-vector-with-scalar (vector scalar)
  "Find xor of each element from VECTOR with SCALAR(integer number)."
  (map 'vector #'(lambda (curr-elem) (logxor curr-elem scalar))
       vector))

(defun xor-vectors-with-vector (vectors vector)
  "Find xor of each vector from VECTORS with each scalar
value from VECTOR."
  (map 'vector #'xor-vector-with-scalar vectors vector))

;;; IMPLEMENTATION
