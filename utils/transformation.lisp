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

(defun xor-ascii-list-with-code (ascii-list code)
  "Find xor of each element from ASCII-LIST with CODE."
  (mapcar #'(lambda (ascii-code) (logxor ascii-code code))
          ascii-list))

(defun xor-ascii-lists-with-vector (ascii-lists vector)
  "Find xor of each list from ASCII-LISTS with VECTOR
(element by element)."
  (map 'list #'xor-ascii-list-with-code ascii-lists vector))

;;; IMPLEMENTATION
