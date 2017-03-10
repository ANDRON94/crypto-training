;;;; special.lisp
;;;; Utility functions for a specific problem

(in-package #:learn-crypto-bitch)

;;; INTERFACE

(defun decompose-xored-letter (letter-code)
  "Find all variants of X such that the
X xor Y = LETTER-CODE, for any Y."
  (loop :with result = (make-array 0 :fill-pointer 0
                                   :adjustable t)
     :for x :in *alphabet*
     :for x-code = (char-code x)
     :when (loop :for y :in *alphabet*
              :for y-code = (char-code y)
              :when (= letter-code (logxor x-code y-code))
              :return t)
     :do (vector-push-extend x-code result)
     :finally (return result)))

(defun decompose-xor-comparison (xor-comparison)
  "Decompose each `xored' letter code from XOR-COMPARISON
vector."
  (loop :with result = (make-array (length xor-comparison) :fill-pointer 0)
     :for code :across xor-comparison
     :for variants = (decompose-xored-letter code)
     :do (vector-push variants result)
     :finally (return result)))

(defun decompose-xor-comparisons (xor-comparisons)
  "Decompose each `xor comparison' from XOR-COMPARISONS
vector and then find intersection of decompositions element
by element. So, finally function returns list of variants
for each xored letter that satisfies all XOR-COMPARISONS."
  (loop :with result = (decompose-xor-comparison (aref xor-comparisons 0))
     :for i :from 1 :below (length xor-comparisons)
     :for curr = (aref xor-comparisons i)
     :for pre-result = (decompose-xor-comparison curr)
     :do (setf result (map 'vector #'intersect-numbers result pre-result))
     :finally (return result)))

;;; IMPLEMENTATION

(defun intersect (vector1 vector2 &key key test)
  "Find intersection of two vectors(VECTOR1, VECTOR2)."
  (coerce (nintersection (coerce vector1 'list)
                 (coerce vector2 'list)
                 :key key :test test) 'vector))

(defun intersect-numbers (vector1 vector2)
  "Find intersection of two vectors with number
values inside."
  (intersect vector1 vector2 :test #'=))
