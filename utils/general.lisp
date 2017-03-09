;;;; general.lisp
;;;; General purpose utility functions

(in-package #:learn-crypto-bitch)

;;; INTERFACE

(defun find-comparisons (comparator vectors pivot-id)
  "Compare vector with PIVOT-ID index with all other
VECTORS usign a COMPARATOR.
COMPARATOR is a function of two arguments(first argument
is pivot vector, second argument is any other vector) that
returns a new vector."
  (loop :with len = (length vectors)
     :with pivot = (aref vectors pivot-id)
     :with result = (make-array (1- len) :fill-pointer 0)
     :for i :from 0 :below len
     :for curr = (aref vectors i)
     :when (not (= i pivot-id))
     :do (vector-push (funcall comparator pivot curr) result)
     :finally (return result)))

(defun visit-all-permutations (variants visitor)
  "VARIANTS is a vector of vectors. Each vector is a collection
of variants and VISIT-ALL-PERMUTATIONS find all possible combinations.
VISITOR is a function of one argument(permutation).
Result is a vector of values returned by VISITOR.
Example:
variants: #(#(1 2) #(3 4 5) #(10 11))
all permutations(2 * 3 * 2 = 12):
1 3 10
1 3 11
1 4 10
1 4 11
1 5 10
1 5 11
2 3 10
and so on..."
  (let* ((lens (loop :for var :across variants :collect (length var)))
         (count-of-variants (length variants))
         (count-of-permutations (reduce #'* lens)))
    (loop :with result = (make-array count-of-permutations :fill-pointer 0)
       :for i :from 0 :below count-of-permutations
       :for permutation = (loop :with result = (make-array count-of-variants
                                                           :fill-pointer 0)
                             :for j :from 0 :below count-of-variants
                             :for variant = (aref variants j)
                             :for len = (nth j lens)
                             :for elem = (aref variant (mod i len))
                             :do (vector-push elem result)
                             :finally (return result))
       :for handled = (funcall visitor permutation)
       :do (vector-push handled result)
       :finally (return result))))

;;; IMPLEMENTATION
