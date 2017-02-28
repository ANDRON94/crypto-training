;;;; solution.lisp
;;;; Solution for lesson #1

(in-package #:learn-crypto-bitch)

;;; INTERFACE

(defun find-all-xor-key-variants (ascii-vectors pivot-id)
  "Find all possible key variants for each ascii code
from ASCII-VECTORS compare to vector with PIVOT-ID index."
  (let ((pivot-vector (aref ascii-vectors pivot-id))
        (comparisons (find-comparisons #'xor-ascii-vectors
                                       ascii-vectors pivot-id)))
    (xor-ascii-lists-with-vector (decompose-xor-comparisons comparisons)
                                 pivot-vector)))

(defun brute-force-attack (ascii-vectors)
  "Find all possible key variants for ASCII-VECTORS(use a first vector
 as pivot). However, a variants generated with other pivot vector will
be the same."
  (find-all-xor-key-variants ascii-vectors 0))

;;; IMPLEMENTATION
