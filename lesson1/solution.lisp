;;;; solution.lisp
;;;; Solution for lesson #1

(in-package #:learn-crypto-bitch)

;;; IMPLEMENTATION

(defparameter *pivot-weight* 3)

(defun letter-or-space-p (code)
  "Check whether char code is a space
or letter."
  (let ((char (code-char code)))
    (or (find char *english-letters*)
        (char= char #\Space))))

(defun digitp (code)
  "Check whether char code is a digit."
  (find (code-char code) *digits*))

(defun punctuationp (code)
  "Check wheter char code is a punctuation sign."
  (find (code-char code) *punctuation-signs*))

(defun char-weight (code)
  "Calculate weight of char code based on frequency
of different groups of characters(letter, digit, punctuation).
More frequency - more weight."
  (cond ((letter-or-space-p code) *pivot-weight*)
        ((digitp code) (- *pivot-weight* 1))
        ((punctuationp code) (- *pivot-weight* 2))
        (t 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun column-weight (ascii-vectors column-id column-key)
    "Calculate a weight of characters belong to column
with COLUMN-ID from ASCII-VECTORS. Character is a xor
of COLUMN-KEY and corresponding ascii value."
    (loop :for vector :across ascii-vectors
       :for code = (aref vector column-id)
       :for letter = (logxor code column-key)
       :sum (char-weight letter))))

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

(defun filter-xor-key-variants (ascii-vectors key-variants filter)
  "Reduce count of possible KEY-VARAINTS using a FILTER heuretic.
FILTER is a function of one argument(a sorted list of
keys with corresponding weight values). Weight for key is
calculating as sum of weights of characters where characters
is a corresponding column from ASCII-VECTORS."
  (loop :for i :from 0 :below (length key-variants)
     :for letter-variants = (nth i key-variants)
     :for raw-keys = (loop :for key :in letter-variants
                        :collect (list (column-weight ascii-vectors i key) key))
     :for sorted-keys = (sort raw-keys #'> :key #'first)
     :collect (funcall filter sorted-keys)))

(defun max-probability-filter (sorted-keys)
  "Return list of keys with maximum weight value."
  (if sorted-keys
      (loop :with max-weight = (first (first sorted-keys))
         :for (weight key) :in sorted-keys
         :if (= weight max-weight)
         :collect key :into result
         :else
         :return result)
      (list nil)))

;;; ANALYSIS OF RESULT

(defparameter *all-key-variants* (brute-force-attack *encrypted-ascii-vectors*))

(defparameter *max-probability-keys* (filter-xor-key-variants
                                      *encrypted-ascii-vectors*
                                      *all-key-variants*
                                      #'max-probability-filter))

(defun print-message-variant (ascii-vector key)
  "Print KEY and message(xor of ASCII-VECTOR and KEY
element by element)."
  (let ((msg (mapcar #'code-char (xor-ascii-vector-with-list ascii-vector
                                                             key))))
    (format t "Keys: ~{~a ~}~%" key)
    (format t "Message: ~{~:c ~}~%~%" msg)
    msg))

(defun print-all-message-variants (ascii-vector key-variants)
  (visit-all-permutations key-variants
                          #'(lambda (perm)
                              (print-message-variant ascii-vector perm))))

;;; ANSWER

(defparameter *final-key* '(102 57 110 137 201 219 216 204 152 116 53 42 205 99 149 16 46 175 206 120 170 127 237 40 160 127 107 201 141 41 197 11 105 176 51 154 25 248 170 64 26 156 109 112 143 128 192 102 199 99 254 240 18 49 72 205 216 232 2 208 91 169 135 119 51 93 174 252 236 213 156 67 58 107 38 139 96 191 78 240 60 154 97))

;;; IMPLEMENTATION
