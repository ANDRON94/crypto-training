;;;; general.lisp
;;;; General purpose utilities

(in-package #:learn-crypto-bitch)

;;; INTERFACE

(defmacro english-letters ()
  "Generate english alphabet at compile time."
  (loop :for i :from (char-code #\a) :to (char-code #\z)
     :with diff = (- (char-code #\A) (char-code #\a))
     :collect (code-char i) :into lowercase
     :collect (code-char (+ i diff)) :into uppercase
     :finally (return `(list ,@(nconc lowercase uppercase)))))

(defmacro digits ()
  "Generate digits at compile time."
  (loop :for i :from (char-code #\0) :to (char-code #\9)
     :collect (code-char i) :into result
     :finally (return `(list ,@result))))

(defparameter *english-letters* (english-letters))

(defparameter *digits* (digits))

(defparameter *punctuation-signs* '(#\Space #\. #\, #\? #\! #\:
                                    #\; #\' #\" #\( #\) #\[ #\] #\{ #\}
                                    #\- #\/))

(defparameter *alphabet* (append *english-letters* *digits*
                                 *punctuation-signs*))

(defun decompose-xored-letter (letter-code)
  "Find all variants of X such that the
X xor Y = LETTER-CODE, for any Y."
  (loop :for x :in *alphabet*
     :for x-code = (char-code x)
     :when (loop :for y :in *alphabet*
              :for y-code = (char-code y)
              :when (= letter-code (logxor x-code y-code))
              :return t)
     :collect x-code))

(defun decompose-xor-comparison (xor-comparison)
  "Decompose each `xored' letter code from XOR-COMPARISON
vector."
  (loop :for code :across xor-comparison
     :collect (decompose-xored-letter code)))

(defun decompose-xor-comparisons (xor-comparisons)
  "Decompose each `xor comparison' from XOR-COMPARISONS
vector and then find intersection of decompositions element
by element. So, finally function returns list of variants
for each xored letter that satisfies all XOR-COMPARISONS."
  (loop :with result = (decompose-xor-comparison (aref xor-comparisons 0))
     :for i :from 1 :below (length xor-comparisons)
     :for curr = (aref xor-comparisons i)
     :for pre-result = (decompose-xor-comparison curr)
     :do (setf result (mapcar #'(lambda (list1 list2)
                                  (nintersection list1 list2 :test #'=))
                              result pre-result))
     :finally (return result)))

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
  "VARIANTS is a list of lists. VISITOR is a function
of one argument(permutation). Result is a list of values
 returned by VISITOR."
  (let* ((lens (loop :for var :in variants :collect (length var)))
         (count (reduce #'* lens)))
    (loop :for i :from 0 :below count
       :for permutation = (loop :for j :from 0 :below (length variants)
                             :for variant = (nth j variants)
                             :for len = (nth j lens)
                             :collect (nth (mod i len) variant))
       :collect (funcall visitor permutation))))

;;; IMPLEMENTATION
