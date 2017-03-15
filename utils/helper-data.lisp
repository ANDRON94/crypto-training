;;;; helper-data.lisp
;;;; Collection of data sets(which helps to solve a problems)

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

(defparameter +english-letters+ (english-letters))

(defparameter +digits+ (digits))

(defparameter +punctuation-signs+ '(#\Space #\. #\, #\? #\! #\:
                                    #\; #\' #\" #\( #\) #\[ #\] #\{ #\}
                                    #\- #\/))

(defparameter +alphabet+ (append +english-letters+ +digits+
                                 +punctuation-signs+))

;;; IMPLEMENTATION
