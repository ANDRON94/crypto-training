;;;; solution.lisp
;;;; Solution for lesson #2

(in-package #:learn-crypto-bitch)

;;; INTERFACE

(defgeneric aes-encrypt (input init-vector key mode action)
  (:documentation "Make encryption/decryption of INPUT byte vector
using a AES block cipher.
MODE specify in which mode encryption will be done:
1) cipher block chain(:cbc);
2) random counter(:ctr).
ACTION specify which process will be executed:
1) encryption(:encrypt);
2) decryption(:decrypt)."))

;;; IMPLEMENTATION

(defmethod aes-encrypt (input init-vector key (mode (eql :cbc))
                        (action (eql :encrypt)))
  (cffi:with-foreign-object (aes-key-mem '(:struct aes-key))
    (let ((en-key (aes-set-key key aes-key-mem))
          (len (length input)))
      (loop :with result = (vector)
         :with chain = init-vector
         :for i :from 0 :below (truncate len +aes-block-size+)
         :for start = (* i +aes-block-size+)
         :for end = (* (1+ i) +aes-block-size+)
         :for curr-input = (subseq input start end)
         :for curr-xored = (xor-vectors curr-input chain)
         :for curr-encrypt = (aes-handle-block curr-xored en-key)
         :do (setf result (concatenate 'vector result curr-encrypt))
             (setf chain curr-encrypt)
         :finally (let* ((padding (make-padding-block input end))
                         (last-xored (xor-vectors padding chain))
                         (last-encrypt (aes-handle-block last-xored en-key)))
                    (setf result (concatenate 'vector result last-encrypt))
                    (return result))))))

(defmethod aes-encrypt (input init-vector key (mode (eql :cbc))
                        (action (eql :decrypt)))
  (cffi:with-foreign-object (aes-key-mem '(:struct aes-key))
    (let ((en-key (aes-set-key key aes-key-mem :action :decrypt)))
      (loop :with result = (vector)
         :with chain = init-vector
         :for i :from 0 :below (length input) :by +aes-block-size+
         :for curr-input = (subseq input i (+ i +aes-block-size+))
         :for curr-encrypt = (aes-handle-block curr-input en-key
                                               :action :decrypt)
         :do (setf result (concatenate 'vector result
                                       (xor-vectors chain curr-encrypt)))
             (setf chain curr-input)
         :finally (return result)))))

(defmethod aes-encrypt (input init-vector key (mode (eql :ctr)) action)
  (declare (ignore action))
  (cffi:with-foreign-object (aes-key-mem '(:struct aes-key))
    (let ((en-key (aes-set-key key aes-key-mem :action :encrypt))
          (len (length input)))
      (loop :with result = (vector)
         :for i :from 0 :below (ceiling len +aes-block-size+)
         :for start = (* i +aes-block-size+)
         :for end = (min (* (1+ i) +aes-block-size+) len)
         :for curr-input = (subseq input start end)
         :for curr-iv = (inc-byte-vector init-vector i)
         :for curr-encrypt = (aes-handle-block curr-iv en-key :action :encrypt)
         :do (setf result (concatenate 'vector result
                                       (xor-vectors curr-input curr-encrypt)))
         :finally (return result)))))

(defun inc-byte-vector (vector num)
  (let ((byte-vector (copy-seq vector)))
    (loop :with inc = num
     :for i :from (1- (length byte-vector)) :downto 0
     :for curr = (aref byte-vector i)
     :for pre-val = (+ curr inc)
     :if (>= pre-val 256)
       :do (setf (aref byte-vector i) (mod pre-val 256))
           (setf inc (truncate pre-val 256))
     :else
       :do (setf (aref byte-vector i) pre-val) :and
       :return byte-vector
     :finally (return byte-vector))))

(defun make-padding-block (input block-end)
  (let* ((len (length input))
         (last-input (subseq input block-end len))
         (pad-len (- +aes-block-size+ (- len block-end))))
    (concatenate 'vector last-input
                 (make-array pad-len :initial-element pad-len))))
