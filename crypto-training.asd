;;;; crypto-training.asd
;;;; System definiton

(asdf:defsystem #:crypto-training
  :version "1.0.0"
  :description ""
  :author "Andrii Tymchuk <makedonsky94@gmail.com>"
  :license "GPLv3.0"
  :depends-on ("cffi")
  :components ((:file "package")
               (:module "utils"
                        :serial nil
                        :components
                        ((:file "general")
                         (:file "helper-data")
                         (:file "transformation")
                         (:file "special" :depends-on("helper-data"))
                         (:file "cl-openssl")))
               (:module "lesson1"
                        :serial nil
                        :components
                        ((:file "data")
                         (:file "solution" :depends-on("data"))))
               (:module "lesson2"
                        :serial nil
                        :components
                        (;; (:file "data")
                         (:file "solution")))))
