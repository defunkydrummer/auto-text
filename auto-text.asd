(in-package :cl-user)
(defpackage :auto-text-asd
  (:use :cl :asdf))
(in-package :auto-text-asd)

(defsystem auto-text
  :version "0.1"
  :author "Flavio Egoavil also known as D E F U N K Y D R U M M E R"
  :license "MIT"
  :depends-on (:babel
               :cl-csv)    
  :serial T
  :components ((:file "common")
               (:file "config")
               (:file "histogram")
               (:file "encoding")
               (:file "eol-reader") ;eol package
               (:file "auto-text"))
  
  :description "Utilities for working with data text files."
  :long-description "No long description!")

