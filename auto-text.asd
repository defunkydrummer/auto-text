(in-package :cl-user)
(defpackage :auto-text-asd
  (:use :cl :asdf))
(in-package :auto-text-asd)

(defsystem auto-text
  :version "0.1"
  :author "Flavio Egoavil"
  :license "Proprietary"
  :depends-on (:babel)    
  :serial T
  :components ((:file "common")
               (:file "config")
               (:file "bom")
               (:file "auto-text"))
  
  :description "Utilities for working with data text files."
  :long-description "No long description!")






