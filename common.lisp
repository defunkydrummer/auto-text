;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License



;; common stuff to our packages
(common-lisp:defpackage :auto-text/common
  (:use :cl)
  (:export
   :tchar
   :tbyte
   :tbytebuffer
   :tunsigned
   :tinteger
   :tbins
   :tbins-element
   :make-buffer
   :bytes-to-string))

(common-lisp:in-package :auto-text/common)

(deftype tchar () 'character)
(deftype tbyte () '(unsigned-byte 8))
(deftype tbytebuffer () '(simple-array (unsigned-byte 8)))
(deftype tunsigned () '(integer 0 1152921504606846975)) ;2^60-1

#-abcl
(deftype tinteger () '(integer -1152921504606846976 1152921504606846975)) ;-2^60 to 2^60-1

#+abcl
(deftype tinteger () 'integer) ; ABCL has a problem with for i of-type tinteger on loops... (!)
                               ; "The specified data type AUTO-TEXT/HISTOGRAM::TINTEGER is not a subtype of NUMBER."

;;histogram bins
(deftype tbins-element () 'tunsigned)
(deftype tbins () '(simple-array tbins-element))

(defun make-buffer (size)
  (make-array size :element-type 'tbyte
                   :initial-element 0
                   :adjustable nil
                   :displaced-to nil
                   ))

(defun bytes-to-string (vector encoding)
  "Convert bytes to string using the specified encoding. Uses BABEL."
  (babel:octets-to-string vector :encoding encoding))

