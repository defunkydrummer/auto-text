;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License

(in-package :common-lisp)

;; common stuff to our packages
(defpackage :auto-text/common
  (:use :cl)
  (:export
   :tchar
   :tbyte
   :tbytebuffer
   :tbins
   :make-buffer
   :bytes-to-string))

(in-package :auto-text/common)

(deftype tchar () 'character)
(deftype tbyte () '(unsigned-byte 8))
(deftype tbytebuffer () '(simple-array (unsigned-byte 8)))

;;histogram bins
(deftype tbins () '(simple-array fixnum))

(defun make-buffer (size)
  (make-array size :element-type 'tbyte
                   :initial-element 0
                   :adjustable nil
                   :displaced-to nil
                   ))

(defun bytes-to-string (vector encoding)
  "Convert bytes to string using the specified encoding. Uses BABEL."
  (babel:octets-to-string vector :encoding encoding))

