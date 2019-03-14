(in-package :common-lisp)

;; common stuff to our packages
(defpackage :auto-text/common
  (:use :cl)
  (:export
   :tchar
   :tbyte
   :tbytebuffer
   :make-buffer
   :bytes-to-string))

(in-package :auto-text/common)

(deftype tchar () 'character)
(deftype tbyte () '(unsigned-byte 8))
(deftype tbytebuffer () '(simple-array (unsigned-byte 8)))

(defun make-buffer (size)
  (make-array size :element-type 'tbyte
                   :initial-element 0
                   :adjustable nil
                   :displaced-to nil
                   ))

(defun bytes-to-string (vector encoding)
  "Convert bytes to string using the specified encoding. Uses BABEL."
  (babel:octets-to-string vector :encoding encoding))

