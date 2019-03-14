(in-package :common-lisp)

(defpackage :auto-text/bom
  (:use :cl :auto-text/common)
  (:export
   :detect-bom-type))

(in-package :auto-text/bom)

;; find if encoding is UTF-8 or what...
;; they are searched in order.
(defparameter *bomsearch*
  '((:type :utf-8
     :bom #(239 187 191)) ; BOM for UTF-8 : EF BB BF
    (:type :utf32-le
     :bom #(255 254 0 0))
    (:type :utf-16be
     :bom #(254 255)) ;  FE FF
    (:type :utf-16le
     :bom #(255 254))
    (:type :utf32-be
     :bom #(0 0 254 255))
    ))


(defun %detect-bom-type (byte-vector)
  "Detect presence and type of BOM in byte vector"
  (loop for test in *bomsearch*
        for type = (getf test :type)
        for bom = (getf test :bom)
        if (eql 0 (search bom byte-vector :test 'equal))
        return type))

(defun detect-bom-type (path)
  "Detect presence and type of BOM in file"
  (with-open-file (str path :element-type 'tbyte)
    (let ((buf (make-array 16 :element-type 'tbyte
                              :initial-element 0
                           )))
      (read-sequence buf str :start 0 :end 15)
      (%detect-bom-type buf))))
