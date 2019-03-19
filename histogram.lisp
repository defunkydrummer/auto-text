;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License

(in-package :common-lisp)

(defpackage :auto-text/histogram
  (:use :cl
   :auto-text/common
        :auto-text/config
        )
  (:export
   :make-histogram-bins
   :process-byte
   :histogram-binary-file
   :histogram-report
   :present-characters
   :delimiters-report
   ))

(in-package :auto-text/histogram)

(defun make-histogram-bins ()
  (make-array 256 :element-type 'fixnum
                  :adjustable nil
                  :initial-element 0
                  :displaced-to nil))


;; (defstruct (status
;;             (:constructor make-status (path)))
;;   "Status of detector."
;;   ;; used for histogram
;;   (bins (make-histogram-bins)
;;    :type (simple-array fixnum))
;;   (path "" :type (or string pathname)))

(declaim (inline process-byte))
(defun process-byte (ch bins)
  "Process byte for histogram.
Histogram counts how many times a character of the corresponding code (0 to 255) appears."
  (declare (type tbyte ch)
           (type (simple-array fixnum) bins))
  (when (<= 0 ch 255)
    ;; incf the bin in one.
    (incf (aref bins ch))))

(defun histogram-binary-file (path)
  "Histogram. Process the whole file as binary file.
Returns histogram bins"
  (declare (inline process-byte))
  (let* ((buffer (make-buffer *chunk-size*))
         (bins (make-histogram-bins)))
    (with-open-file (str path :element-type 'tbyte)
      ;; read the whole file??? no... do it in chunks...
      (prog (res)
       init
         ;; read buffer...
         (setf res (read-sequence buffer str));returns length of read sequence
         ;; apply process
         (loop for i of-type fixnum from 0 to (1- res)
               do (process-byte (aref buffer i) bins))
         (when (> res 0)
           (go init)))
      ;; return histogram bins
      (the tbins bins)
      )))

(defun histogram-report (bins)
  "Report histogram by characters with higher frequencies.
Returns: alist of character . occurrences.
Only returns for characters below 127."
  (declare (type tbins bins))
  (let ((c (loop for i from 0 to 255
                 collecting (cons (code-char i)
                                  (aref bins i)))))
    (sort c #'> :key #'cdr)))

(defun present-characters (bins)
  "From the histogram bins, collect the char codes that were present (count>0)"
  (declare (type tbins bins))
  (loop for i from 0 to 255
        when (> (aref bins i) 0)
        collecting i))


(defun delimiters-report (bins &optional
                                 (delimiter-chars
                                  *delimiter-chars-vector*)) 
  "For the chars in the delimiter-chars vector,
return the histogram (amount of times it appears)."
  (declare (type tbins bins))
  (let ((c (loop for i across delimiter-chars
                 collecting (cons (code-char i)
                                  (aref bins i)))))
    (sort c #'> :key #'cdr)))
