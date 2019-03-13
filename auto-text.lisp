;; automatically detect fixed width data

(declaim (optimize (speed 3)))

(deftype tchar () 'character)
(deftype tbyte () '(unsigned-byte 8))

(defparameter *chunk-size* (* 1 (expt 2 20))) ; 1MB of chunk size
(defun make-buffer ()
  (make-array *chunk-size* :element-type 'tbyte
                           :initial-element 0
                           :adjustable nil
                           :displaced-to nil
                           ))

(defstruct (status)
  "Status of detector."
                                        ;(ht (make-hash-table :test 'eql) :type hash-table)
  ;; used for histogram
  (bins (make-array 256 :element-type 'fixnum :adjustable nil :initial-element 0
                        :displaced-to nil)
   :type (simple-array fixnum)))

(declaim (inline process-byte))
(defun process-byte (ch bins)
  "Process byte for histogram"
  (declare (type tbyte ch)
           (type (simple-array fixnum) bins))
  (when (<= 0 ch 255)
    ;; incf the bin in one.
    (incf (aref bins ch))))

(defun histogram-binary-file (path)
  "Histogram. Process the whole file as binary file"
  (declare (inline process-byte))
  (let* ((s (make-status))
        (buffer (make-buffer))
         (bins (status-bins s)))
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
      ;; return status
      s
      )))

;; #+ccl
;; (defun histogram-binary-file (path)
;;   "Histogram. Process the whole file as binary file.
;; CCL version: Uses memory-mapped files.
;; Note: Time was basically the same, even slightly slower  (!!!)"
;;   (declare (inline process-byte))
;;   (let* ((s (make-status))
;;          (buffer (ccl:map-file-to-octet-vector path))
;;          (bins (status-bins s)))
;;     (let ((len (with-open-file (str path :element-type 'tbyte)
;;                  (file-length str))))
;;     (loop for i of-type fixnum from 0 to (1- len)
;;           do (process-byte (aref buffer i) bins)))
;;     s))

;; cr-lf analysis
(defun analyze-cr-lf (path)
  "Analyze file and find if it is terminated by CR or LF. 
Or CRLF. 
Or no line ending found!
Or mixed results!

This will work for 8-bit/7-bit encodings and UTF8 too."
  (let* ((s (histogram-binary-file path))
         (cr (aref (status-bins s) 13))
         (lf (aref (status-bins s) 10)))
    (declare (type fixnum cr lf))
    ;; cr... lf
    (values (cond
              ((zerop (+ cr lf)) :no-line-ending)
              ((eql cr lf) :crlf)
              ((and (> cr lf)
                    (zerop lf)) :cr)
              ((and (> lf cr)
                    (zerop cr)) :lf)
              (t :mixed))
            ;; extra info
            (list :cr cr
                  :lf lf
                  :most-frequent (find-maxima s)))
    ))

(defun find-maxima (s &optional (excluded #(10 13 32)))
  "Find most frequent characters (the max)"
  (declare (type status s)
           (type simple-vector excluded))
  (let ((max (loop for i of-type fixnum from 0 to 255
                   when (not (find i excluded))
                   maximizing (aref (status-bins s) i))))
    ;; return most frequent chars...
    (loop for i of-type fixnum from 0 to 255
          when (eql (aref (status-bins s) i)
                    max)
          collect (code-char i))))

