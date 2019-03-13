;; automatically detect fixed width or CSV data
;; REQUIRES BABEL


(declaim (optimize (speed 3)))

(deftype tchar () 'character)
(deftype tbyte () '(unsigned-byte 8))
(deftype tbytebuffer () '(simple-array (unsigned-byte 8)))

(defparameter *chunk-size* (* 1 (expt 2 20))) ; 1MB of chunk size
(defparameter *eol-buffer-size* (* 32 (expt 2 10)) ;32K
  "Buffer size for locating an end of line.
Needs to be bigger than the largest line expected!")  

(defun make-buffer (size)
  (make-array size :element-type 'tbyte
                           :initial-element 0
                           :adjustable nil
                           :displaced-to nil
                           ))

(defstruct (status
            (:constructor make-status (path)))
  "Status of detector."
  
  ;; used for histogram
  (bins (make-array 256 :element-type 'fixnum
                        :adjustable nil
                        :initial-element 0
                        :displaced-to nil)
   :type (simple-array fixnum))
  (path "" :type (or string pathname)))

(declaim (inline process-byte))
(defun process-byte (ch bins)
  "Process byte for histogram.
Histogram counts how many times a character of the corresponding code (0 to 255) appears."
  (declare (type tbyte ch)
           (type (simple-array fixnum) bins))
  (when (<= 0 ch 255)
    ;; incf the bin in one.
    (incf (aref bins ch))))

(defun histogram-binary-file (s)
  "Histogram. Process the whole file as binary file."
  (declare (inline process-byte))
  (let* ((path (status-path s))
        (buffer (make-buffer *chunk-size*))
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


;; cr-lf analysis
(defun analyze-cr-lf (path)
  "Analyze file and find if it is terminated by CR or LF. 
Or CRLF. 
Or no line ending found!
Or mixed results!

This will work for 8-bit/7-bit encodings and UTF8 too.

The analysis is done by READING THE WHOLE FILE."
  (let* ((s (histogram-binary-file (make-status path)))
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
                  :lf lf))
    ))

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


(defparameter *eol*
  '(:cr #(13)
   :lf #(10)
   :crlf #(13 10)))

(defun advance-after-eol (stream eol-vector buffer)
  "within stream, seek until finding an EOL, 
return the new file-position for positioning after EOL.

Input parameters: Stream,
EOL VECTOR is the sequence of bytes that indicate an EOL
buffer is a byte buffer, needs to be created beforehand

Side effect: Stream file position altered to after EOL.

Returns: new file position or NIL on EOF."
  (declare (type stream stream)
           (type simple-vector eol-vector)
           (type tbytebuffer buffer))
  (let ((num (read-sequence buffer stream)))
    (if (zerop num) nil
        (progn
          ;; search for the EOL
          (let ((res (search eol-vector buffer :end2 num :test 'eql)))
            (if (null res) ;no EOL found (strange...)
                (advance-after-eol stream eol-vector buffer) ;try again
                ;; else: EOL found
                ;; position after the EOL
                (let ((rewind (- num res (length eol-vector))))
                  
                  (file-position stream
                                 (- (file-position stream) rewind)))))))))


;; included just for clarity
;; (defun advance-after-pattern (stream vector buffer)
;;   "within stream, seek until finding a byte sequence., 
;; return the new file-position for positioning after said sequence..

;; Input parameters: Stream,
;; VECTOR is the sequence to find.
;; buffer is a byte buffer, needs to be created beforehand

;; Side effect: Stream file position altered to after sequence.

;; Returns: new file position or NIL on EOF."
;;   (advance-after-eol stream vector buffer))

(defun sample-rows-bytes (path &key (eol-type :crlf)
                              (sample-size 10))
  "Sample some rows from path, analize them later.
This operates in BYTE mode.
Returns a list of vectors. Each vector being a line, without including EOL bytes."
  (with-open-file (str path 
                            :direction :input
                            :element-type 'tbyte)
    (let* ((result)
          (buffer (make-buffer *eol-buffer-size*))
          (eol-vector (getf *eol* eol-type))
          (eol-len (length eol-vector)))
      (dotimes (x sample-size)
        ;; advance to random position in file
        (file-position str (random (file-length str)))
        ;; advance / position after EOL
        (let* ((fpos1 (advance-after-eol str eol-vector buffer))
               ;; then again -- end of the other line
               (fpos2 (advance-after-eol str eol-vector buffer)))
          (when (not (or (null fpos1)
                         (null fpos2)))
            (let ((line-len (- fpos2 fpos1)))
              ;; read and return the line!
              (file-position str fpos1)
              (read-sequence buffer str :end line-len)
              (push (copy-seq (subseq buffer 0 (- line-len eol-len))) result)))))
      result)))

(defun bytes-to-string (vector encoding)
  "Convert bytes to string using the specified encoding. Uses BABEL."
  (babel:octets-to-string vector :encoding encoding))

(defun sample-rows-string (path &key (eol-type :crlf)
                                     (encoding :utf-8)
                                     (sample-size 10))
  "Sample some rows from path, return as list of strings.
Each line does not include the EOL"
  (loop for rows in (sample-rows-bytes path :eol-type eol-type
                                            :sample-size sample-size)
        collecting 
        (bytes-to-string rows encoding)))


