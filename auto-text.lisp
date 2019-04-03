;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License

(in-package :common-lisp)

(defpackage :auto-text
  (:use :cl
   :auto-text/common
   :auto-text/config
   :auto-text/histogram
        :auto-text/eol
        :auto-text/encoding))

(in-package :auto-text)

(declaim (optimize (speed 0) (debug 3)))

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


(defun sample-rows-string (path &key (eol-type :crlf)
                                     (encoding :utf-8)
                                     (sample-size 10))
  "Sample some rows from path, return as list of strings.
Each line does not include the EOL"
  (loop for rows in (sample-rows-bytes path :eol-type eol-type
                                            :sample-size sample-size)
        collecting 
        (bytes-to-string rows encoding)))


;; simple way to see maximum and minimum line width
(defun line-width-test (path &key eol-type (encoding :utf-8) (sample-size 1000))
  "Sample some lines from the file and return the max, avg and min number of character in each line."
  (loop 
    for c in
    ;; get some sample lines, check the size.
    (mapcar
     (lambda (x) (babel:vector-size-in-chars x :encoding encoding))
     (sample-rows-bytes path :eol-type eol-type :sample-size sample-size))
    maximizing c into i
    minimizing c into j
    summing c into s
    counting T into x
    finally (return (list :max i
                          :min j
                          :avg (float  (/ s x))
                          :is-fixed
                          (fixed-width-p i j (/ s x))))))

(defun fixed-width-p (max min avg)
  "T if must be a fixed width file.
Input parameters are number of characters per line."
  (and (eql max avg) (eql max min) T))


;; (defun histogram-fixed-width-file (path &key width eol-type encoding
;;                                           (separator-char #\Space)
;;                                           (max-rows nil))
;;   "
;; automatically detect fixed width start of columns...
;; by making histogram on the position of each character in the line: how many are spaces?

;; requires reading the file one line at a time.

;; returns: histogram bins, number of valid lines read, number of invalid lines.
;; "
;;   (declare (type fixnum width)
;;            (type symbol eol-type encoding)
;;            (type character separator-char)
;;            (type pathname path)
;;            (type (or fixnum null) max-rows)
;;            (optimize (speed 3)))
;;   (let* ((buffer (make-buffer *eol-buffer-size*))
;;          ;; index: the position within the line -> array
;;          ;; value: how many spaces appear
;;          (megabins
;;            (make-array width
;;                        :element-type 'fixnum 
;;                        :adjustable nil
;;                        :displaced-to nil
;;                        ))
;;          (eol-vector (getf *eol* eol-type)))
;;     (declare (type simple-array megabins buffer))
;;     (or eol-vector (error "Invalid eol-type"))
;;     (with-open-file (str path :element-type 'tbyte)
;;       ;; read each record
;;       (let ((line buffer)
;;             (sline "")
;;             (valid-lines 0)
;;             (invalid-lines 0))
;;         (declare (type string sline)
;;                  (type simple-array buffer)
;;                  (type fixnum valid-lines invalid-lines))
;;         (prog ()
;;          init
;;            (setf line (fetch-line str eol-vector buffer))
;;            ;; decode line to string
;;            ;; because UTF-8 is variable length, for example.
;;            (when line
;;              (setf sline (babel:octets-to-string line :encoding encoding))
;;              (if (eql (length sline) width)
;;                  (progn 
;;                    ;; do histogram:
;;                    ;; array[x]: number of times space appears
;;                    ;; x: position of the character within the line
;;                    ;; (but the line is decoded using Babel!)
;;                    (loop for pos of-type fixnum from 0 to (1- width)
;;                          for char of-type character = (aref sline pos)
;;                          when (eql char separator-char)
;;                          do (incf (aref megabins pos))) 

;;                    ;; next line
;;                    (incf valid-lines)
;;                    (when max-rows
;;                      (if (eql max-rows valid-lines)
;;                          (go end)))
;;                    (go init))
;;                  ;; else
;;                  (progn
;;                    (format t "** Line of unequal width ~D at stream position ~D~%"
;;                            (length sline)
;;                            (file-position str))
;;                    (format t "~A~%" sline)
;;                    (incf invalid-lines)
;;                    (go init))
;;                  ))
;;          end
;;            (return (values megabins valid-lines invalid-lines)))
;;         ))))



;; do automatic analysis
(defun analyze (path &key (sample-size 10))
  "Automatically analyze the file."
  (format t "Reading file for analysis... ~A~%" path)
  (let* ((bins (histogram-binary-file path))
         (eol-type (analyze-cr-lf bins))
         (delimiters-report (delimiters-report bins))
         (bom-type (detect-bom-type path)))
    (format t "Eol-type: ~a~%Likely delimiter? ~a  ~%BOM: ~a ~%"
            eol-type
            (prin1-to-string (caar delimiters-report))
            bom-type)
    (if (and bom-type
             (not (eql bom-type :utf-8)))
        (progn 
          (format t "Bom type != UTF-8 requires stopping at this point.~%")
          (list :eol-type eol-type
                :delimiter (caar delimiters-report)
                :bom-type bom-type))
        
        ;; else
        ;; when no BOM detected, try to detect encoding
        (let ((encodings
                (if (eql bom-type :utf-8) '(:utf-8)
                             (detect-file-encoding bins)))
              (delimiter (caar delimiters-report)))
          (format t "Possible encodings: ~{ ~a ~}~%"
                  encodings)
          ;; when at least one encoding detected,
          ;; and CR/LF is not mixed,
          ;; and delimiter is valid CSV delimiter
          ;; try reading some rows as CSV...
          (when (and  encodings
                      (find (char-code delimiter)
                            *valid-csv-delimiters*
                            :test 'equal)
                      (not (equal eol-type :mixed)))
            (format t "Sampling ~D rows as CSV for checking width...~&" sample-size)
            (let* ((encoding (car encodings))
                   (rows
                     (sample-rows-string
                      path
                      :eol-type eol-type
                      :encoding encoding
                      :sample-size sample-size)))
              ;;use cl-csv to check if all fields have same length
              (let* ((parsed-rows
                       (loop for r in rows
                             collecting 
                             (cl-csv:read-csv-row r
                                                  :separator delimiter)))
                     (len
                       (loop for pr in parsed-rows
                             collecting (length pr)))
                     (all-equal
                       ;; all rows have same length
                       (loop for i from 0 to (- (length len) 2)
                             always (equal (elt len i)
                                           (elt len (1+ i))))))
                (list :same-number-of-columns all-equal
                      :delimiter delimiter
                      :eol-type eol-type
                      :bom-type bom-type
                      :encoding encoding))))))))



;;-------------------------------------
;; Output to CSV from fixed column width format. 
;; Considering a file of this format:
;; |COL1 |COL2   |COL3|   etc. (with or without delimiter)
;; Output a CSV.
;; Requires start and end of each column as a pair.
;; note: column indexes start in 0
;; character at column-end is not read --> indexes are [a,b[
;;
;; start-line = 1 : start at first line
;; start-position = start position index (in bytes)
;;
;; Note: CR/LF inside records will be replaced with #\Space
;;-------------------------------------
;; REQUIRES CL-CSV
(defun fixed-cols-to-csv (file-in file-out
                          line-width
                          column-index-list ;list like '((0 5) (6 10)) etc
                          &key (start-line 1)
                               (start-position 0)
                               (trim-fields T) ; apply rtrim and ltrim on all fields?
                               (eol-type :crlf)
                               in-external-format
                               (out-external-format :default))
  (assert (getf *eol* eol-type)) 
  (let ((buffer (make-array (* line-width 3)
                            :element-type 'character))
        (eol-vector (getf *eol* eol-type))
        (string ""))
    (with-open-file (str-in file-in
                            :direction :input
                            :external-format in-external-format)
      (file-position str-in start-position)
      ;; advance to offset if needed
      ;; skip number of lines if needed
      (when (> start-line 1)
        (dotimes (x (1- start-line))
          (read-line str-in)))
      (with-open-file (str-out file-out
                               :direction :output
                               :external-format out-external-format)
        (prog (num)
         init
           (setf num
                 (read-sequence buffer str-in :start 0 :end line-width))
           
           (when (zerop num) (go end))
           (setf string
                 (subseq buffer 0 num))
           ;(break string)
           
           ;; Filter CR/LF on string
           (setf string (filter-eol-on-string string eol-vector #\Space))

           ;; split in columns, send to cl-csv
           (cl-csv:write-csv-row 
            (loop for index in column-index-list
                  for start = (first index)
                  for end = (second index)
                  collecting
                  (if trim-fields
                      (string-trim '(#\Space) (subseq string start end))
                      (subseq string start end)))
            :stream str-out)
           ;; advance EOL characters
           (file-position str-in
                          (+ (file-position str-in) (length eol-vector)))
           (go init)
         end
           (return T))))))

