;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License

;; ----- CSV UTILS ------


(common-lisp:defpackage :auto-text/csv
  (:use :cl)
  (:export
   :config-csv
   :display-csv-header
   :execute-for-csv-columns
   :fixed-cols-to-csv))

(common-lisp:in-package :auto-text/csv)

;; read header
(defun csv-header (path &key separator encoding)
  "Read CSV header"
  (declare (type character separator))
  (with-open-file (str path
                       :direction :input
                       :external-format encoding)
    (cl-csv:read-csv-row str :separator separator )))

;; Obtain ALIST from the csv header,
;; each cons is ( column name . column index)
(defun alist-header (header)
  (loop for i from 0 to (1- (length header))
        collecting (cons (elt header i) i)))

(defun alist-print (alist)
  "Print the alist obtained by alist-header"
  (format t "~0,0T~a ~1,40T~a~%~%" "Column Header" "Index")
  (loop for c in alist do
        (format t "~0,0T~a ~1,40T~d~%" (car c) (cdr c))))

;; represents a CSV file and its configuration / metadata
(defclass csv-config ()
  ((path :initarg :path
         :accessor csv-path
         :initform ""
         :type (or string pathname)
         :documentation "Path to the CSV file.")
   (num-columns :initarg :num-columns
                :reader csv-num-columns
                :initform 0
                :type fixnum
                :documentation "Number of columns.")
   (separator :initarg :separator
               :initform #\,
               :accessor csv-separator
              :type (or string character))
   (quote-char :accessor csv-quote-char
               :initform #\"
               :type (or character null)
               :documentation "Character for quoting.")
   (column-alist :initarg :column-alist
                 :reader csv-columns
                 :initform ()
                 :type cons
                 :documentation "Alist of column headers.")
   (encoding :initarg :encoding
             :reader csv-encoding
             :type symbol)))

(defun config-csv (path &key separator encoding)
  ;; configure csv file
  (let ((header (csv-header path :separator separator
                                 :encoding encoding)))
    (make-instance 'csv-config
                   :path path
                   :column-alist (alist-header header)
                   :separator separator
                   :num-columns (length header)
                   :column-alist (alist-header header)
                   :encoding encoding)))

(defun display-csv-header (csv-config)
  "Pretty-print the csv header"
  (alist-print (csv-columns csv-config)))

(defun execute-for-csv-columns
    (csv column-index-list row-function &key (skip-header T))
  "Open the CSV file, and execute row-function for each row.
Row-function receives only the specified columns.
Columns are specified in columns-index-list. (First column = 0)"
  (flet ((rowfn (r)
           ;; filter the row with only the columns we want
           ;; and send it to row-function
           (funcall row-function
                    (loop for i from 0 to (length r)
                          when (find i column-index-list :test 'eql)
                          collect (elt r i)))))
    (let ((cl-csv:*default-external-format* (csv-encoding csv)))
      (cl-csv:read-csv (csv-path csv)
                       :skip-first-p skip-header 
                       :row-fn #'rowfn
                       :separator (csv-separator csv)
                       :quote (csv-quote-char csv)
                       ))))


;;-------------------------------------
;; Output to CSV from fixed column width format. 
;; Considering a file of this format:
;; |COL1 |COL2   |COL3|   etc. (with or without delimiter)
;; Output a CSV.
;; Requires start and end of each column as a pair.
;; note: column indexes start in 0
;; character at column-end is not read --> indexes are [a,b[
;; line-width is record width without including CR/LF
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
           
           (cond ((zerop num) (go end))
                 ((< num line-width)
                  ;; we have read less characters than requested.
                  ;; warning
                  (format t "Warning: Read ~A characters, expected ~A. Skipping line.~%"
                          num
                          line-width)
                  ;; go next line.
                  (go init)))
           (setf string
                 (subseq buffer 0 num))
           
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

