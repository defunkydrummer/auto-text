;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License

(in-package :common-lisp)
(defpackage :auto-text/csv
  (:use :cl)
  (:export
   :config-csv
   :display-csv-header
   :execute-for-csv-columns))

(in-package :auto-text/csv)

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
