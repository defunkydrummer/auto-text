;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License

(in-package :common-lisp)

(defpackage :auto-text
  (:use :cl
   :auto-text/common
   :auto-text/config
   :auto-text/histogram
        :auto-text/eol
   :auto-text/encoding)
  (:export
   :analyze
   :fixed-cols-to-csv
   :sample-rows-bytes
   ))

(in-package :auto-text)

;(declaim (optimize (speed 0) (debug 3)))

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
;; (defun line-width-test (path &key eol-type (encoding :utf-8) (sample-size 1000))
;;   "Sample some lines from the file and return the max, avg and min number of character in each line."
;;   (loop 
;;     for c in
;;     ;; get some sample lines, check the size.
;;     (mapcar
;;      (lambda (x) (babel:vector-size-in-chars x :encoding encoding))
;;      (sample-rows-bytes path :eol-type eol-type :sample-size sample-size))
;;     maximizing c into i
;;     minimizing c into j
;;     summing c into s
;;     counting T into x
;;     finally (return (list :max i
;;                           :min j
;;                           :avg (float  (/ s x))
;;                           :is-fixed
;;                           (fixed-width-p i j (/ s x))))))

;; (defun fixed-width-p (max min avg)
;;   "T if must be a fixed width file.
;; Input parameters are number of characters per line."
;;   (and (eql max avg) (eql max min) T))



;; do automatic analysis
(defun analyze (path &key (sample-size 10) (silent nil))
  "Automatically analyze the file."
  (or silent (format t "Reading file for analysis... ~A~%" path))
  (let* ((bins (histogram-binary-file path))
         (eol-type (analyze-cr-lf bins))
         (delimiters-report (delimiters-report bins))
         (bom-type (detect-bom-type path)))
    (or silent (format t "Eol-type: ~a~%Likely delimiter? ~a  ~%BOM: ~a ~%"
                       eol-type
                       (prin1-to-string (caar delimiters-report))
                       bom-type))
    (if (and bom-type
             (not (eql bom-type :utf-8)))
        (progn
          ;; UTF-16 and UTF-32 support not yet ready. 
          (or silent (format t "Bom type != UTF-8 requires stopping at this point.~%"))
          (list :eol-type eol-type
                :delimiter (caar delimiters-report)
                :bom-type bom-type))
        
        ;; else
        ;; when no BOM detected, try to detect encoding
        (let ((encodings
                (if (eql bom-type :utf-8) '(:utf-8)
                             (detect-file-encoding bins)))
              (delimiter (caar delimiters-report)))
          (or silent (format t "Possible encodings: ~{ ~a ~}~%"
                             encodings))
          ;; when at least one encoding detected,
          ;; and CR/LF is not mixed,
          ;; and delimiter is valid CSV delimiter
          ;; try reading some rows as CSV...
          (if (and encodings
                      (find (char-code delimiter)
                            *valid-csv-delimiters*
                            :test 'equal)
                      (not (find eol-type '(:mixed :no-line-ending))))
            (progn 
              (or silent (format t "Sampling ~D rows as CSV for checking width...~&" sample-size))
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
                        :encoding encoding))))
            ;; couldn't sample rows as CSV
            (list :delimiter delimiter
                  :eol-type eol-type
                  :bom-type bom-type
                  :encoding (car encodings))
            )))))



