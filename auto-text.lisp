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

;; cr-lf analysis
(defun analyze-cr-lf (bins)
  "Analyze file and find if it is terminated by CR or LF. 
Or CRLF. 
Or no line ending found!
Or mixed results!

This will work for 8-bit/7-bit encodings and UTF8 too.
"
  (declare (type tbins bins))
  (let* ((cr (aref bins 13))
         (lf (aref bins 10)))
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


(defun histogram-fixed-width-file (path &key width eol-type encoding
                                          (separator-char #\Space)
                                          (max-rows nil))
  "
automatically detect fixed width start of columns...
by making histogram on the position of each character in the line: how many are spaces?

requires reading the file one line at a time.

returns: histogram bins, number of valid lines read, number of invalid lines.
"
  (declare (type fixnum width)
           (type symbol eol-type encoding)
           (type character separator-char)
           (type pathname path)
           (type (or fixnum null) max-rows)
           (optimize (speed 3)))
  (let* ((buffer (make-buffer *eol-buffer-size*))
         ;; index: the position within the line -> array
         ;; value: how many spaces appear
         (megabins
           (make-array width
                       :element-type 'fixnum 
                       :adjustable nil
                       :displaced-to nil
                       ))
         (eol-vector (getf *eol* eol-type)))
    (declare (type simple-array megabins buffer))
    (or eol-vector (error "Invalid eol-type"))
    (with-open-file (str path :element-type 'tbyte)
      ;; read each record
      (let ((line buffer)
            (sline "")
            (valid-lines 0)
            (invalid-lines 0))
        (declare (type string sline)
                 (type simple-array buffer)
                 (type fixnum valid-lines invalid-lines))
        (prog ()
         init
           (setf line (fetch-line str eol-vector buffer))
           ;; decode line to string
           ;; because UTF-8 is variable length, for example.
           (when line
             (setf sline (babel:octets-to-string line :encoding encoding))
             (if (eql (length sline) width)
                 (progn 
                   ;; do histogram:
                   ;; array[x]: number of times space appears
                   ;; x: position of the character within the line
                   ;; (but the line is decoded using Babel!)
                   (loop for pos of-type fixnum from 0 to (1- width)
                         for char of-type character = (aref sline pos)
                         when (eql char separator-char)
                         do (incf (aref megabins pos))) 

                   ;; next line
                   (incf valid-lines)
                   (when max-rows
                     (if (eql max-rows valid-lines)
                         (go end)))
                   (go init))
                 ;; else
                 (progn
                   (format t "** Line of unequal width ~D at stream position ~D~%"
                           (length sline)
                           (file-position str))
                   (format t "~A~%" sline)
                   (incf invalid-lines)
                   (go init))
                 ))
         end
           (return (values megabins valid-lines invalid-lines)))
        ))))

