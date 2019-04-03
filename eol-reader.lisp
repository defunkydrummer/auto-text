;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License

(in-package :common-lisp)

;; **********************
;; Functions for reading byte streams line per line
;; **********************

(defpackage :auto-text/eol
  (:use :cl
   :auto-text/common)
  (:export
   :advance-after-eol
   ;:fetch-line
   ;:fetch-fixed-amount-of-characters
   :filter-eol-on-string))

(in-package :auto-text/eol)

(declaim (optimize (speed 3)))

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
                  ;; NOTE:
                  ;; spec says: file-position returns true if the repositioning is performed successfully
                  (file-position stream
                                 (- (file-position stream) rewind))
                  ;; return file pos
                  (file-position stream)
                  )))))))


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

;; --------------------------------------------------------
;; FETCH LINE DOES NOT WORK CORRECTLY!! BUGS!!!
;; --------------------------------------------------------

;; (defun fetch-line (str eol-vector buffer)
;;   "Read line from stream (current position) into buffer.
;; This advances the file position to after the end of line.
;; This can also be used to read delimited files...

;; NOTE: Obviously this won't work with UTF-16 or UTF-32 files.

;; NIL when line not found (EOF)."
;;   (let* ((fpos1
;;            (if (zerop (file-position str)) 0
;;                (advance-after-eol str eol-vector buffer)))
;;          ;; then again -- go to the end of the other line
;;          (fpos2 (advance-after-eol str eol-vector buffer)))
;;     ;; get line length
;;     (when (not (or (null fpos1)
;;                    (null fpos2)))
;;       (let ((line-len (- fpos2 fpos1)))
;;         ;; read and return the line!
;;         (file-position str fpos1)
;;         (read-sequence buffer str :end line-len)
;;         ;; the line itself... 
;;         (subseq buffer 0 (- line-len (length eol-vector)))))))

;; (defun fetch-line-babel (str eol-vector buffer encoding)
;;   "Read line with fetch-line, then convert with BABEL to the
;; desired encoding. (Encoding must be a babel-accepted encoding.)
;; STRING IS RETURNED (dont use the buffer)"
;;   (babel:octets-to-string
;;    (fetch-line str eol-vector buffer)
;;    :encoding encoding))

;; (defun fetch-fixed-amount-of-characters
;;     (str buffer num-chars encoding)
;;   "Read line (using BABEL) from byte stream, to get the required number of CHARACTERS (not bytes), using babel as a decoder. (Encoding must be a babel-accepted encoding.)
;; STRING IS RETURNED (dont use the buffer)"
;;   ;; we need to read more than the line num of chars, because UTF-8 etc
;;   (read-sequence buffer str :end (length buffer))
;;   (babel:octets-to-string 
;;    buffer
;;    :start 0
;;    :end num-chars
;;    :encoding encoding))

(defun filter-eol-on-string (string eol-vector &optional (new-character #\Space))
  "Filter EOL on string, replace with new-character. 
STRING IS RETURNED (dont use the buffer)"
  ;(break string)
  (loop for chcode across eol-vector
        do
        (when (find (code-char chcode) string :test #'equal)
          (break (format nil "CR or LF on: ~A" string)))
        (nsubstitute new-character (code-char chcode) string :test #'equal))
  string)

