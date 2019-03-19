;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License

(in-package :common-lisp)

(defpackage :auto-text/config
  (:use :cl)
  (:export
   :*eol*
   :*chunk-size*
   :*eol-buffer-size*
   :*delimiter-chars-vector*))

(in-package :auto-text/config)

(defparameter *eol*
  '(:cr #(13)
    :lf #(10)
    :crlf #(13 10))
  "Vectors for searching line endings.")

(defparameter *chunk-size* (* 1 (expt 2 20))
  "Size of block to read from file in binary mode.") ; 1MB of chunk size

(defparameter *eol-buffer-size* (* 32 (expt 2 10)) ;32K
  "Buffer size for locating an end of line.
Needs to be bigger than the largest line expected!")  

(defparameter *delimiter-chars-vector*
  (vector (char-code #\Tab)
          (char-code #\|)
          (char-code #\,)
          (char-code #\;)
          13                            ;CR
          10)                           ;LF
  "Delimiter chars to look for")
