;; Copyright (c) Flavio Egoavil <F_egoavil@hotmail.com> aka D E F U N K Y D R U M M E R
;; MIT License

;; encoding detection

(in-package :common-lisp)

(defpackage :auto-text/encoding
  (:use :cl
   :auto-text/common
   :auto-text/bom)
  (:import-from :auto-text/histogram #:present-characters)
  (:export
   :detect-file-encoding
   :detect-bom-type
   ))

(in-package :auto-text/encoding)


;; file encoding analysis
(defparameter *encoding-detection-tables*
  ;; the tests to perform
  `(:iso-8859-1  ; similar but not equal to windows-1252
    ;; We assume that our UTF-8 file is never going to be
    ;; Cyrillic or of an asian language
    ;; 
    ;; Note: E3...ED on UTF-8 would be used for asian characters
    ;; D0..D4 used for cyrillic
    ;; D5..DD armenic, arabic, hebrew, syriac
    ;; * cells must never appear in a valid UTF-8 sequence:
    ;;  c0, c1, f5...ff
    (#xc1            ; tilde a uppercase *
     #xfa            ; tilde u *
     #xfc            ; u dieresis *
                                        ;#xf1 ; spanish Ñ  --> USED IN UTF8
                                        ;#xf2 ; tilde o reverse --> USED IN UTF8
                                        ;#xf3 ; tilde o --> used in UTF8
     #xd3            ; tilde o uppercase --> Cyrillic in UTF8
     #xe9            ; tilde e --> asian UTF8
     #xc9            ; tilde e uppercase
     #xe8            ; backtilde e -- added for french --> asian UTF8
     #xe2            ; a circumflex -- added for french --> asian UTF8
                                        ;#xc2  ; a circumflex uppercase -- added for french  --> USED IN UTF8
     #xe7 ; c cedilla -- added for french and portuguese --> asian UTF8
     #xd1 ; spanish Ñ uppercase  --> CYRILLIC UTF8
     )
    :iso-8859-1-illegal
    ;; chars that should not be on ISO 8859-1
    ,(append
      (loop for x from #x80 to #x9f collecting x))
    :windows-1252-illegal
    ;; Note: Windows-1252 admits more chars than ISO-8859-1
    (#x81 #x8d #x8f
          #x90 #x9d )
    :utf8-illegal
    ;; illegal characters for UTF-8
    ,(append
      (loop for x from #xf5 to #xff collecting x)
      (list #xc0 #xc1) ; other two illegal characters
      )))

(defparameter *encoding-detection-rules*
  ;; execute rules in order
  ;; if found... then
  '((:test (:utf8-illegal :windows-1252-illegal) ;; complies both
     :not nil
     :result nil)
    (:test-function (:utf8-illegal :iso-8859-1-illegal) ;; complies both
     :not (:windows-1252-illegal)   ;;doesnt test for 1252 illegal
     :result :windows-1252)
    (:test (:utf8-illegal :iso-8859-1)
     :not (:iso-8859-1-illegal)
     :result :iso-8859-1)
    (:test nil
     :any (:windows-1252-illegal :iso-8859-1-illegal) ;; complies any of them
     :not (:utf8-illegal)
     :result :utf-8)))

(defun detect-file-encoding (bins &optional
                                    (tables *encoding-detection-tables*)
                                    (rules
                                     *encoding-detection-rules*))
  "Try to detect file encoding using the histogram bins.
Returns: The result of the test: list of detected encoding(s)
secondary value: the tests that were complied from the encoding-detection-table-

Note: This does not use or read the BOM. For BOM use the functions below."
  (declare (type tbins bins))
  (let* ((tests-complied
           ;; apply each test in the tables.
           (loop with present-chars = (present-characters bins) ;all characters with frequency>0
                 for f in tables by #'cddr ;keys: the tests to perform
                 for inter =  (intersection present-chars (getf tables f) :test 'equal)
                 when inter
                 collect f))
         (rules-complied 
           ;; process each rule...
           (loop for r in rules
                 ;; verify that all the tests in :test are complied
                 for ok-tested = (subsetp (getf r :test) tests-complied :test 'equal)

                 ;; verify that at least one of the tests in :any is complied
                 for ok-any = (not (null (intersection  (getf r :any) tests-complied :test 'equal)))
                 ;; verify that the tests in the :not list are not complied
                 for ok-doesnt-test = (not
                                       (subsetp (getf r :not) tests-complied :test 'equal))
                 ;; below test is necessary because subsetp considers NIL
                 ;; a subset of all sets
                 when (and (or (null (getf r :test)) ok-tested)
                           (or (null (getf r :any)) ok-any)
                           (or (null (getf r :not)) ok-doesnt-test))
                 collect (getf r :result))))
    (values rules-complied tests-complied)))



;; *************
;; BOM detection
;; *************

;; find if encoding is UTF-8 or what... by reading the BOM
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

