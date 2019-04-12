## AUTO-TEXT

This is a Common Lisp library intended for working with unknown "real-life" text files that hopefully hold data in table form (i.e. CSV files, fixed-width files, etc). 

In these cases, given a text file, its useful to finding out the following:

- Which file encoding can we use? UTF-8, 16, 32? ISO-8859-1? Windows-1252?

- Is the file delimited? Which is the delimiter? (i.e. tab, comma, etc)

- Which is the file ending? CR, LF? CR and LF? Does it have mixed encoding? (Files with more than one different end of line indicator(!!)) 

- Given the above, can we read the file as CSV and get the same amount of columns all the time?

- Can we get how many times each different character appears through a file?

- Can we sample some rows, without having to hold all the file in memory?

**Plus**

- It does not hold the whole file into memory, so it should work for files of any size.

- It works reasonably fast.

 ## Caveats
 
 The file encoding detection is able to discriminate between:
 
 - Files with BOM: UTF-8, 16LE, 16BE, and 32.
 
 - ISO-8859-1, Windows-1252 (Latin european encondings)
 
 - Neither of the above.
 
 This library is intended for working with english or "latin-1" (spanish, french, portuguese, italian) data, thus the choice of encodings.
 
 However, more encoding detection rules can be added or edited in a simple way, see `encoding.lisp`, the code is simple to understand. 

For detection of **asian and far eastern** languages like Chinese, Japanese, Korean, Russian, Arabic, Greek, see the [inquisitor](https://github.com/t-sin/inquisitor) lib.  Inquisitor doesn't work with latin or west european languages.

## Usage

**Main usage:**

```lisp
(ql:quickload "auto-text")

(auto-text:analyze "my-file.txt") 
```

Sample out:

```
CL-USER> (auto-text:analyze #P"my.txt" :silent nil)
Reading file for analysis... my.txt
Eol-type: CRLF
Likely delimiter? #\,  
BOM: NIL 
Possible encodings:  UTF-8 
Sampling 10 rows as CSV for checking width...
(:SAME-NUMBER-OF-COLUMNS T :DELIMITER #\, :EOL-TYPE :CRLF :BOM-TYPE NIL
 :ENCODING :UTF-8)
 
CL-USER> (auto-text:analyze #P"file.txt" :silent nil)
Reading file for analysis... file.txt
Eol-type: CRLF
Likely delimiter? #\Return  
BOM: NIL 
Possible encodings:  WINDOWS-1252 
(:DELIMITER #\Return :EOL-TYPE :CRLF :BOM-TYPE NIL :ENCODING :WINDOWS-1252)
```
Silence messages by passing `:silent T`

The `analyze` function will produce a property-list (plist) with the following keys:

- :same-number-of-columns : True if file appears to have the same number of columns when read as a CSV.

- :delimiter : column separator char

- :eol-type : type of end-of-line char (:CR, :LF, :CRLF, :MIXED, :NO-LINE-ENDING)

- :bom-type : if BOM is detected, which is the UTF type reported.

- :encoding : detected encoding.


**Sample an arbitrary number of rows (lines) from an arbitrarily-sized file:** See `sample-rows-string`:

```lisp
(defun sample-rows-string (path &key (eol-type :crlf)
                                     (encoding :utf-8)
                                     (sample-size 10))
  "Sample some rows from path, return as list of strings.
Each line does not include the EOL"
  (loop for rows in (sample-rows-bytes path :eol-type eol-type
                                            :sample-size sample-size)
        collecting 
        (bytes-to-string rows encoding)))
```

**Obtain a histogram** or frequency hash-table that will tell you how many times a character appears on the whole file. (NOTE: This reads the file in byte mode): See `histogram-binary-file` on `histogram.lisp`

## Config

See `config.lisp`, to config:

- Buffer sizes (important: line buffer size, default 32 KB)

- CSV delimiters to look for.

## Hacking

- Feel free to improve the encoding detection rules on `encoding.lisp` and send me a PR.

- There are more features inside the source code that will later be polished, exported, and 'surfaced' to this readme. 

## Implementations

So far works on SBCL, CCL, CLISP, and ABCL.

Should run in any implementation where BABEL and CL-CSV work fine!!

## Author

Flavio E. also known as *defunkydrummer*

## License

MIT

## See also

The aforementioned [inquisitor](https://github.com/t-sin/inquisitor) lib. This code shares no code in common with inquisitor. 



