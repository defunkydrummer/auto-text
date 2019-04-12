#AUTO-TEXT

Common Lisp library for, given a text file, finding out the following:

- Which file encoding can we use? UTF-8, 16, 32? ISO-8859-1? Windows-1252?

- Is the file delimited? Which is the delimiter? (i.e. tab, comma, etc)

- Which is the file ending? CR, LF? CR and LF?

- Given the above, can we read the file as CSV and get the same amount of columns all the time?

**Plus**

- It does not hold the whole file into memory, so it should work for files of any size.

- It works reasonably fast.

 ## Caveats
 
 The file encoding detection is able to discriminate between:
 
 - Files with BOM: UTF-8, 16LE, 16BE, and 32.
 
 - ISO-8859-1, Windows-1252 (Latin european encondings)
 
 - Neither of the above.
 
 This library is intended for working with english or "latin-1" (spanish, french, portuguese, italian) data, thus the choice of encodings.
 
 However, more encoding detection rules can be added or edited in a simple way, see `encoding.lisp`, for example the current rules are as follows:

For detection of **asian and far eastern** languages like Chinese, Japanese, Korean, Russian, Arabic, Greek, see the [inquisitor](https://github.com/t-sin/inquisitor) lib.  Inquisitor doesn't work with latin or west european languages.

## Usage

```lisp
(ql:quickload "auto-text")

(auto-text:analyze "my-file.txt") 
```

This will produce a property-list (plist) with the following keys:

- :same-number-of-columns : True if file appears to have the same number of columns when read as a CSV.

- :delimiter : column separator char

- :eol-type : type of end-of-line char (:CR, :LF, or :CRLF)

- :bom-type : if BOM is detected, which is the UTF type reported.

- :encoding : detected encoding.

## Config

See `config.lisp`, to config:

- Buffer sizes (important: line buffer size, default 32 KB)

- CSV delimiters to look for.

## Hacking

- Feel free to improve the encoding detection rules on `encoding.lisp` and send me a PR.

## Implementations

Should run in any implementation where BABEL and CL-CSV work. 

## Author

Flavio E. also known as *defunkydrummer*

## License

MIT

## See also

The aforementioned [inquisitor](https://github.com/t-sin/inquisitor) lib. This code shares no code in common with inquisitor. 



