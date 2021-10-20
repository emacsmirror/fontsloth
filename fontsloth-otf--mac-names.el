;;; fontsloth-otf--mac-names.el --- Mac char names -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.12.1
;; Package-Requires: ((cl-lib "1.0") (names "20151201.0") (emacs "26.1"))
;; Keywords: true-type, font, bindat, ttf, otf, parsing

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Part of fontsloth

;; fontsloth-otf--mac-names (this file): just the Macintosh char names

;; To use this module by itself, load and enable it as follows:
;;   (use-package fontsloth-otf)
;;

;;; Code:

(defvar fontsloth-otf--mac-names
  (vector
   ".notdef"
   ".null"
   "nonmarkingreturn"
   "space"
   "exclam"
   "quotedbl"
   "numbersign"
   "dollar"
   "percent"
   "ampersand"
   "quotesingle"
   "parenleft"
   "parenright"
   "asterisk"
   "plus"
   "comma"
   "hyphen"
   "period"
   "slash"
   "zero"
   "one"
   "two"
   "three"
   "four"
   "five"
   "six"
   "seven"
   "eight"
   "nine"
   "colon"
   "semicolon"
   "less"
   "equal"
   "greater"
   "question"
   "at"
   "A"
   "B"
   "C"
   "D"
   "E"
   "F"
   "G"
   "H"
   "I"
   "J"
   "K"
   "L"
   "M"
   "N"
   "O"
   "P"
   "Q"
   "R"
   "S"
   "T"
   "U"
   "V"
   "W"
   "X"
   "Y"
   "Z"
   "bracketleft"
   "backslash"
   "bracketright"
   "asciicircum"
   "underscore"
   "grave"
   "a"
   "b"
   "c"
   "d"
   "e"
   "f"
   "g"
   "h"
   "i"
   "j"
   "k"
   "l"
   "m"
   "n"
   "o"
   "p"
   "q"
   "r"
   "s"
   "t"
   "u"
   "v"
   "w"
   "x"
   "y"
   "z"
   "braceleft"
   "bar"
   "braceright"
   "asciitilde"
   "Adieresis"
   "Aring"
   "Ccedilla"
   "Eacute"
   "Ntilde"
   "Odieresis"
   "Udieresis"
   "aacute"
   "agrave"
   "acircumflex"
   "adieresis"
   "atilde"
   "aring"
   "ccedilla"
   "eacute"
   "egrave"
   "ecircumflex"
   "edieresis"
   "iacute"
   "igrave"
   "icircumflex"
   "idieresis"
   "ntilde"
   "oacute"
   "ograve"
   "ocircumflex"
   "odieresis"
   "otilde"
   "uacute"
   "ugrave"
   "ucircumflex"
   "udieresis"
   "dagger"
   "degree"
   "cent"
   "sterling"
   "section"
   "bullet"
   "paragraph"
   "germandbls"
   "registered"
   "copyright"
   "trademark"
   "acute"
   "dieresis"
   "notequal"
   "AE"
   "Oslash"
   "infinity"
   "plusminus"
   "lessequal"
   "greaterequal"
   "yen"
   "mu"
   "partialdiff"
   "summation"
   "product"
   "pi"
   "integral"
   "ordfeminine"
   "ordmasculine"
   "Omega"
   "ae"
   "oslash"
   "questiondown"
   "exclamdown"
   "logicalnot"
   "radical"
   "florin"
   "approxequal"
   "Delta"
   "guillemotleft"
   "guillemotright"
   "ellipsis"
   "nonbreakingspace"
   "Agrave"
   "Atilde"
   "Otilde"
   "OE"
   "oe"
   "endash"
   "emdash"
   "quotedblleft"
   "quotedblright"
   "quoteleft"
   "quoteright"
   "divide"
   "lozenge"
   "ydieresis"
   "Ydieresis"
   "fraction"
   "currency"
   "guilsinglleft"
   "guilsinglright"
   "fi"
   "fl"
   "daggerdbl"
   "periodcentered"
   "quotesinglbase"
   "quotedblbase"
   "perthousand"
   "Acircumflex"
   "Ecircumflex"
   "Aacute"
   "Edieresis"
   "Egrave"
   "Iacute"
   "Icircumflex"
   "Idieresis"
   "Igrave"
   "Oacute"
   "Ocircumflex"
   "apple"
   "Ograve"
   "Uacute"
   "Ucircumflex"
   "Ugrave"
   "dotlessi"
   "circumflex"
   "tilde"
   "macron"
   "breve"
   "dotaccent"
   "ring"
   "cedilla"
   "hungarumlaut"
   "ogonek"
   "caron"
   "Lslash"
   "lslash"
   "Scaron"
   "scaron"
   "Zcaron"
   "zcaron"
   "brokenbar"
   "Eth"
   "eth"
   "Yacute"
   "yacute"
   "Thorn"
   "thorn"
   "minus"
   "multiply"
   "onesuperior"
   "twosuperior"
   "threesuperior"
   "onehalf"
   "onequarter"
   "threequarters"
   "franc"
   "Gbreve"
   "gbreve"
   "Idotaccent"
   "Scedilla"
   "scedilla"
   "Cacute"
   "cacute"
   "Ccaron"
   "ccaron"
   "dcroat"))

(provide 'fontsloth-otf--mac-names)
;;; fontsloth--mac-names.el ends here
