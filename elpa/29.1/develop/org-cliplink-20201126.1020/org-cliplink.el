;;; org-cliplink.el --- insert org-mode links from the clipboard -*- lexical-binding: t -*-

;; Copyright (C) 2014 Alexey Kutepov a.k.a rexim

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/org-cliplink
;; Version: 0.2
;; Package-Requires: ((emacs "24.4"))

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Usage:
;;
;; Bind `org-cliplink` function to something. For example, put
;; this line in your init file:
;;   (global-set-key (kbd "C-x p i") 'org-cliplink)
;;
;; Then copy any http/https URL to the clipboard, switch to
;; the Emacs window and hit `C-x p i`.

;;; Commentary:
;; 
;; A simple command that takes a URL from the clipboard and inserts an
;; org-mode link with a title of a page found by the URL into the
;; current buffer
;; 
;; This code was a part of my Emacs config almost a year. I decided to
;; publish it as a separate package in case someone needs this feature
;; too.

;;; Code:

(require 'em-glob)
(require 'subr-x) ; for string-trim

(require 'org-cliplink-string)
(require 'org-cliplink-transport)

(defconst org-cliplink-basic-escape-alist
  '(("&quot;" . "\"")             ;; " - double-quote
    ("&amp;" . "&")               ;; & - ampersand
    ("&lt;" . "<")                ;; < - less-than
    ("&gt;" . ">")))              ;; > - greater-than

(defconst org-cliplink-iso8869-1-escape-alist
  '(("&nbsp;" . "\u00A0")                   ;; non-breaking space
    ("&iexcl;" . "\u00A1") ;; inverted exclamation mark
    ("&cent;" . "\u00A2")  ;; cent sign
    ("&pound;" . "\u00A3") ;; pound sign
    ("&curren;" . "\u00A4")               ;; currency sign
    ("&yen;" . "\u00A5")                  ;; yen sign = yuan sign
    ("&brvbar;" . "\u00A6") ;; broken bar = broken vertical bar
    ("&sect;" . "\u00A7")   ;; section sign
    ("&uml;" . "\u00A8")    ;; diaeresis = spacing diaeresis
    ("&copy;" . "\u00A9")   ;; © - copyright sign
    ("&ordf;" . "\u00AA")   ;; feminine ordinal indicator
    ("&laquo;" . "\u00AB") ;; left-pointing double angle quotation mark = left pointing guillemet
    ("&not;" . "\u00AC")   ;; not sign
    ("&shy;" . "\u00AD")   ;; soft hyphen = discretionary hyphen
    ("&reg;" . "\u00AE")   ;; ® - registered trademark sign
    ("&macr;" . "\u00AF") ;; macron = spacing macron = overline = APL overbar
    ("&deg;" . "\u00B0")  ;; degree sign
    ("&plusmn;" . "\u00B1") ;; plus-minus sign = plus-or-minus sign
    ("&sup2;" . "\u00B2") ;; superscript two = superscript digit two = squared
    ("&sup3;" . "\u00B3") ;; superscript three = superscript digit three = cubed
    ("&acute;" . "\u00B4") ;; acute accent = spacing acute
    ("&micro;" . "\u00B5") ;; micro sign
    ("&para;" . "\u00B6")  ;; pilcrow sign = paragraph sign
    ("&middot;" . "\u00B7") ;; middle dot = Georgian comma = Greek middle dot
    ("&cedil;" . "\u00B8")  ;; cedilla = spacing cedilla
    ("&sup1;" . "\u00B9") ;; superscript one = superscript digit one
    ("&ordm;" . "\u00BA") ;; masculine ordinal indicator
    ("&raquo;" . "\u00BB") ;; right-pointing double angle quotation mark = right pointing guillemet
    ("&frac14;" . "\u00BC") ;; vulgar fraction one quarter = fraction one quarter
    ("&frac12;" . "\u00BD") ;; vulgar fraction one half = fraction one half
    ("&frac34;" . "\u00BE") ;; vulgar fraction three quarters = fraction three quarters
    ("&iquest;" . "\u00BF") ;; inverted question mark = turned question mark
    ("&Agrave;" . "\u00C0") ;; À - uppercase A, grave accent
    ("&Aacute;" . "\u00C1") ;; Á - uppercase A, acute accent
    ("&Acirc;" . "\u00C2")  ;; Â - uppercase A, circumflex accent
    ("&Atilde;" . "\u00C3") ;; Ã - uppercase A, tilde
    ("&Auml;" . "\u00C4")   ;; Ä - uppercase A, umlaut
    ("&Aring;" . "\u00C5")  ;; Å - uppercase A, ring
    ("&AElig;" . "\u00C6")  ;; Æ - uppercase AE
    ("&Ccedil;" . "\u00C7") ;; Ç - uppercase C, cedilla
    ("&Egrave;" . "\u00C8") ;; È - uppercase E, grave accent
    ("&Eacute;" . "\u00C9") ;; É - uppercase E, acute accent
    ("&Ecirc;" . "\u00CA")  ;; Ê - uppercase E, circumflex accent
    ("&Euml;" . "\u00CB")   ;; Ë - uppercase E, umlaut
    ("&Igrave;" . "\u00CC") ;; Ì - uppercase I, grave accent
    ("&Iacute;" . "\u00CD") ;; Í - uppercase I, acute accent
    ("&Icirc;" . "\u00CE")  ;; Î - uppercase I, circumflex accent
    ("&Iuml;" . "\u00CF")   ;; Ï - uppercase I, umlaut
    ("&ETH;" . "\u00D0")    ;; Ð - uppercase Eth, Icelandic
    ("&Ntilde;" . "\u00D1") ;; Ñ - uppercase N, tilde
    ("&Ograve;" . "\u00D2") ;; Ò - uppercase O, grave accent
    ("&Oacute;" . "\u00D3") ;; Ó - uppercase O, acute accent
    ("&Ocirc;" . "\u00D4")  ;; Ô - uppercase O, circumflex accent
    ("&Otilde;" . "\u00D5") ;; Õ - uppercase O, tilde
    ("&Ouml;" . "\u00D6")   ;; Ö - uppercase O, umlaut
    ("&times;" . "\u00D7")  ;; multiplication sign
    ("&Oslash;" . "\u00D8") ;; Ø - uppercase O, slash
    ("&Ugrave;" . "\u00D9") ;; Ù - uppercase U, grave accent
    ("&Uacute;" . "\u00DA") ;; Ú - uppercase U, acute accent
    ("&Ucirc;" . "\u00DB")  ;; Û - uppercase U, circumflex accent
    ("&Uuml;" . "\u00DC")   ;; Ü - uppercase U, umlaut
    ("&Yacute;" . "\u00DD") ;; Ý - uppercase Y, acute accent
    ("&THORN;" . "\u00DE")  ;; Þ - uppercase THORN, Icelandic
    ("&szlig;" . "\u00DF")  ;; ß - lowercase sharps, German
    ("&agrave;" . "\u00E0") ;; à - lowercase a, grave accent
    ("&aacute;" . "\u00E1") ;; á - lowercase a, acute accent
    ("&acirc;" . "\u00E2")  ;; â - lowercase a, circumflex accent
    ("&atilde;" . "\u00E3") ;; ã - lowercase a, tilde
    ("&auml;" . "\u00E4")   ;; ä - lowercase a, umlaut
    ("&aring;" . "\u00E5")  ;; å - lowercase a, ring
    ("&aelig;" . "\u00E6")  ;; æ - lowercase ae
    ("&ccedil;" . "\u00E7") ;; ç - lowercase c, cedilla
    ("&egrave;" . "\u00E8") ;; è - lowercase e, grave accent
    ("&eacute;" . "\u00E9") ;; é - lowercase e, acute accent
    ("&ecirc;" . "\u00EA")  ;; ê - lowercase e, circumflex accent
    ("&euml;" . "\u00EB")   ;; ë - lowercase e, umlaut
    ("&igrave;" . "\u00EC") ;; ì - lowercase i, grave accent
    ("&iacute;" . "\u00ED") ;; í - lowercase i, acute accent
    ("&icirc;" . "\u00EE")  ;; î - lowercase i, circumflex accent
    ("&iuml;" . "\u00EF")   ;; ï - lowercase i, umlaut
    ("&eth;" . "\u00F0")    ;; ð - lowercase eth, Icelandic
    ("&ntilde;" . "\u00F1") ;; ñ - lowercase n, tilde
    ("&ograve;" . "\u00F2") ;; ò - lowercase o, grave accent
    ("&oacute;" . "\u00F3") ;; ó - lowercase o, acute accent
    ("&ocirc;" . "\u00F4")  ;; ô - lowercase o, circumflex accent
    ("&otilde;" . "\u00F5") ;; õ - lowercase o, tilde
    ("&ouml;" . "\u00F6")   ;; ö - lowercase o, umlaut
    ("&divide;" . "\u00F7") ;; division sign
    ("&oslash;" . "\u00F8") ;; ø - lowercase o, slash
    ("&ugrave;" . "\u00F9") ;; ù - lowercase u, grave accent
    ("&uacute;" . "\u00FA") ;; ú - lowercase u, acute accent
    ("&ucirc;" . "\u00FB")  ;; û - lowercase u, circumflex accent
    ("&uuml;" . "\u00FC")   ;; ü - lowercase u, umlaut
    ("&yacute;" . "\u00FD") ;; ý - lowercase y, acute accent
    ("&thorn;" . "\u00FE")  ;; þ - lowercase thorn, Icelandic
    ("&yuml;" . "\u00FF"))) ;; ÿ - lowercase y, umlaut

(defconst org-cliplink-html40-extended-escape-alist
  '( ;; <!-- Latin Extended-B -->
    ("&fnof;" . "\u0192") ;; latin small f with hook = function= florin, U+0192 ISOtech -->
    ;; <!-- Greek -->
    ("&Alpha;" . "\u0391") ;; greek capital letter alpha, U+0391 -->
    ("&Beta;" . "\u0392") ;; greek capital letter beta, U+0392 -->
    ("&Gamma;" . "\u0393") ;; greek capital letter gamma,U+0393 ISOgrk3 -->
    ("&Delta;" . "\u0394") ;; greek capital letter delta,U+0394 ISOgrk3 -->
    ("&Epsilon;" . "\u0395") ;; greek capital letter epsilon, U+0395 -->
    ("&Zeta;" . "\u0396") ;; greek capital letter zeta, U+0396 -->
    ("&Eta;" . "\u0397")  ;; greek capital letter eta, U+0397 -->
    ("&Theta;" . "\u0398") ;; greek capital letter theta,U+0398 ISOgrk3 -->
    ("&Iota;" . "\u0399") ;; greek capital letter iota, U+0399 -->
    ("&Kappa;" . "\u039A") ;; greek capital letter kappa, U+039A -->
    ("&Lambda;" . "\u039B") ;; greek capital letter lambda,U+039B ISOgrk3 -->
    ("&Mu;" . "\u039C")     ;; greek capital letter mu, U+039C -->
    ("&Nu;" . "\u039D")     ;; greek capital letter nu, U+039D -->
    ("&Xi;" . "\u039E") ;; greek capital letter xi, U+039E ISOgrk3 -->
    ("&Omicron;" . "\u039F") ;; greek capital letter omicron, U+039F -->
    ("&Pi;" . "\u03A0") ;; greek capital letter pi, U+03A0 ISOgrk3 -->
    ("&Rho;" . "\u03A1") ;; greek capital letter rho, U+03A1 -->
    ;; <!-- there is no Sigmaf, and no U+03A2 character either -->
    ("&Sigma;" . "\u03A3") ;; greek capital letter sigma,U+03A3 ISOgrk3 -->
    ("&Tau;" . "\u03A4")   ;; greek capital letter tau, U+03A4 -->
    ("&Upsilon;" . "\u03A5") ;; greek capital letter upsilon,U+03A5 ISOgrk3 -->
    ("&Phi;" . "\u03A6") ;; greek capital letter phi,U+03A6 ISOgrk3 -->
    ("&Chi;" . "\u03A7") ;; greek capital letter chi, U+03A7 -->
    ("&Psi;" . "\u03A8") ;; greek capital letter psi,U+03A8 ISOgrk3 -->
    ("&Omega;" . "\u03A9") ;; greek capital letter omega,U+03A9 ISOgrk3 -->
    ("&alpha;" . "\u03B1") ;; greek small letter alpha,U+03B1 ISOgrk3 -->
    ("&beta;" . "\u03B2") ;; greek small letter beta, U+03B2 ISOgrk3 -->
    ("&gamma;" . "\u03B3") ;; greek small letter gamma,U+03B3 ISOgrk3 -->
    ("&delta;" . "\u03B4") ;; greek small letter delta,U+03B4 ISOgrk3 -->
    ("&epsilon;" . "\u03B5") ;; greek small letter epsilon,U+03B5 ISOgrk3 -->
    ("&zeta;" . "\u03B6") ;; greek small letter zeta, U+03B6 ISOgrk3 -->
    ("&eta;" . "\u03B7") ;; greek small letter eta, U+03B7 ISOgrk3 -->
    ("&theta;" . "\u03B8") ;; greek small letter theta,U+03B8 ISOgrk3 -->
    ("&iota;" . "\u03B9") ;; greek small letter iota, U+03B9 ISOgrk3 -->
    ("&kappa;" . "\u03BA") ;; greek small letter kappa,U+03BA ISOgrk3 -->
    ("&lambda;" . "\u03BB") ;; greek small letter lambda,U+03BB ISOgrk3 -->
    ("&mu;" . "\u03BC") ;; greek small letter mu, U+03BC ISOgrk3 -->
    ("&nu;" . "\u03BD") ;; greek small letter nu, U+03BD ISOgrk3 -->
    ("&xi;" . "\u03BE") ;; greek small letter xi, U+03BE ISOgrk3 -->
    ("&omicron;" . "\u03BF") ;; greek small letter omicron, U+03BF NEW -->
    ("&pi;" . "\u03C0") ;; greek small letter pi, U+03C0 ISOgrk3 -->
    ("&rho;" . "\u03C1") ;; greek small letter rho, U+03C1 ISOgrk3 -->
    ("&sigmaf;" . "\u03C2") ;; greek small letter final sigma,U+03C2 ISOgrk3 -->
    ("&sigma;" . "\u03C3") ;; greek small letter sigma,U+03C3 ISOgrk3 -->
    ("&tau;" . "\u03C4") ;; greek small letter tau, U+03C4 ISOgrk3 -->
    ("&upsilon;" . "\u03C5") ;; greek small letter upsilon,U+03C5 ISOgrk3 -->
    ("&phi;" . "\u03C6") ;; greek small letter phi, U+03C6 ISOgrk3 -->
    ("&chi;" . "\u03C7") ;; greek small letter chi, U+03C7 ISOgrk3 -->
    ("&psi;" . "\u03C8") ;; greek small letter psi, U+03C8 ISOgrk3 -->
    ("&omega;" . "\u03C9") ;; greek small letter omega,U+03C9 ISOgrk3 -->
    ("&thetasym;" . "\u03D1") ;; greek small letter theta symbol,U+03D1 NEW -->
    ("&upsih;" . "\u03D2") ;; greek upsilon with hook symbol,U+03D2 NEW -->
    ("&piv;" . "\u03D6")   ;; greek pi symbol, U+03D6 ISOgrk3 -->
    ;; <!-- General Punctuation -->
    ("&bull;" . "\u2022") ;; bullet = black small circle,U+2022 ISOpub -->
    ;; <!-- bullet is NOT the same as bullet operator, U+2219 -->
    ("&hellip;" . "\u2026") ;; horizontal ellipsis = three dot leader,U+2026 ISOpub -->
    ("&prime;" . "\u2032") ;; prime = minutes = feet, U+2032 ISOtech -->
    ("&Prime;" . "\u2033") ;; double prime = seconds = inches,U+2033 ISOtech -->
    ("&oline;" . "\u203E") ;; overline = spacing overscore,U+203E NEW -->
    ("&frasl;" . "\u2044") ;; fraction slash, U+2044 NEW -->
    ;; <!-- Letterlike Symbols -->
    ("&weierp;" . "\u2118") ;; script capital P = power set= Weierstrass p, U+2118 ISOamso -->
    ("&image;" . "\u2111") ;; blackletter capital I = imaginary part,U+2111 ISOamso -->
    ("&real;" . "\u211C") ;; blackletter capital R = real part symbol,U+211C ISOamso -->
    ("&trade;" . "\u2122") ;; trade mark sign, U+2122 ISOnum -->
    ("&alefsym;" . "\u2135") ;; alef symbol = first transfinite cardinal,U+2135 NEW -->
    ;; <!-- alef symbol is NOT the same as hebrew letter alef,U+05D0 although the
    ;; same glyph could be used to depict both characters -->
    ;; <!-- Arrows -->
    ("&larr;" . "\u2190") ;; leftwards arrow, U+2190 ISOnum -->
    ("&uarr;" . "\u2191") ;; upwards arrow, U+2191 ISOnum-->
    ("&rarr;" . "\u2192") ;; rightwards arrow, U+2192 ISOnum -->
    ("&darr;" . "\u2193") ;; downwards arrow, U+2193 ISOnum -->
    ("&harr;" . "\u2194") ;; left right arrow, U+2194 ISOamsa -->
    ("&crarr;" . "\u21B5") ;; downwards arrow with corner leftwards= carriage return, U+21B5 NEW -->
    ("&lArr;" . "\u21D0") ;; leftwards double arrow, U+21D0 ISOtech -->
    ;; <!-- ISO 10646 does not say that lArr is the same as the 'is implied by'
    ;; arrow but also does not have any other character for that function.
    ;; So ? lArr canbe used for 'is implied by' as ISOtech suggests -->
    ("&uArr;" . "\u21D1") ;; upwards double arrow, U+21D1 ISOamsa -->
    ("&rArr;" . "\u21D2") ;; rightwards double arrow,U+21D2 ISOtech -->
    ;; <!-- ISO 10646 does not say this is the 'implies' character but does not
    ;; have another character with this function so ?rArr can be used for
    ;; 'implies' as ISOtech suggests -->
    ("&dArr;" . "\u21D3") ;; downwards double arrow, U+21D3 ISOamsa -->
    ("&hArr;" . "\u21D4") ;; left right double arrow,U+21D4 ISOamsa -->
    ;; <!-- Mathematical Operators -->
    ("&forall;" . "\u2200") ;; for all, U+2200 ISOtech -->
    ("&part;" . "\u2202") ;; partial differential, U+2202 ISOtech -->
    ("&exist;" . "\u2203") ;; there exists, U+2203 ISOtech -->
    ("&empty;" . "\u2205") ;; empty set = null set = diameter,U+2205 ISOamso -->
    ("&nabla;" . "\u2207") ;; nabla = backward difference,U+2207 ISOtech -->
    ("&isin;" . "\u2208")  ;; element of, U+2208 ISOtech -->
    ("&notin;" . "\u2209") ;; not an element of, U+2209 ISOtech -->
    ("&ni;" . "\u220B") ;; contains as member, U+220B ISOtech -->
    ;; <!-- should there be a more memorable name than 'ni'? -->
    ("&prod;" . "\u220F") ;; n-ary product = product sign,U+220F ISOamsb -->
    ;; <!-- prod is NOT the same character as U+03A0 'greek capital letter pi'
    ;; though the same glyph might be used for both -->
    ("&sum;" . "\u2211") ;; n-ary summation, U+2211 ISOamsb -->
    ;; <!-- sum is NOT the same character as U+03A3 'greek capital letter sigma'
    ;; though the same glyph might be used for both -->
    ("&minus;" . "\u2212") ;; minus sign, U+2212 ISOtech -->
    ("&lowast;" . "\u2217") ;; asterisk operator, U+2217 ISOtech -->
    ("&radic;" . "\u221A") ;; square root = radical sign,U+221A ISOtech -->
    ("&prop;" . "\u221D")  ;; proportional to, U+221D ISOtech -->
    ("&infin;" . "\u221E") ;; infinity, U+221E ISOtech -->
    ("&ang;" . "\u2220")   ;; angle, U+2220 ISOamso -->
    ("&and;" . "\u2227") ;; logical and = wedge, U+2227 ISOtech -->
    ("&or;" . "\u2228")  ;; logical or = vee, U+2228 ISOtech -->
    ("&cap;" . "\u2229") ;; intersection = cap, U+2229 ISOtech -->
    ("&cup;" . "\u222A") ;; union = cup, U+222A ISOtech -->
    ("&int;" . "\u222B") ;; integral, U+222B ISOtech -->
    ("&there4;" . "\u2234") ;; therefore, U+2234 ISOtech -->
    ("&sim;" . "\u223C") ;; tilde operator = varies with = similar to,U+223C ISOtech -->
    ;; <!-- tilde operator is NOT the same character as the tilde, U+007E,although
    ;; the same glyph might be used to represent both -->
    ("&cong;" . "\u2245") ;; approximately equal to, U+2245 ISOtech -->
    ("&asymp;" . "\u2248") ;; almost equal to = asymptotic to,U+2248 ISOamsr -->
    ("&ne;" . "\u2260")    ;; not equal to, U+2260 ISOtech -->
    ("&equiv;" . "\u2261") ;; identical to, U+2261 ISOtech -->
    ("&le;" . "\u2264") ;; less-than or equal to, U+2264 ISOtech -->
    ("&ge;" . "\u2265") ;; greater-than or equal to,U+2265 ISOtech -->
    ("&sub;" . "\u2282") ;; subset of, U+2282 ISOtech -->
    ("&sup;" . "\u2283") ;; superset of, U+2283 ISOtech -->
    ;; <!-- note that nsup, 'not a superset of, U+2283' is not covered by the
    ;; Symbol font encoding and is not included. Should it be, for symmetry?
    ;; It is in ISOamsn --> <!ENTITY nsub", "8836"},
    ;; not a subset of, U+2284 ISOamsn -->
    ("&sube;" . "\u2286") ;; subset of or equal to, U+2286 ISOtech -->
    ("&supe;" . "\u2287") ;; superset of or equal to,U+2287 ISOtech -->
    ("&oplus;" . "\u2295") ;; circled plus = direct sum,U+2295 ISOamsb -->
    ("&otimes;" . "\u2297") ;; circled times = vector product,U+2297 ISOamsb -->
    ("&perp;" . "\u22A5") ;; up tack = orthogonal to = perpendicular,U+22A5 ISOtech -->
    ("&sdot;" . "\u22C5") ;; dot operator, U+22C5 ISOamsb -->
    ;; <!-- dot operator is NOT the same character as U+00B7 middle dot -->
    ;; <!-- Miscellaneous Technical -->
    ("&lceil;" . "\u2308") ;; left ceiling = apl upstile,U+2308 ISOamsc -->
    ("&rceil;" . "\u2309") ;; right ceiling, U+2309 ISOamsc -->
    ("&lfloor;" . "\u230A") ;; left floor = apl downstile,U+230A ISOamsc -->
    ("&rfloor;" . "\u230B") ;; right floor, U+230B ISOamsc -->
    ("&lang;" . "\u2329") ;; left-pointing angle bracket = bra,U+2329 ISOtech -->
    ;; <!-- lang is NOT the same character as U+003C 'less than' or U+2039 'single left-pointing angle quotation
    ;; mark' -->
    ("&rang;" . "\u232A") ;; right-pointing angle bracket = ket,U+232A ISOtech -->
    ;; <!-- rang is NOT the same character as U+003E 'greater than' or U+203A
    ;; 'single right-pointing angle quotation mark' -->
    ;; <!-- Geometric Shapes -->
    ("&loz;" . "\u25CA") ;; lozenge, U+25CA ISOpub -->
    ;; <!-- Miscellaneous Symbols -->
    ("&spades;" . "\u2660") ;; black spade suit, U+2660 ISOpub -->
    ;; <!-- black here seems to mean filled as opposed to hollow -->
    ("&clubs;" . "\u2663") ;; black club suit = shamrock,U+2663 ISOpub -->
    ("&hearts;" . "\u2665") ;; black heart suit = valentine,U+2665 ISOpub -->
    ("&diams;" . "\u2666") ;; black diamond suit, U+2666 ISOpub -->

    ;; <!-- Latin Extended-A -->
    ("&OElig;" . "\u0152") ;; -- latin capital ligature OE,U+0152 ISOlat2 -->
    ("&oelig;" . "\u0153") ;; -- latin small ligature oe, U+0153 ISOlat2 -->
    ;; <!-- ligature is a misnomer, this is a separate character in some languages -->
    ("&Scaron;" . "\u0160") ;; -- latin capital letter S with caron,U+0160 ISOlat2 -->
    ("&scaron;" . "\u0161") ;; -- latin small letter s with caron,U+0161 ISOlat2 -->
    ("&Yuml;" . "\u0178") ;; -- latin capital letter Y with diaeresis,U+0178 ISOlat2 -->
    ;; <!-- Spacing Modifier Letters -->
    ("&circ;" . "\u02C6") ;; -- modifier letter circumflex accent,U+02C6 ISOpub -->
    ("&tilde;" . "\u02DC") ;; small tilde, U+02DC ISOdia -->
    ;; <!-- General Punctuation -->
    ("&ensp;" . "\u2002") ;; en space, U+2002 ISOpub -->
    ("&emsp;" . "\u2003") ;; em space, U+2003 ISOpub -->
    ("&thinsp;" . "\u2009") ;; thin space, U+2009 ISOpub -->
    ("&zwnj;" . "\u200C") ;; zero width non-joiner,U+200C NEW RFC 2070 -->
    ("&zwj;" . "\u200D") ;; zero width joiner, U+200D NEW RFC 2070 -->
    ("&lrm;" . "\u200E") ;; left-to-right mark, U+200E NEW RFC 2070 -->
    ("&rlm;" . "\u200F") ;; right-to-left mark, U+200F NEW RFC 2070 -->
    ("&ndash;" . "\u2013") ;; en dash, U+2013 ISOpub -->
    ("&mdash;" . "\u2014") ;; em dash, U+2014 ISOpub -->
    ("&lsquo;" . "\u2018") ;; left single quotation mark,U+2018 ISOnum -->
    ("&rsquo;" . "\u2019") ;; right single quotation mark,U+2019 ISOnum -->
    ("&sbquo;" . "\u201A") ;; single low-9 quotation mark, U+201A NEW -->
    ("&ldquo;" . "\u201C") ;; left double quotation mark,U+201C ISOnum -->
    ("&rdquo;" . "\u201D") ;; right double quotation mark,U+201D ISOnum -->
    ("&bdquo;" . "\u201E") ;; double low-9 quotation mark, U+201E NEW -->
    ("&dagger;" . "\u2020") ;; dagger, U+2020 ISOpub -->
    ("&Dagger;" . "\u2021") ;; double dagger, U+2021 ISOpub -->
    ("&permil;" . "\u2030") ;; per mille sign, U+2030 ISOtech -->
    ("&lsaquo;" . "\u2039") ;; single left-pointing angle quotation mark,U+2039 ISO proposed -->
    ;; <!-- lsaquo is proposed but not yet ISO standardized -->
    ("&rsaquo;" . "\u203A") ;; single right-pointing angle quotation mark,U+203A ISO proposed -->
    ;; <!-- rsaquo is proposed but not yet ISO standardized -->
    ("&euro;" . "\u20AC"))) ;; -- euro sign, U+20AC NEW -->

(defun org-cliplink-escape-numeric-match (s)
  (char-to-string
   (string-to-number
    (match-string 1 s))))

(defvar org-cliplink-escape-alist
  (append org-cliplink-basic-escape-alist
          org-cliplink-iso8869-1-escape-alist
          org-cliplink-html40-extended-escape-alist
          '(("\\[" . "{")
            ("\\]" . "}")
            ("&#\\([0-9]+\\);" . org-cliplink-escape-numeric-match))))

(defgroup org-cliplink nil
  "A simple command that takes a URL from the clipboard and inserts an
org-mode link with a title of a page found by the URL into the current
buffer."
  :prefix "org-cliplink-"
  :group 'wp
  :link '(url-link "https://github.com/rexim/org-cliplink"))

(defcustom org-cliplink-max-length 80
  "Max length of the title.
Org-cliplink cuts any title that exceeds the limit. Minimum
possible value is 4."
  :group 'org-cliplink
  :type '(choice integer (const :tag "off" nil)))

(defcustom org-cliplink-ellipsis "..."
  "String to mark the end of truncated titles"
  :group 'org-cliplink
  :type 'string)

(defcustom org-cliplink-secrets-path "~/.org-cliplink-secrets.el"
  "Path to file that keeps your org-cliplink related secrets.
It can be any sensitive information like password to different
services."
  :group 'org-cliplink
  :type 'string)

(defcustom org-cliplink-title-replacements
  '(("https://github.com/.+/?"
     ("\\(.*\\) · \\(?:Issue\\|Pull Request\\) #\\([0-9]+\\) · \\(.*\\) · GitHub"
      "\\3#\\2 \\1"))
    ("https://twitter.com/.+/status/[[:digit:]]+/?"
     (".+ on Twitter: \\(.+\\)" "\\1")))
  "A list of rules for formatting titles.

Each entry has the form (URL-REGEXP . (TITLE-REGEXP . REPLACEMENT))."
  :group 'org-cliplink
  :type '(repeat (list string (list string string))))

(defcustom org-cliplink-transport-implementation 'url-el
  "The transport implementation.
Supported transports are `url-el' and `curl'. `curl' is
experimental so use it on your own risk."
  :group 'org-cliplink
  :type 'symbol)

(defcustom org-cliplink-curl-transport-arguments '()
  "Additional arguments for cURL.
Used when the current transport implementation is set to
`curl'."
  :group 'org-cliplink
  :type '(repeat string))

(defcustom org-cliplink-simpleclip-source nil
  "Clipboard source.
Non-nil means use system clipboard as source.
The clipboard content will be provided by `simpleclip',
requiring simpleclip.el to be installed.

When nil, use the first element of kill-ring as source"
  :group 'org-cliplink
  :type 'boolean)

(defun org-cliplink-clipboard-content ()
  (let ((content (if (and org-cliplink-simpleclip-source
                          (fboundp 'simpleclip-get-contents))
                     (simpleclip-get-contents)
                   (current-kill 0))))
    (string-trim
     (substring-no-properties content))))

(defun org-cliplink-parse-raw-header (raw-header)
  (let ((start 0)
        (result-header nil))
    (while (string-match "^\\(.+?\\): \\(.+?\\)\r?$" raw-header start)
      (let ((header-name (match-string 1 raw-header))
            (header-value (match-string 2 raw-header)))
        (setq result-header
              (cons (cons header-name header-value) result-header))
        (setq start (match-end 2))))
    result-header))

(defun org-cliplink-parse-response ()
  (goto-char (point-min))
  (search-forward-regexp "^\r?$")
  (let ((content (buffer-substring (+ (point) 1) (point-max)))
        (raw-header (buffer-substring (point-min) (point))))
    (cons (org-cliplink-parse-raw-header raw-header)
          content)))

(defun org-cliplink-extract-title-from-html (html)
  (let* ((case-fold-search t)
         (start0 (string-match "<title" html))
         (start (when start0 (string-match ">" html start0)))
         (end (string-match "</title>" html))
         (chars-to-skip (length ">")))
    (if (and start end (< start end))
        (substring html (+ start chars-to-skip) end)
      nil)))

(defun org-cliplink-escape-html4 (s)
  (when s
    (let ((case-replace nil)
          (case-fold-search nil)
          (result s))
      (dolist (x org-cliplink-escape-alist result)
        (setq result (replace-regexp-in-string (car x) (cdr x) result))))))

(defun org-cliplink-title-for-url (url title)
  "Replace title using configured rules.

Find the first entry (URL-REGEXP (TITLE-REGEXP REPLACEMENT)) in
`org-cliplink-title-replacements' where URL-REGEXP matches URL,
and return TITLE with any matches for TITLE-REGEXP replaced by
REPLACEMENT.

If no URL-REGEXP matches URL, or if the first matching entry's
TITLE-REGEXP does not match TITLE, return the original TITLE."
  (save-match-data
    (cl-loop for (url-re (title-re rep)) in org-cliplink-title-replacements
             when (string-match url-re url)
             return (replace-regexp-in-string title-re rep title)
             finally return title)))

(defun org-cliplink-org-mode-link-transformer (url title)
  (if title
      (format "[[%s][%s]]" url (org-cliplink-elide-string
                                (org-cliplink-escape-html4
                                 (org-cliplink-title-for-url url title))
                                org-cliplink-max-length))
    (format "[[%s]]" url)))

(defun org-cliplink-insert-org-mode-link-callback (url title)
  (insert (org-cliplink-org-mode-link-transformer url title)))

(defun org-cliplink-uncompress-gziped-text (text)
  (let ((filename (make-temp-file "org-cliplink" nil ".gz")))
    (write-region text nil filename)
    (with-auto-compression-mode
      (with-temp-buffer
        (insert-file-contents filename)
        (delete-file filename)
        (buffer-string)))))

(defun org-cliplink-extract-and-prepare-title-from-current-buffer ()
  (let* ((response (org-cliplink-parse-response))
         (header (car response))
         (content (if (and (string= "gzip" (cdr (assoc "Content-Encoding" header)))
                           (not (string= "gzip" url-mime-encoding-string)))
                      (org-cliplink-uncompress-gziped-text (cdr response))
                    (cdr response)))
         (decoded-content (decode-coding-string content (quote utf-8))))
    (org-cliplink-straight-string
     (org-cliplink-extract-title-from-html
      decoded-content))))

(defun org-cliplink-read-secrets ()
  (when (file-exists-p org-cliplink-secrets-path)
    (with-temp-buffer
      (insert-file-contents org-cliplink-secrets-path)
      (car (read-from-string (buffer-string))))))

(defun org-cliplink-check-basic-auth-for-url (url)
  (let ((basic-auth-secrets (plist-get (org-cliplink-read-secrets)
                                       :basic-auth))
        (result nil))
    (while (and (not result) basic-auth-secrets)
      (let ((secret (car basic-auth-secrets)))
        (when (string-match (eshell-glob-regexp
                             (plist-get secret :url-pattern)) url)
          (setq result secret)))
      (pop basic-auth-secrets))
    result))

;;;###autoload
(defun org-cliplink-retrieve-title (url title-callback)
  (let* ((dest-buffer (current-buffer))
         (basic-auth (org-cliplink-check-basic-auth-for-url url))
         (url-retrieve-callback
          (lambda (status)
            (ignore status)
            (let ((title (org-cliplink-extract-and-prepare-title-from-current-buffer)))
              (with-current-buffer dest-buffer
                (funcall title-callback url title))))))
    (if (equal 'curl org-cliplink-transport-implementation)
        (org-cliplink-http-get-request--curl url url-retrieve-callback basic-auth
                                             org-cliplink-curl-transport-arguments)
      (org-cliplink-http-get-request--url-el url url-retrieve-callback basic-auth))))

;;;###autoload
(defun org-cliplink-insert-transformed-title (url transformer)
  "Takes the URL, asynchronously retrieves the title and applies
a custom TRANSFORMER which transforms the url and title and insert
the required text to the current buffer."
  (org-cliplink-retrieve-title
   url
   (lambda (url title)
     (insert (funcall transformer url title)))))

;;;###autoload
(defun org-cliplink-retrieve-title-synchronously (url)
  (when (member (url-type (url-generic-parse-url url))
                '("http" "https"))
    (let ((response-buffer (url-retrieve-synchronously url t)))
      (when response-buffer
        (with-current-buffer response-buffer
          (org-cliplink-elide-string
           (org-cliplink-escape-html4
            (org-cliplink-extract-and-prepare-title-from-current-buffer))
           org-cliplink-max-length))))))

;;;###autoload
(defun org-cliplink ()
  "Takes a URL from the clipboard and inserts an org-mode link
with the title of a page found by the URL into the current
buffer"
  (interactive)
  (org-cliplink-insert-transformed-title (org-cliplink-clipboard-content)
                                         'org-cliplink-org-mode-link-transformer))

;;;###autoload
(defun org-cliplink-capture ()
  "org-cliplink version for org-capture templates.
Makes synchronous request. Returns the link instead of inserting
it to the current buffer. Doesn't support Basic Auth. Doesn't
support cURL transport."
  (interactive)
  (let ((url (org-cliplink-clipboard-content)))
    (org-cliplink-org-mode-link-transformer url
     (org-cliplink-retrieve-title-synchronously url))))

(provide 'org-cliplink)

;;; org-cliplink.el ends here
