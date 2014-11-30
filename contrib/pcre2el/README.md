# RegeXp Translator Mode

This contrib layer sets up pcre2el or rxt (RegeXp Translator or RegeXp Tools) which is a utility for working with regular expressions in Emacs, to parse, convert, and font-lock PCRE, Emacs and rx regexps.

Using pcre2el you can convert an Emacs Regexp to pcre (and back) (oh and output as rx too.)

## Generate all matching strings (productions)

Occasionally you come across a regexp which is designed to match a finite set of strings, e.g. a set of keywords, and it would be useful to recover the original set. (In Emacs you can generate such regexps using `regexp-opt'). The commands `rxt-convert-to-strings' (`C-c /′'), `rxt-pcre-to-strings' (`C-c / p ′') or `rxt-elisp-to-strings' (`C-c / e ′') accomplish this by generating all the matching strings ("productions") of a regexp.  (The productions are copied to the kill ring as a Lisp list).

An example in Lisp code:

    : (regexp-opt '("cat" "caterpillar" "catatonic"))
    :    ;; => "\\(?:cat\\(?:atonic\\|erpillar\\)?\\)"
    : (rxt-elisp-to-strings "\\(?:cat\\(?:atonic\\|erpillar\\)?\\)")
    :     ;; => '("cat" "caterpillar" "catatonic")

##RE-Builder support

The Emacs RE-Builder is a useful visual tool which allows using several different built-in syntaxes via =reb-change-syntax= (=C-c TAB=). It supports Elisp read and literal syntax and =rx=, but it     can only convert from the symbolic forms to Elisp, not the other way. This package hacks the RE-Builder to also work with emulated     PCRE syntax, and to convert transparently between Elisp, PCRE and rx syntaxes. PCRE mode reads a delimited Perl-like literal of the form =/ ... /=, and it should correctly support using the =x= and =s= flags.


##Use from Lisp

Example of using the conversion functions:

    (rxt-pcre-to-elisp "(abc|def)\\w+\\d+")
       ;; => "\\(\\(?:abc\\|def\\)\\)[_[:alnum:]]+[[:digit:]]+"

## Keybindings

pcre2el defines keybindings under `C-c /` so we'll define them under out perspective map `<SPC> R`

