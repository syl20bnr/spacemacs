# cider-eval-sexp-fu

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [cider-eval-sexp-fu](#cider-eval-sexp-fu)
    - [Install](#install)
    - [Usage](#usage)
    - [Customization](#customization)

<!-- markdown-toc end -->

eval-sexp-fu.el extensions for [CIDER][].
This package briefly highlights evaluated sexps in a clojure buffer
connected to an nREPL via [CIDER][].

## Install

The package is not yet available in [MELPA][] repositories.
Check back soon for news.

You can easily install it by opening `cider-eval-sexp-fu.el` in Emacs and call
`package-install-from-buffer`.

## Usage

Just require it:

```elisp
(require 'cider-eval-sexp-fu)
```

## Customization

Customization is done via [eval-sexp-fu][].

[MELPA]: http://melpa.org/
[eval-sexp-fu]: https://github.com/hchbaw/eval-sexp-fu.el
[CIDER]: https://github.com/clojure-emacs/cider
