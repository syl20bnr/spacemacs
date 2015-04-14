;;; extensions.el --- reftex Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar reftex-pre-extensions
  '(
    ;; pre extension reftexs go here
    )
  "List of all extensions to load before the packages.")

(defvar reftex-post-extensions
  '(
    reftex
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function reftex/init-<extension-reftex>
;;
(defun reftex/init-reftex ()
  "Initialize reftex"
  (when (configuration-layer/layer-usedp 'auctex)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t))
  (setq spacemacs/key-binding-prefixes '(("mr" . "RefTeX")))
  (evil-leader/set-key-for-mode 'latex-mode
        "m&" 'reftex-view-crossref
        "m(" 'reftex-label
        "m)" 'reftex-reference
        "m-" 'reftex-toc-recenter
        "m/" 'reftex-index-selection-or-word
        "m<" 'reftex-index
        "m=" 'reftex-toc
        "m>" 'reftex-display-index
        "m[" 'reftex-citation
        "m\\" 'reftex-index-phrase-selection-or-word
        "m|" 'reftex-index-visit-phrases-buffer

        "mrt" 'reftex-toc
        "mrl" 'reftex-label
        "mrr" 'reftex-reference
        "mrc" 'reftex-citation
        "mrv" 'reftex-view-crossref
        "mrs" 'reftex-search-document
        "mrg" 'reftex-grep-document
)
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
