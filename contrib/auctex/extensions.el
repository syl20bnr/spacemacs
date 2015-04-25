;;; extensions.el --- Auctex Layer Extensions File for Spacemacs
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

(setq auctex-post-extensions '(emacs-builtin-reftex))

(defun auctex/init-emacs-builtin-reftex ()
  "Initialize reftex"

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)

  ;; not supported for now
  ;; (setq spacemacs/key-binding-prefixes '(("mr" . "RefTeX")))

  (evil-leader/set-key-for-mode 'LaTeX-mode
    "mrc"    'reftex-citation
    "mrg"    'reftex-grep-document
    "mri"    'reftex-index-selection-or-word
    "mrI"    'reftex-display-index
    "mr C-i" 'reftex-index
    "mrl"    'reftex-label
    "mrp"    'reftex-index-phrase-selection-or-word
    "mrP"    'reftex-index-visit-phrases-buffer
    "mrr"    'reftex-reference
    "mrs"    'reftex-search-document
    "mrt"    'reftex-toc
    "mrT"    'reftex-toc-recenter
    "mrv"    'reftex-view-crossref))
