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

(setq latex-post-extensions '(emacs-builtin-reftex))

(defun latex/init-emacs-builtin-reftex ()
  "Initialize reftex"
  
  (add-hook 'reftex-toc-mode-hook 'turn-on-evil-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX '(nil nil t t t))

  "ric"  'reftex-citation
  "rsq"  'reftex-query-replace-document
  "rsg"  'reftex-grep-document
  "riI"  'reftex-index-selection-or-word
  "rvi"  'reftex-display-index
  "rii"  'reftex-index
  "ril"  'reftex-label
  "rip"  'reftex-index-phrase-selection-or-word
  "rgb"  'reftex-index-visit-phrases-buffer
  "rir"  'reftex-reference
  "rsd"  'reftex-search-document
  "ria"  'reftex-index-phrases-apply-to-region ;; 
  "rvt"  'reftex-toc
  "rvT"  'reftex-toc-recenter
  "rsd"  'reftex-find-duplicate-labels
  "rit"  'reftex-create-tags-file
  "rmb"  'reftex-create-bibtex-file
  "rpf"  'reftex-parse-one
  "rpd"  'reftex-parse-all
  "rsa"  'tags-query-replace
  "rw"   'reftex-save-all-document-buffers
  "rns"  'reftex-renumber-simple-labels
  "rgl"  'reftex-goto-label
  "rvc"  'reftex-view-crossref)
  ; =======
  ; = TOC =
  ; =======
  (spacemacs/set-leader-keys-for-major-mode 'reftex-toc-mode
  "vl"  'reftex-toc-view-line
  "gl"  'reftex-toc-goto-line
  "gL"  'reftex-toc-goto-line-and-hide
  "vc"  'reftex-toc-show-calling-point
  "q"  'reftex-toc-quit
  "p"  'reftex-toc-promote
  "d"  'reftex-toc-demote
  "rl"  'reftex-toc-rename-label
  "vi"  'reftex-toc-display-index
  "ge"  'reftex-toc-external
  "et"  'revert-buffer
  "md"  'reftex-toc-Rescan
  "tb"  'reftex-toc-toggle-file-boundary
  "tl"  'reftex-toc-toggle-labels
  "ti"  'reftex-toc-toggle-index
  ))
