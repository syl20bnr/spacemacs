;;; packages.el --- Latex Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst latex-packages
  '(
    auctex
    (auctex-latexmk :toggle (string= "LatexMk" latex-build-command))
    company
    math-symbol-lists
    (company-math :requires company math-symbol-lists)
    (company-auctex :requires company)
    (company-reftex :requires company)
    counsel-gtags
    evil-matchit
    flycheck
    flyspell
    ggtags
    helm-gtags
    (lsp-latex :requires lsp-mode)
    (magic-latex-buffer :toggle latex-enable-magic)
    smartparens
    (reftex :location built-in)
    typo
    which-key
    yasnippet))

(defun latex/post-init-company ()
  (add-hook  'latex-mode-local-vars-hook #'spacemacs//latex-setup-company))

(defun latex/init-auctex ()
  (use-package tex
    :defer t
    :init
    (progn
      (setq TeX-command-default latex-build-command
            TeX-engine latex-build-engine
            TeX-auto-save t
            TeX-parse-self t
            TeX-syntactic-comment t
            ;; Synctex support
            TeX-source-correlate-start-server nil
            ;; Don't insert line-break at inline math
            LaTeX-fill-break-at-separators nil)
      (when latex-enable-auto-fill
        (add-hook 'latex-mode-hook 'latex/auto-fill-mode))
      (when latex-enable-folding
        (add-hook 'latex-mode-hook 'tex-fold-mode))
      (add-hook 'latex-mode-hook 'latex-math-mode)
      (add-hook 'latex-mode-hook 'TeX-source-correlate-mode)
      (add-hook 'latex-mode-hook 'TeX-PDF-mode)
      (add-hook 'latex-mode-hook #'spacemacs//latex-setup-backend)
      (when latex-refresh-preview
        (add-hook 'doc-view-mode-hook 'auto-revert-mode)))
    :config
    (progn
      ;; otherwise `, p` preview commands doesn't work
      (require 'preview)

      (spacemacs//latex-setup-binding
       ;; prefix for plain TeX
       '("mxf" "fonts")
       ;; key bindings for plain TeX
       '("\\"  TeX-insert-macro                   ;; C-c C-m
         "-"   TeX-recenter-output-buffer         ;; C-c C-l
         "%"   TeX-comment-or-uncomment-paragraph ;; C-c %
         ";"   comment-or-uncomment-region        ;; C-c ; or C-c :
         "k"   TeX-kill-job                       ;; C-c C-k
         "l"   TeX-recenter-output-buffer         ;; C-c C-l
         "m"   TeX-insert-macro                   ;; C-c C-m
         "n"   TeX-next-error                     ;; C-c `
         "N"   TeX-previous-error                 ;; M-g p
         "v"   TeX-view                           ;; C-c C-v
         ;; TeX-doc is a very slow function
         "hd"  TeX-doc
         "xb"  latex/font-bold
         "xc"  latex/font-code
         "xe"  latex/font-emphasis
         "xi"  latex/font-italic
         "xr"  latex/font-clear
         "xo"  latex/font-oblique
         ;; fonts
         "xfc" latex/font-small-caps
         "xff" latex/font-sans-serif
         "xfr" latex/font-serif)
       ;; prefix specific to LaTeX
       '("mi" "insert"
         "mp" "preview"
         "mf" "fill")
       ;; key bindings specific to LaTeX
       '("*"   LaTeX-mark-section     ;; C-c *
         "."   LaTeX-mark-environment ;; C-c .
         ;; insert
         "ii"  LaTeX-insert-item     ;; C-c C-j

         "s"   LaTeX-section          ;; C-c C-s
         ;; fill
         "fe"  LaTeX-fill-environment ;; C-c C-q C-e
         "fp"  LaTeX-fill-paragraph   ;; C-c C-q C-p
         "fr"  LaTeX-fill-region      ;; C-c C-q C-r
         "fs"  LaTeX-fill-section     ;; C-c C-q C-s
         ;; preview
         "pb"  preview-buffer
         "pc"  preview-clearout
         "pd"  preview-document
         "pe"  preview-environment
         "pf"  preview-cache-preamble
         "pp"  preview-at-point
         "pr"  preview-region
         "ps"  preview-section

         "xB"  latex/font-medium
         "xr"  latex/font-clear
         ;; fonts
         "xfa" latex/font-calligraphic
         "xfn" latex/font-normal
         "xfu" latex/font-upright)
       ;; prefix for non-lsp LaTeX
       '("mh" "help"
         "mx" "text/fonts")
       ;; bindings for non-lsp LaTeX
       '("a"   TeX-command-run-all     ;; C-c C-a
         "b"   latex/build
         "c"   LaTeX-close-environment ;; C-c ]
         "e"   LaTeX-environment)      ;; C-c C-e
       ;; bindings for lsp LaTeX
       '("au"  TeX-command-run-all
         "c"   latex/build
         "ic"  LaTeX-close-environment ;; C-c ]
         "ie"  LaTeX-environment))     ;; C-c C-e

      (dolist (mode '(tex-mode latex-mode context-mode))
        (when dotspacemacs-major-mode-emacs-leader-key
          (spacemacs/set-leader-keys-for-major-mode mode
            dotspacemacs-major-mode-emacs-leader-key 'TeX-command-master))
        (when dotspacemacs-major-mode-leader-key
          (spacemacs/set-leader-keys-for-major-mode mode
            dotspacemacs-major-mode-leader-key 'TeX-command-master))
        (when latex-enable-folding
          (spacemacs/set-leader-keys-for-major-mode mode
            ;; the following commands are mostly not autoloaded, but that's fine
            ;; because `tex-fold-mode' is added to `latex-mode-hook'
            "z=" 'TeX-fold-math
            "zb" 'TeX-fold-buffer
            "zB" 'TeX-fold-clearout-buffer
            "ze" 'TeX-fold-env
            "zI" 'TeX-fold-clearout-item
            "zm" 'TeX-fold-macro
            "zp" 'TeX-fold-paragraph
            "zP" 'TeX-fold-clearout-paragraph
            "zr" 'TeX-fold-region
            "zR" 'TeX-fold-clearout-region
            "zz" 'TeX-fold-dwim)
          (spacemacs/declare-prefix-for-mode mode "mz" "fold"))))))

(defun latex/pre-init-auctex-latexmk ()
  (spacemacs|use-package-add-hook tex
    :post-config
    (auctex-latexmk-setup)))

(defun latex/init-auctex-latexmk ()
  (use-package auctex-latexmk
    :defer t
    :init (setq auctex-latexmk-inherit-TeX-PDF-mode t)))

(defun latex/post-init-evil-matchit ()
  (add-hook 'latex-mode-hook 'evil-matchit-mode))

(defun latex/post-init-flycheck ()
  (spacemacs/enable-flycheck 'latex-mode))

(defun latex/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'latex-mode-hook))

(defun latex/init-reftex ()
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t)
  (spacemacs//latex-reftex-setup-binding
   "reftex"
   '("c"    reftex-citation
     "g"    reftex-grep-document
     "i"    reftex-index-selection-or-word
     "I"    reftex-display-index
     " TAB" reftex-index
     "l"    reftex-label
     "p"    reftex-index-phrase-selection-or-word
     "P"    reftex-index-visit-phrases-buffer
     "r"    reftex-reference
     "s"    reftex-search-document
     "t"    reftex-toc
     "T"    reftex-toc-recenter
     "v"    reftex-view-crossref)))

(defun latex/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'latex-mode))

(defun latex/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'latex-mode))

(defun latex/post-init-ggtags ()
  (add-hook 'latex-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun latex/post-init-smartparens ()
  (add-hook 'latex-mode-hook #'spacemacs//activate-smartparens))

(defun latex/post-init-typo ()
  ;; Typo mode isn't useful for LaTeX.
  (defun spacemacs//disable-typo-mode ()
    (typo-mode -1))
  (add-hook 'latex-mode-hook 'spacemacs//disable-typo-mode))

(defun latex/post-init-yasnippet ()
  (add-hook 'latex-mode-hook 'spacemacs/load-yasnippet))

(defun latex/post-init-which-key ()
  (push '((nil . "\\`latex/font-\\(.+\\)\\'") . (nil . "\\1"))
        which-key-replacement-alist))

(defun latex/init-magic-latex-buffer ()
  (use-package magic-latex-buffer
    :defer t
    :init
    (progn
      (add-hook 'TeX-update-style-hook 'magic-latex-buffer)
      (setq magic-latex-enable-block-highlight t
            magic-latex-enable-suscript t
            magic-latex-enable-pretty-symbols t
            magic-latex-enable-block-align nil
            magic-latex-enable-inline-image nil))))

(defun latex/init-lsp-latex ()
  (use-package lsp-latex
    :defer t))

(defun latex/init-math-symbol-lists ()
  (use-package math-symbol-lists
    :defer t))

(defun latex/init-company-math ()
  (use-package company-math
    :defer t))

(defun latex/init-company-auctex ()
  (use-package company-auctex
    :defer t))

(defun latex/init-company-reftex ()
  (use-package company-reftex
    :defer t))
