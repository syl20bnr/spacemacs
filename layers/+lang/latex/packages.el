;;; packages.el --- Latex Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq latex-packages
  '(
    auctex
    auctex-latexmk
    company
    company-auctex
    evil-matchit
    (reftex :location built-in)
    flycheck
    flyspell
    smartparens
    typo
    yasnippet
    which-key
    ))

(defun latex/init-auctex ()
  (use-package tex
    :defer t
    :init
    (progn
      (setq TeX-command-default latex-build-command
            TeX-auto-save t
            TeX-parse-self t
            TeX-syntactic-comment t
            ;; Synctex support
            TeX-source-correlate-start-server nil
            ;; Don't insert line-break at inline math
            LaTeX-fill-break-at-separators nil)
      (when latex-enable-auto-fill
        (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
      (when latex-enable-folding
        (add-hook 'LaTeX-mode-hook 'TeX-fold-mode))
      (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
      (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
      (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))
    :config
    (progn
      ;; Key bindings for plain TeX
      (dolist (mode '(tex-mode latex-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "\\"  'TeX-insert-macro           ;; C-c C-m
          "-"   'TeX-recenter-output-buffer ;; C-c C-l
          "b"   'latex/build
          "k"   'TeX-kill-job               ;; C-c C-k
          "l"   'TeX-recenter-output-buffer ;; C-c C-l
          "m"   'TeX-insert-macro           ;; C-c C-m
          "v"   'TeX-view                   ;; C-c C-v
          ;; TeX-doc is a very slow function
          "hd"  'TeX-doc
          "xb"  'latex/font-bold
          "xc"  'latex/font-code
          "xe"  'latex/font-emphasis
          "xi"  'latex/font-italic
          "xr"  'latex/font-clear
          "xo"  'latex/font-oblique
          "xfc" 'latex/font-small-caps
          "xff" 'latex/font-sans-serif
          "xfr" 'latex/font-serif)
        (when dotspacemacs-major-mode-emacs-leader-key
          (spacemacs/set-leader-keys-for-major-mode mode
            dotspacemacs-major-mode-emacs-leader-key 'TeX-command-master))
        (when dotspacemacs-major-mode-leader-key
          (spacemacs/set-leader-keys-for-major-mode mode
            dotspacemacs-major-mode-leader-key 'TeX-command-master))
        (when latex-enable-folding
          (spacemacs/set-leader-keys-for-major-mode mode
            "z=" 'TeX-fold-math
            "zb" 'TeX-fold-buffer
            "ze" 'TeX-fold-env
            "zm" 'TeX-fold-macro
            "zr" 'TeX-fold-region))
        (spacemacs/declare-prefix-for-mode mode "mx"  "text/fonts")
        (spacemacs/declare-prefix-for-mode mode "mz"  "fold"))

      ;; Key bindings specific to LaTeX
      (spacemacs/set-leader-keys-for-major-mode 'latex-mode
        "*"   'LaTeX-mark-section      ;; C-c *
        "."   'LaTeX-mark-environment  ;; C-c .
        "c"   'LaTeX-close-environment ;; C-c ]
        "e"   'LaTeX-environment       ;; C-c C-e
        "i"   'LaTeX-insert-item       ;; C-c C-j
        "s"   'LaTeX-section           ;; C-c C-s
        "pb"  'preview-buffer
        "pc"  'preview-clearout
        "pd"  'preview-document
        "pe"  'preview-environment
        "pf"  'preview-cache-preamble
        "pp"  'preview-at-point
        "pr"  'preview-region
        "ps"  'preview-section
        "xB"  'latex/font-medium
        "xr"  'latex/font-clear
        "xfa" 'latex/font-calligraphic
        "xfn" 'latex/font-normal
        "xfu" 'latex/font-upright)
      (spacemacs/declare-prefix-for-mode 'latex-mode "mp"  "preview"))))


(when (string= latex-build-command "LatexMk")
  (defun latex/init-auctex-latexmk ()
    (use-package auctex-latexmk
      :defer t
      :init
      (progn
        (setq auctex-latexmk-inherit-TeX-PDF-mode t)
        (spacemacs|use-package-add-hook tex
          :post-config
          (auctex-latexmk-setup))))))

(defun latex/init-reftex ()
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t)
  (spacemacs/declare-prefix-for-mode 'latex-mode "mr"  "reftex")
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "rc"    'reftex-citation
    "rg"    'reftex-grep-document
    "ri"    'reftex-index-selection-or-word
    "rI"    'reftex-display-index
    "r TAB" 'reftex-index
    "rl"    'reftex-label
    "rp"    'reftex-index-phrase-selection-or-word
    "rP"    'reftex-index-visit-phrases-buffer
    "rr"    'reftex-reference
    "rs"    'reftex-search-document
    "rt"    'reftex-toc
    "rT"    'reftex-toc-recenter
    "rv"    'reftex-view-crossref))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun latex/post-init-company ()
    (spacemacs|add-company-hook LaTeX-mode))

  (defun latex/init-company-auctex ()
    (use-package company-auctex
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (progn
        (push 'company-auctex-labels company-backends-LaTeX-mode)
        (push 'company-auctex-bibs company-backends-LaTeX-mode)
        (push '(company-auctex-macros
                company-auctex-symbols
                company-auctex-environments) company-backends-LaTeX-mode)))))

(defun latex/post-init-evil-matchit ()
  (add-hook 'LaTeX-mode-hook 'evil-matchit-mode))

(defun latex/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'LaTeX-mode-hook))

(defun latex/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'LaTeX-mode-hook))

(defun latex/post-init-smartparens ()
  (add-hook 'LaTeX-mode-hook 'smartparens-mode))

(defun latex/post-init-typo ()
  ;; Typo mode isn't useful for LaTeX.
  (defun spacemacs//disable-typo-mode ()
    (typo-mode -1))
  (add-hook 'LaTeX-mode-hook 'spacemacs//disable-typo-mode))

(defun latex/post-init-yasnippet ()
  (add-hook 'LaTeX-mode-hook 'spacemacs/load-yasnippet))

(defun latex/post-init-which-key ()
  (push '("\\`latex/font-\\(.+\\)\\'" . "\\1")
        which-key-description-replacement-alist))
