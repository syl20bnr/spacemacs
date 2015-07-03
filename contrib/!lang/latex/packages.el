;;; packages.el --- Latex Layer packages File for Spacemacs
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

(setq latex-packages
  '(
    auctex
    auctex-latexmk
    company
    company-auctex
    evil-matchit
    flycheck
    flyspell
    smartparens
    yasnippet
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
            TeX-PDF-mode t
            ;; Synctex support
            TeX-source-correlate-mode t
            TeX-source-correlate-start-server nil
            ;; Setup reftex style (RefTeX is supported through extension)
            reftex-use-fonts t
            ;; Don't insert line-break at inline math
            LaTeX-fill-break-at-separators nil)
      (when latex-enable-auto-fill
        (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
      (add-hook 'LaTeX-mode-hook 'latex-math-mode))
    :config
    (progn
      ;; Key bindings for plain TeX
      (evil-leader/set-key-for-mode 'tex-mode
        "m\\" 'TeX-insert-macro
        "mb" 'latex/build
        "mC" 'TeX-command-master
        ;; Find a way to rebind tex-fonts
        "mf" 'TeX-font
        "mv" 'TeX-view)

      ;; Key bindings for LaTeX
      (evil-leader/set-key-for-mode 'latex-mode
        "m\\" 'TeX-insert-macro
        "mb" 'latex/build
        "mc" 'LaTeX-close-environment
        "mC" 'TeX-command-master
        "me" 'LaTeX-environment
        ;; Find a way to rebind tex-fonts
        "mf" 'TeX-font
        "mhd" 'TeX-doc
        "mi" 'LaTeX-insert-item
        ;; TeX-doc is a very slow function
        "mpb" 'preview-buffer
        "mpc" 'preview-clearout
        "mpd" 'preview-document
        "mpe" 'preview-environment
        "mpf" 'preview-cache-preamble
        "mpp" 'preview-at-point
        "mpr" 'preview-region
        "mps" 'preview-section
        "mv" 'TeX-view))))

(when (string= latex-build-command "LatexMk")
(defun latex/init-auctex-latexmk ()
  (use-package auctex-latexmk
    :defer t
    :init (add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup))))

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
        (push '(company-auctex-macros company-auctex-symbols company-auctex-environments)
              company-backends-LaTeX-mode)))))

(defun latex/post-init-evil-matchit ()
  (add-hook 'LaTeX-mode-hook 'evil-matchit-mode))

(defun latex/post-init-flycheck ()
  (add-hook 'LaTeX-mode-hook 'flycheck-mode))

(defun latex/post-init-flyspell ()
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(defun latex/post-init-smartparens ()
  (add-hook 'LaTeX-mode-hook 'smartparens-mode))

(defun latex/post-init-yasnippet ()
  (add-hook 'LaTeX-mode-hook 'spacemacs/load-yasnippet))
