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
    magic-latex-buffer
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
      (spacemacs/set-leader-keys-for-major-mode 'tex-mode
          ;; build/compile/typeset
          "b" 'bq-latex/build ; left for historical reason
          "cc" 'bq-latex/build
          "c:" 'TeX-command-master
          "cr" 'TeX-command-region
          "cb" 'TeX-command-buffer
          ;;"cs" 'Tex-command-sentinel
          "dc" 'TeX-clean
          "de" 'TeX-error-overview
          "dk" 'TeX-kill-job
          "do" 'TeX-recenter-output-buffer
          "dn" 'TeX-next-error
          "dN" 'TeX-previous-error
          "gh" 'TeX-home-buffer
          ;; TeX-doc is a very slow function
          "hd" 'TeX-doc
          "im" 'TeX-insert-macro
          "v" 'TeX-view

          "xb" 'latex/font-bold
          "xc" 'latex/font-code
          "xe" 'latex/font-emphasis
          "xi" 'latex/font-italic
          "xr" 'latex/font-clear
          "xo" 'latex/font-oblique
          "xfc" 'latex/font-small-caps
          "xff" 'latex/font-sans-serif
          "xfr" 'latex/font-serif)
        (spacemacs/declare-prefix-for-mode 'tex-mode "mx" "tex/text")
        (spacemacs/declare-prefix-for-mode 'tex-mode "mx" "tex/fonts")

        ;; Key bindings for LaTeX
        (spacemacs/set-leader-keys-for-major-mode 'latex-mode
            ; ============
            ; = comments =
            ; ============
          "%" 'TeX-comment-or-uncomment-paragraph
          ";" 'TeX-comment-or-uncomment-region
          ; ======================
          ; = Controlling the output =
          ; ======================
          "dk" 'TeX-kill-job
          "do" 'TeX-recenter-output-buffer
          "gh" 'TeX-home-buffer
          ; ======================
          ; = Starting a command =
          ; ======================
          "a" 'TeX-master-file-ask
          "cc" 'bq-latex/build
          "c:" 'TeX-command-master
          "cb" 'TeX-command-buffer
          "cr" 'TeX-command-region
          "cs:" 'Tex-command-select-master
          "csb" 'TeX-command-select-buffer
          "csr" 'TeX-command-select-region
          "cv" 'TeX-command-run-all
          "s" 'TeX-save-document
          "v" 'TeX-view
          ; =============
          ; = Debugging =
          ; =============
          "dc" 'TeX-clean
          "de" 'TeX-error-overview
          "dn" 'TeX-next-error
          "dN" 'TeX-previous-error
          "dr" 'TeX-normal-mode
          ; =================
          ; = Documentation =
          ; =================
          "ha" 'TeX-goto-info-page
          "hd" 'TeX-doc
          "hp" 'preview-goto-info-page
          ; ====================
          ; = Editing document =
          ; ====================
          "ic" 'TeX-complete-symbol
          "ie" 'LaTeX-environment
          "iE" 'LaTeX-close-environment
          "ii" 'LaTeX-insert-item
          "im" 'TeX-insert-macro
          "t$" 'LaTeX-math-mode
          ; =========
          ; = Marking =
          ; =========
          "ms" 'LaTeX-mark-section
          "me" 'LaTeX-mark-environment
          "mp" 'TeX-pin-region
          ; ============
          ; = Previewing =
          ; ============
          "pb" 'preview-buffer
          "pcb" 'preview-clearout-buffer
          "pcd" 'preview-clearout-document
          "pcp" 'preview-clearout-at-point
          "pcr" 'preview-clearout
          "pcs" 'preview-clearout-section
          "pd" 'preview-document
          "pe" 'preview-environment
          "pf" 'preview-cache-preamble
          "pF" 'preview-cache-preamble-off
          "pp" 'preview-at-point
          "pr" 'preview-region
          "ps" 'preview-section
          ; ===============
          ; = Remap fonts =
          ; ===============
          "xb" 'latex/font-bold
          "xB" 'latex/font-medium
          "xc" 'latex/font-typewriter
          "xe" 'latex/font-emphasis
          "xi" 'latex/font-italic
          "xo" 'latex/font-slanted
          "xr" 'latex/font-delete
          "xfa" 'latex/font-calligraphic
          "xfc" 'latex/font-small-caps
          "xff" 'latex/font-sans-serif
          "xfn" 'latex/font-normal
          "xfr" 'latex/font-serif
          "xfu" 'latex/font-upright)
      (spacemacs/declare-prefix-for-mode 'latex-mode "mx" "latex/text")
      (spacemacs/declare-prefix-for-mode 'latex-mode "mx" "latex/fonts"))))

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

(defun latex/init-magic-latex-buffer ()
    (use-package magic-latex-buffer
       :defer t
       :init
       (progn
           (setq magic-latex-enable-block-highlight t
              magic-latex-enable-suscript t
              magic-latex-enable-pretty-symbols t
              magic-latex-enable-block-align t
              magic-latex-enable-inline-image t)))
  (add-hook 'LaTeX-mode-hook 'magic-latex-buffer))

