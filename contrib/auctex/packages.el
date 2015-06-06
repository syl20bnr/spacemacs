(setq auctex-packages
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

(defun auctex/init-auctex ()
  (use-package tex
    :defer t
    :init
    (progn
      (setq TeX-command-default auctex-build-command
            TeX-auto-save t
            TeX-parse-self t
            TeX-syntactic-comment t
            TeX-PDF-mode t
            TeX-electric-sub-and-superscript t
            TeX-electric-escape t
            ;; Synctex support
            TeX-source-correlate-mode t
            TeX-source-correlate-start-server nil
            ;; Setup reftex style (RefTeX is supported through extension)
            reftex-use-fonts t
            ;; Don't insert line-break at inline math
            LaTeX-fill-break-at-separators nil)
      (when auctex-enable-auto-fill
        (add-hook 'LaTeX-mode-hook 'auctex/auto-fill-mode))
      (add-hook 'LaTeX-mode-hook 'latex-math-mode))
    :config
    (progn
      ;; Key bindings for plain TeX
      (evil-leader/set-key-for-mode 'tex-mode
        "m\\" 'TeX-insert-macro
        "mb" 'auctex/build
        "mC" 'TeX-command-master
        ;; Find a way to rebind tex-fonts
        "mf" 'TeX-font
        "mv" 'TeX-view)

      ;; Key bindings for LaTeX
      (evil-leader/set-key-for-mode 'latex-mode
        "m\\" 'TeX-insert-macro
        "mb" 'auctex/build
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

(when (string= auctex-build-command "LatexMk")
(defun auctex/init-auctex-latexmk ()
  (use-package auctex-latexmk
    :defer t
    :init (add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun auctex/post-init-company ()
    (spacemacs|add-company-hook LaTeX-mode))

  (defun auctex/init-company-auctex ()
    (use-package company-auctex
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (progn
        (push 'company-auctex-labels company-backends-LaTeX-mode)
        (push 'company-auctex-bibs company-backends-LaTeX-mode)
        (push '(company-auctex-macros company-auctex-symbols company-auctex-environments)
              company-backends-LaTeX-mode)))))

(defun auctex/post-init-evil-matchit ()
  (add-hook 'LaTeX-mode-hook 'evil-matchit-mode))

(defun python/post-init-flycheck ()
  (add-hook 'LaTeX-mode-hook 'flycheck-mode))

(defun auctex/post-init-flyspell ()
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(defun auctex/post-init-smartparens ()
  (add-hook 'LaTeX-mode-hook 'smartparens-mode))

(defun auctex/post-init-yasnippet ()
  (add-hook 'LaTeX-mode-hook 'spacemacs/load-yasnippet))
