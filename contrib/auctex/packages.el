(setq auctex-packages
  '(
    auctex
    auctex-latexmk
    company
    company-auctex
    evil-matchit
    flycheck
    ))

(defun auctex/init-auctex ()
  (use-package tex
    :defer t
    :config
    (progn
      (add-hook 'TeX-mode-hook 'flyspell-mode)
      (add-hook 'TeX-mode-hook 'company-mode)
      (add-hook 'TeX-mode-hook 'spacemacs/load-yasnippet)
      (when auctex-auto-fill (add-hook 'TeX-mode-hook 'auctex/auto-fill-mode))
      (add-hook 'TeX-mode-hook 'smartparens-mode)
      (add-hook 'LaTeX-mode-hook 'latex-math-mode)

      ;; (setq spacemacs/key-binding-prefixes '(("mp" . "LaTeX Preview")))

      ;; Key bindings for plain TeX
      (evil-leader/set-key-for-mode 'tex-mode
        "mC" 'TeX-command-master
        "mb" 'auctex/build
        "mv" 'TeX-view
        "m\\" 'TeX-insert-macro
        "mf" 'TeX-font) ;; Find a way to rebind tex-fonts

      ;; Key bindings for LaTeX
      (evil-leader/set-key-for-mode 'latex-mode
        "mC" 'TeX-command-master
        "mb" 'auctex/build
        "mv" 'TeX-view
        "m\\" 'TeX-insert-macro
        "mf" 'TeX-font ;; Find a way to rebind tex-fonts
        "me" 'LaTeX-environment
        "mc" 'LaTeX-close-environment
        "mi" 'LaTeX-insert-item)

      ;; Preview key bindings
      (evil-leader/set-key-for-mode 'latex-mode
        "mpr" 'preview-region
        "mpb" 'preview-buffer
        "mpd" 'preview-document
        "mpe" 'preview-environment
        "mps" 'preview-section
        "mpp" 'preview-at-point
        "mpf" 'preview-cache-preamble
        "mpc" 'preview-clearout

        "mhd" 'TeX-doc ;; TeX-doc is a very slow function
        )

      (setq-default TeX-command-default auctex-build-command)

      (setq-default TeX-auto-save t)
      (setq-default TeX-parse-self t)
      (setq-default TeX-syntactic-comment t)
      (setq-default TeX-PDF-mode t)

      (when auctex-electric-sub-and-superscript
        (setq-default TeX-electric-sub-and-superscript t))
      (when auctex-electric-escape
        (setq-default TeX-electric-escape t))
      (setq-default TeX-electric-macro)

      ;; Synctex support
      (setq-default TeX-source-correlate-mode t)
      (setq-default TeX-source-correlate-start-server nil)

      ;; Setup reftex style (RefTeX is supported through extension)
      (setq-default reftex-use-fonts t)

      ;; Don't insert line-break at inline math
      (setq-default LaTeX-fill-break-at-separators '()))))

(when (string= auctex-build-command "LatexMk")
  (defun auctex/init-auctex-latexmk ()
    (use-package auctex-latexmk
      :defer t
      :init
      (progn
        (auctex-latexmk-setup)))))

(defun auctex/post-init-evil-matchit ()
  (add-hook 'LaTeX-mode-hook 'evil-matchit-mode))

(defun python/post-init-flycheck ()
  (add-hook 'LaTeX-mode-hook 'flycheck-mode))

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
