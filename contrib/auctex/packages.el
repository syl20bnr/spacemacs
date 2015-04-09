(defvar auctex-packages
  '(
    auctex
    company
    company-auctex
    evil-matchit
    ))

(defun auctex/init-auctex ()
  (use-package tex
    :defer t
    :config
    (progn
      (defun auctex/build-view ()
        (interactive)
        (if (buffer-modified-p)
            (progn
              (let ((TeX-save-query nil))
                (TeX-save-document (TeX-master-file)))
              (setq build-proc (TeX-command "LaTeX" 'TeX-master-file -1))
              (set-process-sentinel  build-proc  'auctex/build-sentinel))
          (TeX-view)))

      (defun auctex/build-sentinel (process event)
        (if (string= event "finished\n")
            (TeX-view)
          (message "Errors! Check with C-`")))

      (add-hook 'LaTeX-mode-hook '(lambda () (local-set-key (kbd "H-r") 'auctex/build-view)))
      (add-hook 'LaTeX-mode-hook 'flyspell-mode)
      (add-hook 'LaTeX-mode-hook 'company-mode)
      (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
      (add-hook 'LaTeX-mode-hook 'spacemacs/load-yasnippet)

      (setq spacemacs/key-binding-prefixes '(("mp" . "LaTeX Preview")))
      (evil-leader/set-key-for-mode 'latex-mode
        "mb" 'auctex/build-view
        "me" 'LaTeX-environment
        "mc" 'LaTeX-close-environment
        "mi" 'LaTeX-insert-item
        "mf" 'TeX-font ;; Find a way to rebind tex-fonts

        "mC" 'TeX-command-master

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

      (setq-default TeX-auto-save t)
      (setq-default TeX-parse-self t)
      (setq-default TeX-PDF-mode t))))


(defun auctex/post-init-evil-matchit ()
  (add-hook 'LaTeX-mode-hook 'evil-matchit-mode))

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
