(defvar auctex-packages
  '(
    auctex
    ))

(when (member 'company-mode dotspacemacs-configuration-layers)
  (add-to-list 'auctex-packages 'company-auctex))

(defun auctex/init-auctex ()
  (interactive)
  (use-package tex
    :config
    (progn
      (when (member 'company-mode dotspacemacs-configuration-layers)
        (use-package company-auctex
          :init (company-auctex-init)))

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
      (setq-default TeX-master nil)
      (setq-default TeX-PDF-mode t))))
