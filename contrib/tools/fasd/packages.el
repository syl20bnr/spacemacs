(setq fasd-packages
  '(
    fasd
    ))

(defun fasd-find-file-only ()
  (interactive)
  (fasd-find-file -1))

(defun fasd-find-directory-only ()
  (interactive)
  (fasd-find-file 1))

(defun fasd/init-fasd ()
  "initializes fasd-emacs and adds a key binding to <SPC f z>"
  (use-package fasd
    :init
    (progn
      (global-fasd-mode 1)
      (spacemacs/declare-prefix "fa" "fasd-find")
      (evil-leader/set-key "fad" 'fasd-find-directory-only)
      (evil-leader/set-key "faf" 'fasd-find-file-only)
      (evil-leader/set-key "fas" 'fasd-find-file)

      ;; we will fall back to using the default completing-read function, which is helm once helm is loaded.
      (setq fasd-completing-read-function 'nil)
      )
    )
  )
