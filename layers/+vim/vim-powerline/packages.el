(setq vim-powerline-packages
      '(
        powerline
        (vim-powerline :location local)
        ))

(defun vim-powerline/init-powerline ())

(defun vim-powerline/init-vim-powerline ()
  (require 'powerline)
  (if (display-graphic-p)
      (setq powerline-default-separator 'arrow)
    (setq powerline-default-separator 'utf-8))

  (defun powerline-raw (str &optional face pad)
  "Render STR as mode-line data using FACE and optionally
PAD import on left (l) or right (r) or left-right (lr)."
  (when str
    (let* ((rendered-str (format-mode-line str))
           (padded-str (concat
                        (when (and (> (length rendered-str) 0)
                                   (or (eq pad 'l) (eq pad 'lr))) " ")
                        (if (listp str) rendered-str str)
                        (when (and (> (length rendered-str) 0)
                                   (or (eq pad 'r) (eq pad 'lr))) " "))))

      (if face
          (pl/add-text-property padded-str 'face face)
        padded-str))))

  (require 'vim-powerline-theme)
  (powerline-vimish-theme)

  (defun spacemacs//set-vimish-powerline-for-startup-buffers ()
    "Set the powerline for buffers created when Emacs starts."
    (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
      (when (get-buffer buffer)
        (with-current-buffer buffer
          (setq-local mode-line-format (default-value 'mode-line-format))
          (powerline-set-selected-window)
          (powerline-reset)))))
  (add-hook 'emacs-startup-hook
            'spacemacs//set-vimish-powerline-for-startup-buffers))
