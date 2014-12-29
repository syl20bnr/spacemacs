(defvar scala-packages
  '(
    ensime
    sbt-mode
    scala-mode2
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun spacemacs/init-ensime ()
  (use-package ensime
    :commands (ensime-mode)
    :init
    (add-hook 'scala-mode-hook 'ensime-mode)
    :config
    (progn
      (evil-define-key 'normal scala-mode-map "J" 'spacemacs/scala-join-line)

      (evil-define-key 'normal ensime-popup-buffer-map
        (kbd "q") 'ensime-popup-buffer-quit-function)

      (evil-define-key 'normal ensime-refactor-info-map
        (kbd "q") 'spacemacs/ensime-refactor-cancel
        (kbd "c") 'spacemacs/ensime-refactor-accept
        (kbd "RET") 'spacemacs/ensime-refactor-accept)

      (evil-define-key 'normal ensime-compile-result-map
        (kbd "g") 'ensime-show-all-errors-and-warnings
        (kbd "TAB") 'forward-button
        (kbd "<backtab>") 'backward-button
        (kbd "M-n") 'forward-button
        (kbd "M-p") 'backward-button))))
