;;; init.el --- Spacemacs as an elisp package

(defconst spacemacs-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "My emacs config directory.")

;; Make a fake entry point for spacemacs, also modify the
;; `user-emacs-directory' temporary to mislead spacemacs real emacs
;; directory.
(require 'f)
(let* ((spacemacs-init
        (concat (file-name-as-directory spacemacs-dir) "init.el"))
       (user-emacs-directory (file-name-directory spacemacs-init)))
  ;; Initial spacemacs, our emacs run on top of it
  (load spacemacs-init))

;; Make spacemacs not remove my packages.
(defadvice configuration-layer/delete-orphan-packages (around null-func activate)
  "Overwrite the spacemacs's `configuration-layer/delete-orphan-packages'
  to make it not remove any orphan packages.")


(defadvice spacemacs/check-for-new-version (around null-func activate)
  "Overwrite the spacemacs's `spacemacs/check-for-new-version' to
  Update spacemacs using package tools, like quelpa")

(provide 'spacemacs)
