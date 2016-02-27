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

(provide 'spacemacs)
