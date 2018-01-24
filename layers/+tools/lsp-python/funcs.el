;;; packages.el --- lsp-python layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; (defun ensure-lsp-python-server ()
;;   (unless (file-directory-p lsp-python-server-install-directory)
;;     (install-lsp-python-server)
;;     ))

;; (defun install-lsp-python-server ()
;;   (shell-command-to-string (concat "pip3 install python-language-server --install-option=\"--prefix=\" --target " lsp-python-server-install-directory))
;;   (when python-enable-mypy
;;     (shell-command-to-string (concat "pip3 install pyls-mypy --install-option=\"--prefix=\" --target " lsp-python-server-install-directory)))
;;   (when python-enable-import-sort
;;     (shell-command-to-string (concat "pip3 install pyls-isort --install-option=\"--prefix=\" --target " lsp-python-server-install-directory)))
;;   )


(defun spacemacs//add-python-format-on-save ()
  (add-hook 'before-save-hook 'lsp-format-buffer)
  )

(defun spacemacs/run-current-file-in-python ()
  (interactive)
  (unless (member "#<buffer *Python*>" (buffer-list))
    (run-python))
  (python-shell-send-file buffer-file-name)
  (display-buffer "*Python*")
  )

(defun spacemacs/kill-python-interpreter ()
  (interactive)
  (when (member "#<buffer *Python*>" (buffer-list))
    (kill-buffer "*Python*"))
  )

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
;; from spacemacs python layer
(defun spacemacs/python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))
