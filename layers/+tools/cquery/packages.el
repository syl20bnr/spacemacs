(defconst cquery-packages '((cquery)))

;; See also https://github.com/cquery-project/cquery/wiki/Emacs
(defun cquery/init-cquery ()
  (use-package cquery
    :commands lsp-cquery-enable
    :init
    ;; Customize `lsp-project-whitelist' `lsp-project-blacklist' to disable auto initialization.
    (add-hook 'c-mode-common-hook #'cquery//enable)
    ))
