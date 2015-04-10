(defvar editorconfig-packages '(editorconfig))

(defun editorconfig/init-editorconfig ()
  (use-package editorconfig
    :defer t
    :init (add-to-list 'auto-mode-alist '("\\.editorconfig" . conf-unix-mode))))
