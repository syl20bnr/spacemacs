(setq editorconfig-packages '(editorconfig))

(defun editorconfig/init-editorconfig ()
  (use-package editorconfig
    :defer t
    :init (add-to-list 'auto-mode-alist '("\\.editorconfig" . conf-unix-mode))
    :config (add-to-list 'edconf-custom-hooks 'editorconfig//adjust-evil-shift)))

(defun editorconfig//adjust-evil-shift (props)
  (let ((ec-indent-style (gethash 'indent_style props))
        (ec-indent-size (gethash 'indent_size props))
        (ec-tab-width (gethash 'tab_width props)))
    (when (and ec-tab-width (or (equal ec-indent-style "tab")
                                (equal ec-indent-size "tab")))
      (setq ec-indent-size ec-tab-width))
    (when (edconf-string-integer-p ec-indent-size)
      (setq ec-indent-size (string-to-number ec-indent-size)))
    (when ec-indent-size
      (setq-local evil-shift-width ec-indent-size))))
