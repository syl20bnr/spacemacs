;; Post extensions are loaded *after* the packages
(defvar windows-scripts-post-extensions '(dos))

;; Initialize the extensions

(defun windows-scripts/init-dos ()
  (use-package dos
    :commands dos-mode
    :mode ("\\.bat$" . dos-mode)
    :init
    (progn
      (defun windows-scripts/dos-outline-hook ()
        (local-set-key (kbd "SPC m z") 'dos-mode)
        (defun outline-mouse-select ()
          "Select position and return to `dos-mode'."
          (interactive)
          (dos-mode)
          (beginning-of-line)))
      (add-hook 'outline-mode-hook 'windows-scripts/dos-outline-hook))
    :config
    (evil-leader/set-key-for-mode 'dos-mode
      "mD"  'dos-help-cmd  
      "meb" 'dos-run
      "meB" 'dos-run-args
      "ms"  'dos-sep
      "mt"  'dos-template-mini
      "mT"  'dos-template
      "mz"  'dos-outline)))
