(defvar syl:pre-extensions
  '(
    use-package
    revive
    window-numbering
    ))

;; load extensions
(dolist (ext syl:pre-extensions)
  (add-to-list 'load-path (format "%s%s/" user-extensions-directory ext)))

;; initialize extensions
(setq syl:extension-init-dir (concat user-emacs-directory "init-extension/"))
(dolist (ext syl:pre-extensions)
    (let* ((initfile (concat syl:extension-init-dir (format "init-%s.el" ext))))
      (if (file-exists-p initfile)
          (load initfile))))

(provide 'pre-extensions)
