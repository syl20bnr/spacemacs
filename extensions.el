(defvar syl:extensions
  '(
    centered-cursor
    delim-pad
    ;; emacs-eclim
    evil-plugins
    flymake
    mu4e
    pylookup
    ;; pymacs
    ))

;; load extensions
(dolist (ext syl:extensions)
  (add-to-list 'load-path (format "%s%s/" user-extensions-directory ext)))

;; initialize extensions
(setq syl:extension-init-dir (concat user-emacs-directory "init-extension/"))
(message (format "initializing extensions out of %s" syl:extension-init-dir))
(dolist (ext syl:extensions)
    (let* ((initfile (concat syl:extension-init-dir (format "init-%s.el" ext))))
      (if (file-exists-p initfile)
          (progn (load initfile)
                 (message (format "loaded %s" initfile))))))

(provide 'extensions)
