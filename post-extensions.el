(defvar syl:post-extensions
  '(
    centered-cursor
    dos
    emoji-cheat-sheet
    evil-org-mode
    evil-plugins
    nose
    o-blog
    powerline
    pylookup
    solarized-theme
    ))

;; load extensions
(dolist (ext syl:post-extensions)
  (add-to-list 'load-path (format "%s%s/" user-extensions-directory ext)))

;; initialize extensions
(setq syl:extension-init-dir (concat user-emacs-directory "init-extension/"))
(dolist (ext syl:post-extensions)
    (let* ((initfile (concat syl:extension-init-dir (format "init-%s.el" ext))))
      (if (file-exists-p initfile)
          (load initfile))))

(provide 'post-extensions)
