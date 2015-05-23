(setq perspectives-packages
  '(
    perspective
    persp-projectile
    ))

(defun perspectives/init-perspective ()
  (use-package perspective
    :commands (custom-persp
               persp-add-buffer
               persp-set-buffer
               persp-kill
               persp-remove-buffer
               ;; persp-cycle-next
               ;; persp-cycle-prev
               persp-rename
               persp-switch
               projectile-persp-bridge
               )
    :init
    (progn
      ;; muh perspectives
      (defun custom-persp/emacs ()
        (interactive)
        (custom-persp ".emacs.d"
                      (find-file (locate-user-emacs-file "init.el"))))

      (defun custom-persp/org ()
        (interactive)
        (custom-persp "@org"
                      (find-file (first org-agenda-files))))

      (defun custom-persp/rcirc ()
        (interactive)
        (custom-persp "@RCIRC" (spacemacs/rcirc nil)))

      (spacemacs/declare-prefix "L" "layouts")
      (spacemacs/declare-prefix "Lo" "custom-perspectives")
      (evil-leader/set-key
        "La"  'persp-add-buffer
        "LA"  'persp-set-buffer
        "Lc"  'persp-kill
        "Lk"  'persp-remove-buffer
        "Ln"  'persp-next
        "Loe" 'custom-persp/emacs
        "Loi" 'custom-persp/rcirc
        "Loo" 'custom-persp/org
        "Lp"  'persp-prev
        "Lr"  'persp-rename
        "Ls"  'persp-switch))
    :config
    (progn
      (persp-mode t)
      ;; loading code for our custom perspectives
      ;; taken from Magnar Sveen
      (defmacro custom-persp (name &rest body)
        `(let ((initialize (not (gethash ,name perspectives-hash)))
               (current-perspective persp-curr))
           (persp-switch ,name)
           (when initialize ,@body)
           (setq persp-last current-perspective)))

      (add-hook 'after-init-hook '(lambda ()
                                    (persp-rename "@spacemacs")))
      ;; Jump to last perspective
      ;; taken from Magnar Sveen
      (defun custom-persp-last ()
        (interactive)
        (persp-switch (persp-name persp-last)))

      ;; (defun persp-cycle-next ()
      ;;   "Cycle throught the available perspectives."
      ;;   (interactive)
      ;;   (let ((next-pos (1+ (persp-name persp-curr)))
      ;;         (list-size (length (persp-all-names))))
      ;;     (cond ((eq 1 list-size) (persp-switch nil))
      ;;           ((>= next-pos list-size) (persp-switch (nth 0 (persp-all-names))))
      ;;           (t (persp-next)))))
      ;; (defun persp-cycle-prev ()
      ;;   "Cycle throught the available perspectives."
      ;;   (interactive)
      ;;   (let ((next-pos (- (persp-name persp-curr) 1))
      ;;         (list-size (length (persp-all-names))))
      ;;     (cond ((eq 1 list-size) (persp-switch nil))
      ;;           ((< next-pos 0) (persp-switch (nth (- list-size 1) (persp-all-names))))
      ;;           (t (persp-prev)))))
      )
    ))

(defun perspectives/init-persp-projectile ()
  (use-package persp-projectile
    :if perspective-enable-persp-projectile
    :config
    (progn
      (projectile-persp-bridge helm-projectile-switch-project)

      (evil-leader/set-key
        "pp" 'spacemacs/persp-switch-project)

      (defun spacemacs/persp-switch-project ()
        (interactive)
        (evil-leader/set-key
          "pp" 'helm-projectile-switch-project)
        (find-file "~/.spacemacs")
        (helm-projectile-switch-project)
        (persp-add-buffer "*spacemacs*")
        (persp-kill "@spacemacs")))))
