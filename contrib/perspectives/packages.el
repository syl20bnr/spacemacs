(defvar perspectives-packages
  '(
    perspective
    persp-projectile
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar perspectives-excluded-packages '()
  "List of packages to exclude.")

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
                      (find-file "~/.emacs.d/init.el")))

      (defun custom-persp/org ()
        (interactive)
        (custom-persp "@org"
                      (find-file (first org-agenda-files))))

      (defun custom-persp/rcirc ()
        (interactive)
        (custom-persp "@RCIRC" (rcirc-config)))
      (evil-leader/set-key
        "Poi" 'custom-persp/rcirc)

      (spacemacs/declare-prefix "P" "perspectives")
      (spacemacs/declare-prefix "Po" "custom-perspectives")
      (evil-leader/set-key
        "Pa"  'persp-add-buffer
        "PA"  'persp-set-buffer
        "Pc"  'persp-kill
        "Pk"  'persp-remove-buffer
        "Pn"  'persp-next
        "Poe" 'custom-persp/emacs
        "Poo" 'custom-persp/org
        "Pp"  'persp-prev
        "Pr"  'persp-rename
        "Ps"  'persp-switch))
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
    :config
    (when perspective-use-persp-projectile
      (projectile-persp-bridge helm-projectile-switch-project)

      (evil-leader/set-key
        "ps" 'spacemacs/persp-switch-project)

      (defun spacemacs/persp-switch-project ()
        (interactive)
        (evil-leader/set-key
          "ps" 'helm-projectile-persp-switch-project)
        (find-file "~/.spacemacs")
        (helm-projectile-switch-project)
        (persp-add-buffer "*spacemacs*")
        (persp-kill "@spacemacs")))
    ))
