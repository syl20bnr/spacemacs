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
               persp-cycle-next
               persp-cycle-prev
               persp-rename
               persp-switch)
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

      (define-prefix-command 'perspectives-prefix)
      (evil-leader/set-key "P" 'perspectives-prefix)
      (define-prefix-command 'perspectives-custom-prefix)
      (evil-leader/set-key "Po" 'perspectives-custom-prefix)
      (evil-leader/set-key
        "Pa"  'persp-add-buffer
        "PA"  'persp-set-buffer
        "Pc"  'persp-kill
        "Pk"  'persp-remove-buffer
        "Pn"  'persp-cycle-next
        "Poe" 'custom-persp/emacs
        "Poo" 'custom-persp/org
        "Pp"  'persp-cycle-prev
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
      ;; Jump to last perspective
      ;; taken from Magnar Sveen
      (defun custom-persp-last ()
        (interactive)
        (persp-switch (persp-name persp-last)))
      (defun persp-cycle-next ()
        "Cycle throught the available perspectives."
        (interactive)
        (let ((next-pos (1+ (persp-curr-position)))
              (list-size (length (persp-all-names))))
          (cond ((eq 1 list-size) (persp-switch nil))
                ((>= next-pos list-size) (persp-switch (nth 0 (persp-all-names))))
                (t (persp-next)))))
      (defun persp-cycle-prev ()
        "Cycle throught the available perspectives."
        (interactive)
        (let ((next-pos (- (persp-curr-position) 1))
              (list-size (length (persp-all-names))))
          (cond ((eq 1 list-size) (persp-switch nil))
                ((< next-pos 0) (persp-switch (nth (- list-size 1) (persp-all-names))))
                (t (persp-prev)))))

      (eval-after-load 'helm-projectile
        '(projectile-persp-bridge helm-projectile))
      )
    ))
