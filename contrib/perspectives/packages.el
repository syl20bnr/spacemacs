(defvar perspectives-packages
  '(
    perspective
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar perspectives-excluded-packages '()
  "List of packages to exclude.")

(defun perspectives/init-perspective ()
  (use-package perspective
    :commands persp-mode
    :init
    (progn
      (persp-mode t)
      (evil-leader/set-key
        "Ps" 'persp-switch
        "Pk" 'persp-remove-buffer
        "Pc" 'persp-kill
        "Pr" 'persp-rename
        "Pa" 'persp-add-buffer
        "PA" 'persp-set-buffer)) 
    :config
    (progn
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
      
      ;; muh perspectives
      (defun custom-persp/emacs ()
        (interactive)
        (custom-persp ".emacs.d"
                      (find-file "~/.emacs.d/init.el")))
      (defun custom-persp/org ()
        (interactive)
        (custom-persp "@org"
                      (find-file (first org-agenda-files))))

      (eval-after-load "evil-leader"
        '(evil-leader/set-key
           "Pn" 'persp-cycle-next
           "Pp" 'persp-cycle-prev
           "Poe" 'custom-persp/emacs
           "Poo" 'custom-persp/org))
      )
    )
  )
