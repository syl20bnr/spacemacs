;;; packages.el --- Eyebrowse Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq eyebrowse-packages '(eyebrowse))

(defun eyebrowse/init-eyebrowse ()
  (use-package eyebrowse
    :init
    (progn
      (setq eyebrowse-new-workspace #'spacemacs/home-delete-other-windows
            eyebrowse-wrap-around t)
      (eyebrowse-mode)

      ;; vim-style tab switching
      (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
      (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)

      (spacemacs/set-leader-keys "bW" 'spacemacs/goto-buffer-workspace)

      (defun spacemacs/find-workspace (buffer)
        "Find Eyebrowse workspace containing BUFFER.
If several workspaces contain BUFFER, return the first one. Workspaces are
ordered by slot number.
If no workspace contains
BUFFER, return nil."
        ;; the second element of a workspace is its window-state object
        (--find (memq buffer (spacemacs/window-state-get-buffers (cadr it)))
                (eyebrowse--get 'window-configs)))

      (defun spacemacs/display-in-workspace (buffer alist)
        "Display BUFFER's workspace.
Return BUFFER's window, if exists, otherwise nil.
If BUFFER is already visible in current workspace, just return its window
without switching workspaces."
        (or (get-buffer-window buffer)
            (-when-let (workspace (spacemacs/find-workspace buffer))
              (eyebrowse-switch-to-window-config (car workspace))
              (get-buffer-window buffer))))

      (defun spacemacs/goto-buffer-workspace (buffer)
        "Switch to BUFFER's window in BUFFER's workspace.
If BUFFER isn't displayed in any workspace, display it in the current
workspace, preferably in the current window."
        (interactive "B")
        (pop-to-buffer buffer '((;; reuse buffer window from some workspace
                                 spacemacs/display-in-workspace
                                 ;; fallback to display in current window
                                 display-buffer-same-window)
                                (inhibit-same-window . nil))))

      (defun spacemacs/workspaces-ms-rename ()
        "Rename a workspace and get back to transient-state."
        (interactive)
        (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
        (spacemacs/workspaces-transient-state/body))

      (defun spacemacs//workspaces-ms-get-slot-name (window-config)
        "Return the name for the given window-config"
        (let ((slot (car window-config))
              (caption (eyebrowse-format-slot window-config)))
          (if (= slot current-slot)
              (format "[%s]" caption)
            caption)))

      (defun spacemacs//workspaces-ms-get-window-configs ()
        "Return the list of window configs."
        (--sort (if (eq (car other) 0)
                    t
                  (< (car it) (car other)))
                (eyebrowse--get 'window-configs)))

      (spacemacs|define-transient-state workspaces
        :title "Workspaces Transient State"
        :additional-docs
        (spacemacs--workspaces-ms-documentation .
         "\n\n[_0_.._9_] switch to workspace  [_n_/_p_] next/prev  [_<tab>_] last  [_c_] close  [_r_] rename")
        :bindings
        ("0" eyebrowse-switch-to-window-config-0)
        ("1" eyebrowse-switch-to-window-config-1)
        ("2" eyebrowse-switch-to-window-config-2)
        ("3" eyebrowse-switch-to-window-config-3)
        ("4" eyebrowse-switch-to-window-config-4)
        ("5" eyebrowse-switch-to-window-config-5)
        ("6" eyebrowse-switch-to-window-config-6)
        ("7" eyebrowse-switch-to-window-config-7)
        ("8" eyebrowse-switch-to-window-config-8)
        ("9" eyebrowse-switch-to-window-config-9)
        ("<tab>" eyebrowse-last-window-config)
        ("C-i" eyebrowse-last-window-config)
        ("c" eyebrowse-close-window-config)
        ("h" eyebrowse-prev-window-config)
        ("l" eyebrowse-next-window-config)
        ("n" eyebrowse-next-window-config)
        ("N" eyebrowse-prev-window-config)
        ("p" eyebrowse-prev-window-config)
        ("r" spacemacs/workspaces-ms-rename :exit t)
        ("w" eyebrowse-switch-to-window-config :exit t))


      (defun spacemacs//workspace-format-name (workspace)
        (let ((current (eq (eyebrowse--get 'current-slot) (car workspace)))
              (name (nth 2 workspace))
              (number (car workspace)))
          (concat
           (if current "[" "")
           (if (< 0 (length name)) name (int-to-string number))
           (if current "]" ""))))

      (defun spacemacs//workspaces-ms-list ()
        "Return the list of workspaces for the workspacae
transient state."
        (mapconcat 'spacemacs//workspace-format-name (eyebrowse--get 'window-configs) " | "))

      (add-hook 'spacemacs-post-user-config-hook
                (lambda ()
                  (setq spacemacs/workspaces-transient-state/hint
                        `(concat
                          ,(when dotspacemacs-show-transient-state-title
                             (concat
                              (propertize "Workspaces Transient State"
                                          'face 'spacemacs-transient-state-title-face)
                              "\n"))
                          (spacemacs//workspaces-ms-list)
                          spacemacs--workspaces-ms-documentation)))
                t)

      ;; The layouts layer defines this keybinding inside a transient-state
      ;; thus this is only needed if that layer is not used
      (unless (configuration-layer/layer-usedp 'spacemacs-layouts)
        (spacemacs/set-leader-keys "lw" 'spacemacs/workspaces-transient-state/body)))))
