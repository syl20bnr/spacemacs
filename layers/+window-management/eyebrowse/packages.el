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

      (defun spacemacs/workspaces-ts-rename ()
        "Rename a workspace and get back to transient-state."
        (interactive)
        (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
        (spacemacs/workspaces-transient-state/body))

      (spacemacs|transient-state-format-hint workspaces
        spacemacs--workspaces-ts-full-hint
        "\n\n
 Go to^^^^^^                         Remove/Rename...^^
--^-^--^^^^-----------------------  --^-^---------------------------
 [_0_,_9_]^^     nth/new workspace   [_d_] close current workspace
 [_C-0_,_C-9_]^^ nth/new workspace   [_R_] rename current workspace
 [_n_/_C-l_]^^   next workspace
 [_N_/_p_/_C-h_] prev workspace
 [_<tab>_]^^^^   last workspace\n")

      (spacemacs|define-transient-state workspaces
        :title "Workspaces Transient State"
        :hint-is-doc t
        :dynamic-hint (spacemacs//workspaces-ts-hint)
        :bindings
        ("0" eyebrowse-switch-to-window-config-0 :exit t)
        ("1" eyebrowse-switch-to-window-config-1 :exit t)
        ("2" eyebrowse-switch-to-window-config-2 :exit t)
        ("3" eyebrowse-switch-to-window-config-3 :exit t)
        ("4" eyebrowse-switch-to-window-config-4 :exit t)
        ("5" eyebrowse-switch-to-window-config-5 :exit t)
        ("6" eyebrowse-switch-to-window-config-6 :exit t)
        ("7" eyebrowse-switch-to-window-config-7 :exit t)
        ("8" eyebrowse-switch-to-window-config-8 :exit t)
        ("9" eyebrowse-switch-to-window-config-9 :exit t)
        ("C-0" eyebrowse-switch-to-window-config-0)
        ("C-1" eyebrowse-switch-to-window-config-1)
        ("C-2" eyebrowse-switch-to-window-config-2)
        ("C-3" eyebrowse-switch-to-window-config-3)
        ("C-4" eyebrowse-switch-to-window-config-4)
        ("C-5" eyebrowse-switch-to-window-config-5)
        ("C-6" eyebrowse-switch-to-window-config-6)
        ("C-7" eyebrowse-switch-to-window-config-7)
        ("C-8" eyebrowse-switch-to-window-config-8)
        ("C-9" eyebrowse-switch-to-window-config-9)
        ("<tab>" eyebrowse-last-window-config)
        ("C-h" eyebrowse-prev-window-config)
        ("C-i" eyebrowse-last-window-config)
        ("C-l" eyebrowse-next-window-config)
        ("d" eyebrowse-close-window-config)
        ("h" eyebrowse-prev-window-config)
        ("l" eyebrowse-next-window-config)
        ("n" eyebrowse-next-window-config)
        ("N" eyebrowse-prev-window-config)
        ("p" eyebrowse-prev-window-config)
        ("R" spacemacs/workspaces-ts-rename :exit t)
        ("w" eyebrowse-switch-to-window-config :exit t))

      (defun spacemacs//workspace-format-name (workspace)
        "Return a porpertized string given a WORKSPACE name."
        (let* ((current (eq (eyebrowse--get 'current-slot) (car workspace)))
               (name (nth 2 workspace))
               (number (car workspace))
               (caption (if (< 0 (length name))
                            (concat (int-to-string number) ":" name)
                          (int-to-string number))))
          (if current
              (propertize (concat "[" caption "]") 'face 'warning)
            caption)))

      (defun spacemacs//workspaces-ts-hint ()
        "Return a one liner string containing all the workspace names."
        (concat
         " "
         (mapconcat 'spacemacs//workspace-format-name
                    (eyebrowse--get 'window-configs) " | ")
         (when eyebrowse-display-help spacemacs--workspaces-ts-full-hint)))

      ;; The layouts layer defines this keybinding inside a transient-state
      ;; thus this is only needed if that layer is not used
      (unless (configuration-layer/layer-usedp 'spacemacs-layouts)
        (spacemacs/set-leader-keys
          "lw" 'spacemacs/workspaces-transient-state/body)))))
