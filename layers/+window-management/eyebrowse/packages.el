;;; packages.el --- Eyebrowse Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
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
    :diminish eyebrowse-mode
    :init
    (progn
      (setq eyebrowse-new-workspace #'spacemacs/home
            eyebrowse-wrap-around t)
      (eyebrowse-mode)

      ;; vim-style tab switching
      (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
      (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)

      (defun spacemacs/workspace-number ()
        "Return the number of the current workspace."
        (let* ((num (eyebrowse--get 'current-slot))
               (str (if num (int-to-string num))))
          (cond
           ((not (dotspacemacs|symbol-value
                  dotspacemacs-mode-line-unicode-symbols)) str)
           ((equal str "1") "➊")
           ((equal str "2") "➋")
           ((equal str "3") "➌")
           ((equal str "4") "➍")
           ((equal str "5") "➎")
           ((equal str "6") "❻")
           ((equal str "7") "➐")
           ((equal str "8") "➑")
           ((equal str "9") "➒")
           ((equal str "0") "➓"))))

      (defun spacemacs/workspaces-ms-rename ()
        "Rename a workspace and get back to micro-state."
        (interactive)
        (eyebrowse-rename-window-config (eyebrowse--get 'current-slot))
        (spacemacs/workspaces-micro-state))

      (defun spacemacs//workspaces-ms-get-slot-name (window-config)
        "Return the name for the given window-config"
        (let ((slot (car window-config))
              (caption (eyebrowse-format-slot window-config)))
          (if (= slot current-slot)
              (format "[%s]" caption)
            caption)))

      (defun spacemacs//workspaces-ms-documentation ()
        "Return the docstring for the workspaces micro-state."
        (let* ((current-slot (eyebrowse--get 'current-slot))
               (window-configs (eyebrowse--get 'window-configs))
               (window-config-slots (mapcar (lambda (x)
                                              (number-to-string (car x)))
                                            window-configs)))
          (concat
           "<" (if window-configs
                   (concat
                    (mapconcat 'spacemacs//workspaces-ms-get-slot-name
                               window-configs "> <") ">")
                 (when eyebrowse-display-help
                   (concat
                    "\n[0-9] to create/switch to a workspace, "
                    "[n] next, [p/N] previous, [TAB] back and forth, [c] close, "
                    "[r] rename"))))))

      (spacemacs|define-micro-state workspaces
        :doc (spacemacs//workspaces-ms-documentation)
        :use-minibuffer t
        :evil-leader "W"
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
        ("n" eyebrowse-next-window-config)
        ("N" eyebrowse-prev-window-config)
        ("p" eyebrowse-prev-window-config)
        ("r" spacemacs/workspaces-ms-rename :exit t)
        ("s" eyebrowse-switch-to-window-config :exit t)))))
