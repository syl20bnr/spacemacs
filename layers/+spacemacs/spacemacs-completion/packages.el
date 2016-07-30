;;; packages.el --- Spacemacs Completion Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-completion-packages
      '(
        (default-helm-config :location built-in)
        (default-ivy-config :location built-in)
        (ido :location built-in)
        ido-vertical-mode
        ))

(defun spacemacs-completion/init-default-helm-config ()
  (setq helm-prevent-escaping-from-minibuffer t
        helm-bookmark-show-location t
        helm-display-header-line nil
        helm-split-window-in-side-p t
        helm-always-two-windows t
        helm-echo-input-in-header-line t
        helm-imenu-execute-action-at-once-if-one nil
        helm-org-format-outline-path t
        helm-display-function 'spacemacs//display-helm-window)
  (with-eval-after-load 'helm
    (spacemacs|hide-lighter helm-mode)
    (when (and dotspacemacs-helm-resize
               (or (eq dotspacemacs-helm-position 'bottom)
                   (eq dotspacemacs-helm-position 'top)))
      (setq helm-autoresize-min-height 10)
      (helm-autoresize-mode 1))
    ;; setup hooks
    (add-hook 'helm-minibuffer-set-up-hook
              'spacemacs//helm-hide-minibuffer-maybe)
    (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
    (spacemacs/add-to-hook 'helm-after-initialize-hook
                           '(spacemacs//prevent-minibuffer-escape
                             spacemacs//hide-cursor-in-helm-buffer))
    (add-hook 'helm-cleanup-hook #'spacemacs//unprevent-minibuffer-escape)
    (add-hook 'helm-find-files-before-init-hook
              'spacemacs//set-dotted-directory)
    (add-hook 'spacemacs-editing-style-hook 'spacemacs//helm-hjkl-navigation)
    ;; setup advices
    ;; fuzzy matching for all the sourcess
    (unless (eq dotspacemacs-helm-use-fuzzy 'source)
      (advice-add 'helm-make-source :around #'spacemacs//helm-make-source))

    (defadvice spacemacs/post-theme-init
        (after spacemacs/helm-header-line-adv activate)
      "Update defaults for `helm' header line whenever a new theme is loaded"
      ;; TODO factorize face definition with those defined in config.el
      (setq helm-source-header-default-foreground
            (face-attribute 'helm-source-header :foreground)
            helm-source-header-default-background
            (face-attribute 'helm-source-header :background)
            helm-source-header-default-box
            (face-attribute 'helm-source-header :box)
            helm-source-header-default-height
            (face-attribute 'helm-source-header :height)))
    ;; ensure that the correct bindings are set at startup
    (spacemacs//helm-hjkl-navigation dotspacemacs-editing-style)
    ;; Transient state
    (spacemacs//define-helm-action-functions)
    (spacemacs|define-transient-state helm-navigation
      :title "Helm Transient State"
      :doc "
 [_j_/_k_]  next/prev candidate  [_v_]^^     persistent action     [_e_]^^    edit occurrences
 [_h_/_l_]  prev/next source     [_1_.._0_]  action 1..10          [_t_/_T_]  toggle visible/all mark
 [_q_]^^    quit                 [_a_]^^     action selection pg"
        :foreign-keys run
        :on-enter (spacemacs//helm-navigation-ts-on-enter)
        :on-exit  (spacemacs//helm-navigation-ts-on-exit)
        :bindings
        ("1" spacemacs/helm-action-1 :exit t)
        ("2" spacemacs/helm-action-2 :exit t)
        ("3" spacemacs/helm-action-3 :exit t)
        ("4" spacemacs/helm-action-4 :exit t)
        ("5" spacemacs/helm-action-5 :exit t)
        ("6" spacemacs/helm-action-6 :exit t)
        ("7" spacemacs/helm-action-7 :exit t)
        ("8" spacemacs/helm-action-8 :exit t)
        ("9" spacemacs/helm-action-9 :exit t)
        ("0" spacemacs/helm-action-10 :exit t)
        ("<tab>" helm-select-action :exit t)
        ("TAB" helm-select-action :exit t)
        ("<RET>" helm-maybe-exit-minibuffer :exit t)
        ;; ("?" nil :doc (spacemacs//helm-navigation-ts-full-doc))
        ("a" spacemacs/helm-transient-state-select-action)
        ("e" spacemacs/helm-ts-edit)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("h" helm-previous-source)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-next-source)
        ("q" nil :exit t)
        ("t" helm-toggle-visible-mark)
        ("T" helm-toggle-all-marks)
        ("v" helm-execute-persistent-action))
      (define-key helm-map (kbd "M-SPC")
        'spacemacs/helm-navigation-transient-state/body)
      (define-key helm-map (kbd "s-M-SPC")
        'spacemacs/helm-navigation-transient-state/body)
      ;; Swap default TAB and C-z commands.
      ;; For GUI.
      (with-eval-after-load 'helm-files
        (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
        (define-key helm-find-files-map
          (kbd "S-<tab>") 'helm-find-files-up-one-level)
        (define-key helm-find-files-map
          (kbd "<backtab>") 'helm-find-files-up-one-level)
        ;; For terminal.
        (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
        (define-key helm-find-files-map
          (kbd "S-TAB") 'helm-find-files-up-one-level)
        (define-key helm-map (kbd "C-z") 'helm-select-action))))

(defun spacemacs-completion/init-default-ivy-config ()
  (with-eval-after-load 'ivy
    (setq ivy-height 15
          ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
    (spacemacs|hide-lighter ivy-mode)
    ;; setup hooks
    (add-hook 'spacemacs-editing-style-hook 'spacemacs//ivy-hjkl-navigation)
    ;; key bindings
    ;; ensure that the correct bindings are set at startup
    (spacemacs//ivy-hjkl-navigation dotspacemacs-editing-style)
    ;; Transient state
    ;; ivy-hydra disabled for now, waiting to see how the dependency management
    ;; evolves upstream
    ;; (require 'ivy-hydra)
    (spacemacs|define-transient-state ivy
      :doc "
 Move/Resize^^^^      | Select Action^^^^   |  Call^^          |  Cancel^^    | Toggles
--^-^-^-^-------------|--^-^-^-^------------|--^---^-----------|--^-^---------|---------------------
 [_j_/_k_] by line    | [_s_/_w_] next/prev | [_RET_] & done   | [_i_] & ins  | [_C_] calling: %s(if ivy-calling \"on\" \"off\")
 [_g_/_G_] first/last | [_a_]^ ^  list all  | [_TAB_] alt done | [_q_] & quit | [_m_] matcher: %s(ivy--matcher-desc)
 [_d_/_u_] pg down/up |  ^ ^ ^ ^            | [_c_]   & cont   |  ^ ^         | [_f_] case-fold: %`ivy-case-fold-search
 [_<_/_>_] resize     |  ^ ^ ^ ^            | [_o_]   occur    |  ^ ^         | [_t_] truncate: %`truncate-lines
 [_h_/_l_] out/in dir |  ^ ^ ^ ^            |  ^ ^             |  ^ ^         |  ^ ^

Current Action: %s(ivy-action-name)
"
      :foreign-keys run
      :bindings
      ;; arrows
      ("j" ivy-next-line)
      ("k" ivy-previous-line)
      ("l" ivy-alt-done)
      ("h" spacemacs/counsel-up-directory-no-error)
      ("g" ivy-beginning-of-buffer)
      ("G" ivy-end-of-buffer)
      ("d" ivy-scroll-up-command)
      ("u" ivy-scroll-down-command)
      ;; actions
      ("q" keyboard-escape-quit :exit t)
      ("C-g" keyboard-escape-quit :exit t)
      ("<escape>" keyboard-escape-quit :exit t)
      ("i" nil)
      ("C-o" nil)
      ("TAB" ivy-alt-done :exit nil)
      ;; ("C-j" ivy-alt-done :exit nil)
      ;; ("d" ivy-done :exit t)
      ("RET" ivy-done :exit t)
      ("c" ivy-call)
      ("C-m" ivy-done :exit t)
      ("C" ivy-toggle-calling)
      ("m" ivy-toggle-fuzzy)
      (">" ivy-minibuffer-grow)
      ("<" ivy-minibuffer-shrink)
      ("w" ivy-prev-action)
      ("s" ivy-next-action)
      ("a" ivy-read-action)
      ("t" (setq truncate-lines (not truncate-lines)))
      ("f" ivy-toggle-case-fold)
      ("o" ivy-occur :exit t))
    (define-key ivy-minibuffer-map "\C-o" 'spacemacs/ivy-transient-state/body)
    ))

(defun spacemacs-completion/init-ido ()
  (setq ido-save-directory-list-file
        (concat spacemacs-cache-directory "ido.last")
        ;; enable fuzzy matching
        ido-enable-flex-matching t)
  (ido-mode t))

(defun spacemacs-completion/init-ido-vertical-mode ()
  (use-package ido-vertical-mode
    :init
    (progn
      (ido-vertical-mode t)
      (defun spacemacs//ido-minibuffer-setup ()
        "Setup the minibuffer."
        ;; Since ido is implemented in a while loop where each
        ;; iteration setup a whole new minibuffer, we have to keep
        ;; track of any activated ido navigation transient-state and force
        ;; the reactivation at each iteration.
        (when spacemacs--ido-navigation-ms-enabled
          (spacemacs/ido-navigation-micro-state)))
      (add-hook 'ido-minibuffer-setup-hook 'spacemacs//ido-minibuffer-setup)

      (defun spacemacs//ido-setup ()
        (when spacemacs--ido-navigation-ms-face-cookie-minibuffer
          (face-remap-remove-relative
           spacemacs--ido-navigation-ms-face-cookie-minibuffer))
        ;; be sure to wipe any previous transient-state flag
        (setq spacemacs--ido-navigation-ms-enabled nil)
        ;; overwrite the key bindings for ido vertical mode only
        (define-key ido-completion-map (kbd "C-<return>") 'ido-select-text)
        ;; use M-RET in terminal
        (define-key ido-completion-map "\M-\r" 'ido-select-text)
        (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
        (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
        (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
        (define-key ido-completion-map (kbd "C-S-h") 'ido-prev-match-dir)
        (define-key ido-completion-map (kbd "C-S-j") 'next-history-element)
        (define-key ido-completion-map (kbd "C-S-k") 'previous-history-element)
        (define-key ido-completion-map (kbd "C-S-l") 'ido-next-match-dir)
        (define-key ido-completion-map (kbd "C-S-n") 'next-history-element)
        (define-key ido-completion-map (kbd "C-S-p") 'previous-history-element)
        ;; ido-other window maps
        (define-key ido-completion-map (kbd "C-o") 'spacemacs/ido-invoke-in-other-window)
        (define-key ido-completion-map (kbd "C-s") 'spacemacs/ido-invoke-in-vertical-split)
        (define-key ido-completion-map (kbd "C-t") 'spacemacs/ido-invoke-in-new-frame)
        (define-key ido-completion-map (kbd "C-v") 'spacemacs/ido-invoke-in-horizontal-split)
        ;; more natural navigation keys: up, down to change current item
        ;; left to go up dir
        ;; right to open the selected item
        (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
        (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
        (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
        ;; initiate transient-state
        (define-key ido-completion-map (kbd "M-SPC") 'spacemacs/ido-navigation-micro-state)
        (define-key ido-completion-map (kbd "s-M-SPC") 'spacemacs/ido-navigation-micro-state)
        )
      (add-hook 'ido-setup-hook 'spacemacs//ido-setup)

      (defun spacemacs/ido-invoke-in-other-window ()
        "signals ido mode to switch to (or create) another window after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'other)
        (ido-exit-minibuffer))

      (defun spacemacs/ido-invoke-in-horizontal-split ()
        "signals ido mode to split horizontally and switch after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'horizontal)
        (ido-exit-minibuffer))

      (defun spacemacs/ido-invoke-in-vertical-split ()
        "signals ido mode to split vertically and switch after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'vertical)
        (ido-exit-minibuffer))

      (defun spacemacs/ido-invoke-in-new-frame ()
        "signals ido mode to create a new frame after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'frame)
        (ido-exit-minibuffer))

      (defadvice ido-read-internal
          (around ido-read-internal-with-minibuffer-other-window activate)
        (let* (ido-exit-minibuffer-target-window
               (this-buffer (current-buffer))
               (result ad-do-it))
          (cond
           ((equal ido-exit-minibuffer-target-window 'other)
            (if (= 1 (count-windows))
                (spacemacs/split-window-horizontally-and-switch)
              (other-window 1)))
           ((equal ido-exit-minibuffer-target-window 'horizontal)
            (spacemacs/split-window-horizontally-and-switch))

           ((equal ido-exit-minibuffer-target-window 'vertical)
            (spacemacs/split-window-vertically-and-switch))
           ((equal ido-exit-minibuffer-target-window 'frame)
            (make-frame)))
          ;; why? Some ido commands, such as textmate.el's
          ;; textmate-goto-symbol don't switch the current buffer
          (switch-to-buffer this-buffer)
          result))

      (defvar spacemacs--ido-navigation-ms-enabled nil
        "Flag which is non nil when ido navigation transient-state is enabled.")

      (defvar spacemacs--ido-navigation-ms-face-cookie-minibuffer nil
        "Cookie pointing to the local face remapping.")

      (defface spacemacs-ido-navigation-ms-face
        `((t :background ,(face-attribute 'error :foreground)
             :foreground "black"
             :weight bold))
        "Face for ido minibuffer prompt when ido transient-state is activated."
        :group 'spacemacs)

      (defun spacemacs//ido-navigation-ms-set-face ()
        "Set faces for ido navigation transient-state."
        (setq spacemacs--ido-navigation-ms-face-cookie-minibuffer
              (face-remap-add-relative
               'minibuffer-prompt
               'spacemacs-ido-navigation-ms-face)))

      (defun spacemacs//ido-navigation-ms-on-enter ()
        "Initialization of ido transient-state."
        (setq spacemacs--ido-navigation-ms-enabled t)
        (spacemacs//ido-navigation-ms-set-face))

      (defun spacemacs//ido-navigation-ms-on-exit ()
        "Action to perform when exiting ido transient-state."
        (face-remap-remove-relative
         spacemacs--ido-navigation-ms-face-cookie-minibuffer))

      (defun spacemacs//ido-navigation-ms-full-doc ()
        "Full documentation for ido navigation transient-state."
        "
  [?]          display this help
  [e]          enter dired
  [j] [k]      next/previous match
  [J] [K]      sub/parent directory
  [h]          delete backward or parent directory
  [l]          select match
  [n] [p]      next/previous directory in history
  [o]          open in other window
  [s]          open in a new horizontal split
  [t]          open in other frame
  [v]          open in a new vertical split
  [q]          quit")

      (spacemacs|define-transient-state ido-navigation
        :title "ido Transient State"
        :foreign-keys run
        :on-enter (spacemacs//ido-navigation-ms-on-enter)
        :on-exit  (spacemacs//ido-navigation-ms-on-exit)
        :bindings
        ;;("?" nil (spacemacs//ido-navigation-ms-full-doc))
        ("<RET>" ido-exit-minibuffer :exit t)
        ("<escape>" nil :exit t)
        ("e" ido-select-text :exit t)
        ("h" ido-delete-backward-updir)
        ("j" ido-next-match)
        ("J" ido-next-match-dir)
        ("k" ido-prev-match)
        ("K" ido-prev-match-dir)
        ("l" ido-exit-minibuffer :exit t)
        ("n" ido-next-match-dir)
        ("o" spacemacs/ido-invoke-in-other-window :exit t)
        ("p" ido-prev-match-dir)
        ("q" nil :exit t)
        ("s" spacemacs/ido-invoke-in-vertical-split :exit t)
        ("t" spacemacs/ido-invoke-in-new-frame :exit t)
        ("v" spacemacs/ido-invoke-in-horizontal-split :exit t)))))
