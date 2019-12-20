;;; packages.el --- Spacemacs Completion Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
        flx-ido
        (ido :location built-in)
        (ido-vertical-mode :location built-in)
        ))

(defun spacemacs-completion/init-default-helm-config ()
  (setq helm-prevent-escaping-from-minibuffer t
        helm-bookmark-show-location t
        helm-display-header-line nil
        helm-split-window-inside-p t
        helm-always-two-windows t
        helm-echo-input-in-header-line t
        helm-imenu-execute-action-at-once-if-one nil
        helm-org-format-outline-path t
        helm-display-function 'spacemacs//display-helm-window)
  (with-eval-after-load 'helm
    (spacemacs|hide-lighter helm-mode)
    (when (and helm-enable-auto-resize
               (or (eq helm-position 'bottom)
                   (eq helm-position 'top)))
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
    (add-hook 'helm-find-files-after-init-hook
              'spacemacs//helm-find-files-enable-helm--in-fuzzy)
    ;; setup advices
    ;; fuzzy matching for all the sources
    (unless (eq helm-use-fuzzy 'source)
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
 [_j_/_k_] next/prev candidate   [_v_]^^    persistent action    [_e_]^^   edit occurrences
 [_h_/_l_] prev/next source      [_1_.._0_] action 1..10         [_t_/_T_] toggle visible/all mark
 [_g_/_G_] first/last candidate  [_a_]^^    action selection pg  [_q_]^^   quit"
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
        ("M-SPC" nil :exit t)
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
          ivy-re-builders-alist '((spacemacs/counsel-search . spacemacs/ivy--regex-plus)
                                  (t . ivy--regex-ignore-order)))
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
 [_g_/_G_] first/last | [_a_]^ ^  list all  | [_TAB_] alt done | [_q_] & quit | [_m_] matcher: %s(spacemacs//ivy-matcher-desc)
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
      ("M-SPC" nil)
      ("TAB" ivy-alt-done :exit nil)
      ;; ("C-j" ivy-alt-done :exit nil)
      ;; ("d" ivy-done :exit t)
      ("RET" ivy-done :exit t)
      ("c" ivy-call)
      ("C-m" ivy-done :exit t)
      ("C" ivy-toggle-calling)
      ("m" ivy-rotate-preferred-builders)
      (">" ivy-minibuffer-grow)
      ("<" ivy-minibuffer-shrink)
      ("w" ivy-prev-action)
      ("s" ivy-next-action)
      ("a" ivy-read-action)
      ("t" (setq truncate-lines (not truncate-lines)))
      ("f" ivy-toggle-case-fold)
      ("o" ivy-occur :exit t))
    (define-key ivy-minibuffer-map "\C-o" 'spacemacs/ivy-transient-state/body)
    (define-key ivy-minibuffer-map (kbd "M-SPC")
      'spacemacs/ivy-transient-state/body)
    (define-key ivy-minibuffer-map (kbd "s-M-SPC")
      'spacemacs/ivy-transient-state/body)
    (define-key ivy-minibuffer-map (kbd "s-M-SPC")
      'spacemacs/ivy-transient-state/body)))

(defun spacemacs-completion/init-flx-ido ()
  (use-package flx-ido
    :defer t
    :init (add-hook 'ido-vertical-mode-hook 'flx-ido-mode)))

(defun spacemacs-completion/init-ido ()
  (setq ido-save-directory-list-file
        (concat spacemacs-cache-directory "ido.last")
        ;; enable fuzzy matching
        ido-enable-flex-matching t)
  (ido-mode t))

(defun spacemacs-completion/init-ido-vertical-mode ()
  (use-package ido-vertical-mode
    :defer t
    :init
    (progn
      (add-hook 'ido-minibuffer-setup-hook ido-vertical-mode)
      (add-hook 'ido-minibuffer-setup-hook 'spacemacs//ido-minibuffer-setup)
      (add-hook 'ido-setup-hook 'spacemacs//ido-setup)

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

      (defvar spacemacs--ido-navigation-ts-enabled nil
        "Flag which is non nil when ido navigation transient-state is enabled.")

      (defvar spacemacs--ido-navigation-ts-face-cookie-minibuffer nil
        "Cookie pointing to the local face remapping.")

      (defface spacemacs-ido-navigation-ts-face
        `((t :background ,(face-attribute 'error :foreground)
             :foreground "black"
             :weight bold))
        "Face for ido minibuffer prompt when ido transient-state is activated."
        :group 'spacemacs)

      (spacemacs|define-transient-state ido-navigation
        :title "ido Transient State"
        :foreign-keys run
        :on-enter (spacemacs//ido-navigation-ts-on-enter)
        :on-exit  (spacemacs//ido-navigation-ts-on-exit)
        :bindings
        ;;("?" nil (spacemacs//ido-navigation-ts-full-doc))
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
