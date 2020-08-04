;;; packages.el --- Spell Checking Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spell-checking-packages
  '(
    auto-dictionary
    flyspell
    flyspell-correct
    (flyspell-correct-ivy :toggle (configuration-layer/layer-used-p 'ivy))
    (flyspell-correct-helm :toggle (configuration-layer/layer-used-p 'helm))
    (flyspell-correct-popup :toggle (and (not (configuration-layer/layer-used-p 'ivy))
                                         (not (configuration-layer/layer-used-p 'helm))))
    (flyspell-popup :toggle enable-flyspell-auto-completion)
    ))

(defun spell-checking/init-auto-dictionary ()
  (use-package auto-dictionary
    :defer t
    :if spell-checking-enable-auto-dictionary
    :init
    (progn
      (add-hook 'flyspell-mode-hook 'auto-dictionary-mode)
      ;; Select the buffer local dictionary if it was set, otherwise
      ;; auto-dictionary will replace it with a guessed one at each activation.
      ;; https://github.com/nschum/auto-dictionary-mode/issues/5
      (defun spacemacs//adict-set-local-dictionary ()
        "Set the local dictionary if not nil."
        (when (and (fboundp 'adict-change-dictionary)
                   ispell-local-dictionary)
          (adict-change-dictionary ispell-local-dictionary)))
      (add-hook 'auto-dictionary-mode-hook
                'spacemacs//adict-set-local-dictionary 'append))))

(defun spell-checking/init-flyspell ()
  (use-package flyspell
    :defer t
    :commands (spell-checking/change-dictionary)
    :init
    (progn
      (spacemacs|define-transient-state spell-checking
        :title "Spell Checking Transient State"
        :doc "
Spell Commands^^            Add To Dictionary^^               Other
--------------^^----------  -----------------^^-------------  -----^^---------------------------
[_b_] check whole buffer    [_B_] add word to dict (buffer)   [_t_] toggle spell check
[_r_] check region          [_G_] add word to dict (global)   [_q_] exit
[_d_] change dictionary     [_S_] add word to dict (session)  [_Q_] exit and disable spell check
[_n_] next spell error
[_c_] correct before point
[_s_] correct at point"
        :on-enter (flyspell-mode)
        :bindings
        ("B" spacemacs/add-word-to-dict-buffer)
        ("b" flyspell-buffer)
        ("r" flyspell-region)
        ("d" spell-checking/change-dictionary)
        ("G" spacemacs/add-word-to-dict-global)
        ("n" flyspell-goto-next-error)
        ("c" flyspell-correct-wrapper)
        ("Q" flyspell-mode :exit t)
        ("q" nil :exit t)
        ("S" spacemacs/add-word-to-dict-session)
        ("s" flyspell-correct-at-point)
        ("t" spacemacs/toggle-spelling-checking))

      (spacemacs/set-leader-keys "S." 'spacemacs/spell-checking-transient-state/body)
      (spell-checking/add-flyspell-hook 'text-mode-hook)
      (when spell-checking-enable-by-default
        (add-hook 'prog-mode-hook 'flyspell-prog-mode))

      (spacemacs|add-toggle spelling-checking
        :status flyspell-mode
        :on (if (derived-mode-p 'prog-mode)
                (flyspell-prog-mode)
              (flyspell-mode))
        :off (progn
               (flyspell-mode-off)
               ;; Also disable auto-dictionary when disabling spell-checking.
               (when (fboundp 'auto-dictionary-mode) (auto-dictionary-mode -1)))
        :documentation "Enable automatic spell checking."
        :evil-leader "tS")

      (spacemacs/declare-prefix "S" "spelling")
      (spacemacs/declare-prefix "Sa" "add word to dict")
      (spacemacs/set-leader-keys
        "Sab" 'spacemacs/add-word-to-dict-buffer
        "Sag" 'spacemacs/add-word-to-dict-global
        "Sas" 'spacemacs/add-word-to-dict-session
        "Sb" 'flyspell-buffer
        "Sr" 'flyspell-region
        "Sd" 'spell-checking/change-dictionary
        "Sn" 'flyspell-goto-next-error
        "Ss" 'flyspell-correct-at-point))
    :config (spacemacs|diminish flyspell-mode " Ⓢ" " S")))

(defun spell-checking/init-flyspell-correct ()
  (use-package flyspell-correct
    :commands (flyspell-correct-at-point
               flyspell-correct-wrapper)
    :init
    (spacemacs/set-leader-keys "Sc" #'flyspell-correct-wrapper)))

(defun spell-checking/init-flyspell-correct-ivy ()
  (use-package flyspell-correct-ivy
    :commands (flyspell-correct-ivy)
    :init
    (setq flyspell-correct-interface #'flyspell-correct-ivy)))

(defun spell-checking/init-flyspell-correct-helm ()
  (use-package flyspell-correct-helm
    :commands (flyspell-correct-helm)
    :init
    (setq flyspell-correct-interface #'flyspell-correct-helm)))

(defun spell-checking/init-flyspell-correct-popup ()
  (use-package flyspell-correct-popup
    :commands (flyspell-correct-popup)
    :init
    (setq flyspell-correct-interface #'flyspell-correct-popup)))

(defun spell-checking/init-flyspell-popup ()
  (use-package flyspell-popup
    :defer t
    :init
    (progn
      (setq flyspell-popup-correct-delay 0.8)
      (add-hook 'flyspell-mode-hook 'flyspell-popup-auto-correct-mode))))
