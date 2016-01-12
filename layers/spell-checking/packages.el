;;; packages.el --- Spell Checking Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    helm-flyspell
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

      (spacemacs/set-leader-keys
        "Sb" 'flyspell-buffer
        "Sd" 'spell-checking/change-dictionary
        "Sn" 'flyspell-goto-next-error))
    :config
    (spacemacs|diminish flyspell-mode " Ⓢ" " S")))

(defun spell-checking/init-helm-flyspell ()
  (use-package helm-flyspell
    :commands helm-flyspell-correct
    :init (spacemacs/set-leader-keys "Sc" 'helm-flyspell-correct)))
