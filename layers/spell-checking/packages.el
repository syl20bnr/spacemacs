;;; packages.el --- Spell Checking Layer packages File for Spacemacs
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

(setq spell-checking-packages
  '(
    flyspell
    helm-flyspell
    ))

(defun spell-checking/init-flyspell ()
  (use-package flyspell
    :defer t
    :init
    (progn
      (add-hook 'markdown-mode-hook 'flyspell-mode)
      (add-hook 'text-mode-hook 'flyspell-mode)
      (spacemacs|add-toggle spelling-checking
        :status flyspell-mode
        :on (flyspell-mode)
        :off (flyspell-mode -1)
        :documentation
        "Enable automatic spell checking."
        :evil-leader "tS")
      (evil-leader/set-key
        "Sd" 'ispell-change-dictionary
        "Sn" 'flyspell-goto-next-error))
    :config
    (progn
      (flyspell-prog-mode)
      (spacemacs|diminish flyspell-mode " Ⓢ" " S"))))

(defun spell-checking/init-helm-flyspell ()
  (use-package helm-flyspell
    :commands helm-flyspell-correct
    :init (evil-leader/set-key "Sc" 'helm-flyspell-correct)))
