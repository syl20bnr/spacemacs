;;; packages.el --- ensime layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Tor Hedin Brønner <torhedinbronner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ensime-packages '(ensime))

(defun ensime/init-ensime ()
  (use-package ensime
    :defer t
    :commands ensime-mode
    :config
    (progn
      (setq ensime-startup-dirname (expand-file-name "ensime" spacemacs-cache-directory))

      (evil-define-key 'insert ensime-mode-map
        (kbd ".") 'ensime/completing-dot
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack)

      (evil-define-key 'normal ensime-mode-map
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack)

      (evil-define-key 'normal ensime-popup-buffer-map
        (kbd "q") 'ensime-popup-buffer-quit-function)

      (evil-define-key 'normal ensime-inspector-mode-map
        (kbd "q") 'ensime-popup-buffer-quit-function)

      (evil-define-key 'normal ensime-refactor-info-map
        (kbd "q") 'spacemacs/ensime-refactor-cancel
        (kbd "c") 'spacemacs/ensime-refactor-accept
        (kbd "RET") 'spacemacs/ensime-refactor-accept)

      (evil-define-key 'normal ensime-compile-result-map
        (kbd "g") 'ensime-show-all-errors-and-warnings
        (kbd "TAB") 'forward-button
        (kbd "<backtab>") 'backward-button
        (kbd "M-n") 'forward-button
        (kbd "M-p") 'backward-button
        (kbd "n") 'forward-button
        (kbd "N") 'backward-button)

      (defun ensime-gen-and-restart()
        "Regenerate `.ensime' file and restart the ensime server."
        (interactive)
        (progn
          (sbt-command ";ensimeConfig;ensimeConfigProject")
          (ensime-shutdown)
          (ensime)))

      (defun ensime-inf-eval-buffer-switch ()
        "Send buffer content to shell and switch to it in insert mode."
        (interactive)
        (ensime-inf-eval-buffer)
        (ensime-inf-switch)
        (evil-insert-state))

      (defun ensime-inf-eval-region-switch (start end)
        "Send region content to shell and switch to it in insert mode."
        (interactive "r")
        (ensime-inf-switch)
        (ensime-inf-eval-region start end)
        (evil-insert-state)))
    ))


;;; packages.el ends here
