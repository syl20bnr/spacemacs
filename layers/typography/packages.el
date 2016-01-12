;;; packages.el --- typography Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq typography-packages
      '(
        typo
        ))

(when (version<= "25" emacs-version)
  (push 'tildify typography-packages))

(setq typography-excluded-packages '())

(defun typography/init-typo ()
  (use-package typo
    :defer t
    :init
    (progn
      (when typography-enable-typographic-editing
        (add-hook 'text-mode-hook 'typo-mode))

      (spacemacs|add-toggle typographic-substitutions
        :status typo-mode
        :on (typo-mode)
        :off (typo-mode -1)
        :documentation "Enable typographic substitutions"
        :evil-leader "tT")
      (spacemacs|diminish typo-mode " Ⓣ" " T"))
    :config (setq-default typo-language "English")))

(defun typography/init-tildify ()
  (use-package tildify
    :defer t
    :init
    (progn
      (when typography-enable-typographic-editing
        (add-hook 'text-mode-hook 'tildify-mode))

      (spacemacs/set-leader-keys
        "x~" 'tildify-region)

      ;; Use the symbolic non-breaking space for LaTeX
      (defun typography/tildify-latex-space ()
        "Set tildify space for LaTeX"
        (setq-local tildify-space-string "~"))
      (add-hook 'LaTeX-mode-hook 'typography/tildify-latex-space)

      (spacemacs|add-toggle tildify-space
        :status tildify-mode
        :on (tildify-mode)
        :off (tildify-mode -1)
        :documentation "Enable electric non-breaking space"
        :evil-leader "t~")
      (spacemacs|diminish tildify-mode " ~" " ~"))))
