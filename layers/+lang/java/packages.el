;;; packages.el --- Java Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar java-packages
  '(
    eclim
    company
    ))

(defun java/init-eclim ()
  (use-package eclim
    :defer t
    :diminish eclim-mode
    :init (add-hook 'java-mode-hook 'eclim-mode)
    :config
    (progn
      (setq help-at-pt-display-when-idle t
            help-at-pt-timer-delay 0.1)
      (help-at-pt-set-timer)

      (add-to-list 'minor-mode-alist
                   '(eclim-mode (:eval (eclim-modeline-string))))

      (evil-define-key 'insert java-mode-map
        (kbd ".") 'spacemacs/java-completing-dot
        (kbd ":") 'spacemacs/java-completing-double-colon
        (kbd "M-.") 'eclim-java-find-declaration
        (kbd "M-,") 'pop-tag-mark
        (kbd "M-<mouse-3>") 'eclim-java-find-declaration
        (kbd "<mouse-8>") 'pop-tag-mark)

      (evil-define-key 'normal java-mode-map
        (kbd "M-.") 'eclim-java-find-declaration
        (kbd "M-,") 'pop-tag-mark
        (kbd "M-<mouse-3>") 'eclim-java-find-declaration
        (kbd "<mouse-8>") 'pop-tag-mark)

      (evil-define-key 'normal eclim-problems-mode-map
        (kbd "a") 'eclim-problems-show-all
        (kbd "e") 'eclim-problems-show-errors
        (kbd "g") 'eclim-problems-buffer-refresh
        (kbd "q") 'eclim-quit-window
        (kbd "w") 'eclim-problems-show-warnings
        (kbd "f") 'eclim-problems-toggle-filefilter
        (kbd "c") 'eclim-problems-correct
        (kbd "RET") 'eclim-problems-open-current)

      (evil-define-key 'normal eclim-project-mode-map
        (kbd "N") 'eclim-project-create
        (kbd "m") 'eclim-project-mark-current
        (kbd "M") 'eclim-project-mark-all
        (kbd "u") 'eclim-project-unmark-current
        (kbd "U") 'eclim-project-unmark-all
        (kbd "o") 'eclim-project-open
        (kbd "c") 'eclim-project-close
        (kbd "i") 'eclim-project-info-mode
        (kbd "I") 'eclim-project-import
        (kbd "RET") 'eclim-project-goto
        (kbd "D") 'eclim-project-delete
        (kbd "p") 'eclim-project-update
        (kbd "g") 'eclim-project-mode-refresh
        (kbd "R") 'eclim-project-rename
        (kbd "q") 'eclim-quit-window)

      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "ea" 'eclim-problems-show-all
        "eb" 'eclim-problems
        "ec" 'eclim-problems-correct
        "ee" 'eclim-problems-show-errors
        "ef" 'eclim-problems-toggle-filefilter
        "en" 'eclim-problems-next-same-window
        "eo" 'eclim-problems-open
        "ep" 'eclim-problems-previous-same-window
        "ew" 'eclim-problems-show-warnings

        "ff" 'eclim-java-find-generic

        "gg" 'eclim-java-find-declaration
        "gt" 'eclim-java-find-type

        "rc" 'eclim-java-constructor
        "rg" 'eclim-java-generate-getter-and-setter
        "rf" 'eclim-java-format
        "ri" 'eclim-java-import-organize
        "rj" 'eclim-java-implement
        "rr" 'eclim-java-refactor-rename-symbol-at-point

        "hc" 'eclim-java-call-hierarchy
        "hh" 'eclim-java-show-documentation-for-current-element
        "hi" 'eclim-java-hierarchy
        "hu" 'eclim-java-find-references

        "mi" 'spacemacs/java-maven-clean-install
        "mI" 'spacemacs/java-maven-install
        "mp" 'eclim-maven-lifecycle-phases
        "mr" 'eclim-maven-run
        "mR" 'eclim-maven-lifecycle-phase-run
        "mt" 'spacemacs/java-maven-test

        "aa" 'eclim-ant-run
        "ac" 'eclim-ant-clear-cache
        "ar" 'eclim-ant-run
        "av" 'eclim-ant-validate

        "pb" 'eclim-project-build
        "pc" 'eclim-project-create
        "pd" 'eclim-project-delete
        "pg" 'eclim-project-goto
        "pi" 'eclim-project-import
        "pj" 'eclim-project-info-mode
        "pk" 'eclim-project-close
        "po" 'eclim-project-open
        "pp" 'eclim-project-mode
        "pu" 'eclim-project-update

        "tt" 'eclim-run-junit)))

  (use-package company-emacs-eclim
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init
    (push 'company-emacs-eclim company-backends-java-mode)))

(defun java/post-init-company ()
  (spacemacs|add-company-hook java-mode))
