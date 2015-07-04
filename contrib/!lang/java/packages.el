;;; packages.el --- Java Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Lukasz Klich
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar java-packages
  '(
    emacs-eclim
    company
    ))

(defun java/init-emacs-eclim ()
  (use-package eclim
    :config
    (progn
      (message "starting eclim mode")
      (global-eclim-mode)
      (setq help-at-pt-display-when-idle t)
      (setq help-at-pt-timer-delay 0.1)
      (help-at-pt-set-timer)

      (add-to-list 'minor-mode-alist
             '(eclim-mode (:eval (eclim-modeline-string))))

      (evil-define-key 'insert java-mode-map
        (kbd ".") 'java/completing-dot
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

      (evil-leader/set-key-for-mode 'java-mode

        "meo" 'eclim-problems-open
        "meb" 'eclim-problems
        "mea" 'eclim-problems-show-all
        "mee" 'eclim-problems-show-errors
        "mew" 'eclim-problems-show-warnings
        "mef" 'eclim-problems-toggle-filefilter
        "men" 'eclim-problems-next-same-window
        "mep" 'eclim-problems-previous-same-window
        "mec" 'eclim-problems-correct

        "mgg" 'eclim-java-find-declaration
        "mgt" 'eclim-java-find-type
        "mff" 'eclim-java-find-generic

        "mrf" 'eclim-java-format
        "mrr" 'eclim-java-refactor-rename-symbol-at-point
        "mri" 'eclim-java-import-organize
        "mrg" 'eclim-java-generate-getter-and-setter
        "mrc" 'eclim-java-constructor
        "mrj" 'eclim-java-implement


        "mhh" 'eclim-java-show-documentation-for-current-element
        "mhu" 'eclim-java-find-references
        "mhc" 'eclim-java-call-hierarchy
        "mhi" 'eclim-java-hierarchy

        "mmm" 'java/maven-clean-install
        "mmp" 'eclim-maven-lifecycle-phases
        "mmR" 'eclim-maven-lifecycle-phase-run
        "mmr" 'eclim-maven-run
        "mmt" 'java/maven-test
        "mmi" 'java/maven-clean-install
        "mmI" 'java/maven-install

        "maa" 'eclim-ant-run
        "mar" 'eclim-ant-run
        "mav" 'eclim-ant-validate
        "mac" 'eclim-ant-clear-cache

        "mpj" 'eclim-project-info-mode
        "mpo" 'eclim-project-open
        "mpb" 'eclim-project-build
        "mpd" 'eclim-project-delete
        "mpg" 'eclim-project-goto
        "mpi" 'eclim-project-import
        "mpc" 'eclim-project-create
        "mpk" 'eclim-project-close
        "mpp" 'eclim-project-mode
        "mpu" 'eclim-project-update

        "mtt" 'eclim-run-junit
        "mtT" 'java/maven-test)))

  (use-package company-emacs-eclim
    ;;:if (configuration-layer/layer-usedp 'company)
    :config (progn
              (message "company-emacs-eclim-setup")
              (company-emacs-eclim-setup)
              (global-company-mode t))))

(defun java/post-init-company ()
  (spacemacs|add-company-hook java-mode))
