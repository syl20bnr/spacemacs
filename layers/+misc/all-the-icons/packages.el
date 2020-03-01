;;; packages.el --- all-the-icons layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author:  <Steven Allen <steven@stebalien.com>>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst all-the-icons-packages
  '(
    (all-the-icons :protected t)
    ivy-rich
    (all-the-icons-ivy :toggle (configuration-layer/package-used-p 'ivy))
    (all-the-icons-ibuffer :toggle (configuration-layer/package-used-p 'ibuffer))
    (all-the-icons-dired :toggle (configuration-layer/package-used-p 'dired))
    company-box)
  "The list of Lisp packages required by the all-the-icons layer.")

(defun all-the-icons/init-all-the-icons ()
  (use-package all-the-icons
    :defer t
    :config
    ;; EXWM
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(exwm-mode all-the-icons-octicon "browser" :face all-the-icons-dsilver))
    ;; Notmuch
    (add-to-list 'all-the-icons-mode-icon-alist
                  '(notmuch-show-mode all-the-icons-octicon "mail-read" :face all-the-icons-lblue))
    (add-to-list 'all-the-icons-mode-icon-alist
                  '(notmuch-search-mode all-the-icons-material "inbox" :face all-the-icons-blue))
    (add-to-list 'all-the-icons-mode-icon-alist
                  '(notmuch-message-mode all-the-icons-material "email" :face all-the-icons-orange))

    ;; Missing directory type (for ivy)
    (add-to-list 'all-the-icons-icon-alist '("/$" all-the-icons-octicon "file-directory"))

    ;; Switch to a go icon that shows up. The default one is practically invisible.
    (add-to-list 'all-the-icons-mode-icon-alist '(go-mode all-the-icons-fileicon "go" :face all-the-icons-lblue))
    (add-to-list 'all-the-icons-icon-alist '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-lblue))

    ;; systemd
    (add-to-list 'all-the-icons-mode-icon-alist '(systemd-mode all-the-icons-faicon "cogs" :face all-the-icons-dred))
    (add-to-list 'all-the-icons-icon-alist `(,systemd-autoload-regexp all-the-icons-faicon "cogs" :face all-the-icons-dred))

    ;; matrix
    (add-to-list 'all-the-icons-mode-icon-alist '(matrix-client-mode all-the-icons-material "chat" :face all-the-icons-dcyan))

    ;; Spacemacs home
    (add-to-list 'all-the-icons-mode-icon-alist
                  '(spacemacs-buffer-mode all-the-icons-material "home" :face all-the-icons-purple))))

(defun all-the-icons/init-all-the-icons-dired ()
    (use-package all-the-icons-dired
      :defer t
      :init
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(defun all-the-icons/init-all-the-icons-ibuffer ()
  (use-package all-the-icons-ibuffer
    :defer t
    :init
    (add-hook 'ibuffer-mode-hook 'all-the-icons-ibuffer-mode)))

(defun all-the-icons/init-all-the-icons-ivy ()
    (use-package all-the-icons-ivy
      :after ivy
      :config
      (all-the-icons-ivy-setup)))

(defun all-the-icons/post-init-ivy-rich ()
  (with-eval-after-load 'ivy-rich
    (defun ivy-rich-file-icon (candidate)
      (let ((icon (all-the-icons-icon-for-file candidate)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon)))
    (defun ivy-rich-buffer-icon (candidate)
      (let ((icon (all-the-icons-icon-for-mode
                   (buffer-local-value 'major-mode (get-buffer candidate)))))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon)))))

(defun all-the-icons/post-init-company-box ()
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (with-eval-after-load 'company-box
    ;; Icons selected by liguangsheng
    ;; https://github.com/liguangsheng/emacsd/blob/master/lisp/init-completion.el
      (defun spacemacs/company-box-icon (family icon &rest args)
        (when icon
          (let ((icon (pcase family
                        ('octicon (all-the-icons-octicon icon :height 0.8 :v-adjust -0.05 args))
                        ('faicon (all-the-icons-faicon icon :height 0.8 :v-adjust -0.0575))
                        ('material (all-the-icons-material icon :height 0.8 :v-adjust -0.225 args))
                        ('alltheicon (all-the-icons-alltheicon icon :height 0.8 args)))))
            (unless (symbolp icon)
              (concat icon
                      (propertize " " 'face 'variable-pitch))))))
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(spacemacs/company-box-icon 'octicon "file-text"))
              (Text . ,(spacemacs/company-box-icon 'faicon "file-text-o"))
              (Method . ,(spacemacs/company-box-icon 'faicon "cube"))
              (Function . ,(spacemacs/company-box-icon 'faicon "cube"))
              (Constructor . ,(spacemacs/company-box-icon 'faicon "cube"))
              (Field . ,(spacemacs/company-box-icon 'faicon "tag"))
              (Variable . ,(spacemacs/company-box-icon 'faicon "tag"))
              (Class . ,(spacemacs/company-box-icon 'faicon "cog"))
              (Interface . ,(spacemacs/company-box-icon 'faicon "cogs"))
              (Module . ,(spacemacs/company-box-icon 'alltheicon "less"))
              (Property . ,(spacemacs/company-box-icon 'faicon "wrench"))
              (Unit . ,(spacemacs/company-box-icon 'faicon "tag"))
              (Value . ,(spacemacs/company-box-icon 'faicon "tag"))
              (Enum . ,(spacemacs/company-box-icon 'faicon "file-text-o"))
              (Keyword . ,(spacemacs/company-box-icon 'material "format_align_center"))
              (Snippet . ,(spacemacs/company-box-icon 'material "content_paste"))
              (Color . ,(spacemacs/company-box-icon 'material "palette"))
              (File . ,(spacemacs/company-box-icon 'faicon "file"))
              (Reference . ,(spacemacs/company-box-icon 'faicon "tag"))
              (Folder . ,(spacemacs/company-box-icon 'faicon "folder"))
              (EnumMember . ,(spacemacs/company-box-icon 'faicon "tag"))
              (Constant . ,(spacemacs/company-box-icon 'faicon "tag"))
              (Struct . ,(spacemacs/company-box-icon 'faicon "cog"))
              (Event . ,(spacemacs/company-box-icon 'faicon "bolt"))
              (Operator . ,(spacemacs/company-box-icon 'faicon "tag"))
              (TypeParameter . ,(spacemacs/company-box-icon 'faicon "cog"))
              (Template . ,(spacemacs/company-box-icon 'octicon "file-code"))))))
