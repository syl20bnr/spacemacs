;;; packages.el --- react Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2015 Sylvain Benner
;; Copyright (c) 2014-2015 Andrea Moretti & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq react-packages
    '(
      web-mode
      js2-mode
      flycheck
      tern
      company
      company-tern
      ))

;; List of packages to exclude.
(setq react-excluded-packages '())

;; For each package, define a function react/init-<package-name>
;;
(defun react/post-init-flycheck ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (with-eval-after-load 'flycheck
                  ;; use eslint with web-mode for jsx files
                  (flycheck-add-mode 'javascript-eslint 'web-mode)

                  ;; disable jshint since we prefer eslint checking
                  (setq-default flycheck-disabled-checkers
                                (append flycheck-disabled-checkers
                                        '(javascript-jshint)))

                  ;; disable json-jsonlist checking for json files
                  (setq-default flycheck-disabled-checkers
                                (append flycheck-disabled-checkers
                                        '(json-jsonlist))))))))

(defun react/post-init-web-mode ()
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (defadvice web-mode-highlight-part (around tweak-jsx activate)
                (if (equal web-mode-content-type "jsx")
                    (let ((web-mode-enable-part-face nil))
                      ad-do-it)
                  ad-do-it)))))

(defun react/post-init-js2-mode ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (js2-minor-mode)))))

(defun react/pre-init-tern ()
  (spacemacs|use-package-add-hook tern
    :post-config
    (progn
      ;;(evil-leader/set-key-for-mode 'web-mode "mrrV" 'tern-rename-variable)
      (evil-leader/set-key-for-mode 'web-mode "mhd" 'tern-get-docs)
      (evil-leader/set-key-for-mode 'web-mode "mgg" 'tern-find-definition)
      (evil-leader/set-key-for-mode 'web-mode "mgG" 'tern-find-definition-by-name)
      (evil-leader/set-key-for-mode 'web-mode (kbd "m C-g") 'tern-pop-find-definition)
      (evil-leader/set-key-for-mode 'web-mode "mht" 'tern-get-type))))

(defun react/post-init-tern ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (tern-mode)))))

(defun react/post-init-company-tern ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (push 'company-tern company-backends-web-mode)))))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
