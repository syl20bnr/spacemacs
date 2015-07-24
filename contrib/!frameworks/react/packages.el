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
      tern-mode
      ))

;; List of packages to exclude.
(setq react-excluded-packages '())

;; For each package, define a function react/init-<package-name>
;;
(defun react/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    ;; (setq flycheck-check-syntax-automatically '(save new-line mode-enabled))

    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))

    ;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist)))))

(defun react/post-init-web-mode ()
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (or (equal web-mode-content-type "jsx")
                        (equal web-mode-content-type "javascript"))
                (web-mode-set-content-type "jsx")
                (add-to-list 'company-backends 'company-tern)
                (js2-minor-mode)
                (tern-mode))))

  (with-eval-after-load 'web-mode
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
