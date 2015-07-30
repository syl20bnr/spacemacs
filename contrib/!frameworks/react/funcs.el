;;; funcs.el --- react Layer packages File for Spacemacs
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

(defun spacemacs//react-config-flycheck ()
  (with-eval-after-load 'flycheck
    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'react-mode)

    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))

    ;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist)))))

(defun spacemacs//react-config-web-mode ()
  (emmet-mode 0)
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (let ((web-mode-enable-part-face nil))
      ad-do-it)))

(defun spacemacs//react-load-js2-refactor ()
  "Lazy load js2-refactor"
  (require 'js2-refactor))

(defun spacemacs//react-load-js-doc ()
  "Lazy load js-doc"
  (require 'js-doc))

