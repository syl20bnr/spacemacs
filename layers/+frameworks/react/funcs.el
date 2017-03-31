;;; funcs.el --- React Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; react mode

(defun spacemacs//setup-react-mode ()
  "Adjust web-mode to accommodate react-mode"
  (emmet-mode 0)
  ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
  (setq-local emmet-expand-jsx-className? t)
  ;; Enable js-mode snippets
  (yas-activate-extra-mode 'js-mode)
  ;; Force jsx content type
  (web-mode-set-content-type "jsx")
  ;; Don't auto-quote attribute values
  (setq-local web-mode-enable-auto-quoting nil))
