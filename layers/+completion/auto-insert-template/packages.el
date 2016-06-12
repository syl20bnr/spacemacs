;;; packages.el --- auto-insert-template layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Yasuharu Iida <YasuharuIida@users.noreply.github.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst auto-insert-template-packages
  '(
    (autoinsert :location built-in)
    yasnippet
    yatemplate
    ))

(defun auto-insert-template/init-yatemplate ()
  (use-package yatemplate
    :config
    (yatemplate-fill-alist)
    (auto-insert-mode t)
    (setq auto-insert-query nil)))


;;; packages.el ends here
