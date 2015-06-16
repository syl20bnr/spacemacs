;;; extensions.el --- chinese Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq chinese-post-extensions
      '(
        eim
        ))


(defun chinese/init-eim ()
  "Initialize eim"
  (use-package eim
    :if chinese-im-enable-wubi
    :init
    (progn
      (autoload 'eim-use-package "eim" "Another emacs input method")
      ;; Tooptip is not good enough
      (setq eim-use-tooltip nil)

      (register-input-method
       "eim-wb" "euc-cn" 'eim-use-package
       "五笔" "汉字五笔输入法" "wb.txt")
      (register-input-method
       "eim-py" "euc-cn" 'eim-use-package
       "拼音" "汉字拼音输入法" "py.txt")
      ;; 用 ; 暂时输入英文
      (require 'eim-extra)
      (global-set-key ";" 'eim-insert-ascii)
      ;; 设置默认输入法为 五笔
      (setq default-input-method 'eim-wb)
      )))
