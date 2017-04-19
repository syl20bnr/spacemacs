;;; packages.el --- chinese Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq chinese-packages
    '(
      youdao-dictionary
      chinese-pyim
      find-by-pinyin-dired
      ace-pinyin
      chinese-wbim
      ))

(defun chinese/init-chinese-wbim ()
  "Initialize chinese-wubi"
  (use-package chinese-wbim
    :if chinese-im-enable-wubi
    :init
    (progn
      (autoload 'chinese-wbim-use-package "chinese-wubi" "Another emacs input method")
      ;; Tooptip is not good enough, so disable it here.
      (setq chinese-wbim-use-tooltip nil)

      (register-input-method
       "chinese-wubi" "euc-cn" 'chinese-wbim-use-package
       "五笔" "汉字五笔输入法" "wb.txt")
      (require 'chinese-wbim-extra)
      (global-set-key ";" 'chinese-wbim-insert-ascii)
      (setq default-input-method 'chinese-wubi)
      )))

(defun chinese/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :config
    (progn
      ;; Enable Cache
      (setq url-automatic-caching t)

      ;; Set file path for saving search history
      (setq youdao-dictionary-search-history-file (concat spacemacs-cache-directory ".youdao"))

      ;; Enable Chinese word segmentation support 
      (setq youdao-dictionary-use-chinese-word-segmentation t)
      )))

(defun chinese/init-chinese-pyim ()
  (use-package chinese-pyim
    :if chinese-im-enable-pinyin
    :init
    (progn
      (setq pyim-use-tooltip t)
      (setq pyim-dicts-directory spacemacs-cache-directory)
      (setq pyim-personal-file (concat spacemacs-cache-directory "pyim-personal.txt"))
      (setq default-input-method "chinese-pyim")
      (evilify pyim-dicts-manager-mode pyim-dicts-manager-mode-map)
      )))


(defun chinese/init-find-by-pinyin-dired ()
  (use-package find-by-pinyin-dired
    :defer t
    ))

(defun chinese/init-ace-pinyin ()
  (use-package ace-pinyin
    :defer t
    :init
    (progn
      (ace-pinyin-global-mode t)
      (spacemacs|hide-lighter ace-pinyin-mode))))
