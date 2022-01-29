;;; packages.el --- Chinese Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq chinese-packages
      '(
        (pyim :toggle (eq chinese-default-input-method 'pinyin))
        (chinese-wbim :toggle (eq chinese-default-input-method 'wubi))
        (fcitx :toggle chinese-enable-fcitx)
        find-by-pinyin-dired
        ace-pinyin
        pangu-spacing
        org
        (youdao-dictionary :toggle chinese-enable-youdao-dict)
        chinese-conv))


(defun chinese/init-fcitx ()
  (use-package fcitx
    :init (fcitx-evil-turn-on)
    :config
    (progn
      (setq fcitx-active-evil-states '(insert emacs hybrid))
      (when chinese-use-fcitx5
        (setq fcitx-remote-command "fcitx5-remote"))
      (fcitx-default-setup)
      (fcitx-prefix-keys-add "M-m" "C-M-m")
      (when chinese-fcitx-use-dbus
        (setq fcitx-use-dbus t)))))

(defun chinese/init-chinese-wbim ()
  "Initialize chinese-wubi"
  (use-package chinese-wbim
    :if (eq 'wubi chinese-default-input-method)
    :init
    (progn
      (autoload 'chinese-wbim-use-package "chinese-wubi"
        "Another emacs input method")
      ;; Tooptip is not good enough, so disable it here.
      (setq chinese-wbim-use-tooltip nil)
      (register-input-method
       "chinese-wubi" "euc-cn" 'chinese-wbim-use-package
       "五笔" "汉字五笔输入法" "wb.txt")
      (require 'chinese-wbim-extra)
      (global-set-key ";" 'chinese-wbim-insert-ascii)
      (setq default-input-method 'chinese-wubi))))

(defun chinese/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :if chinese-enable-youdao-dict
    :defer
    :config
    (progn
      ;; Enable Cache
      (setq url-automatic-caching t
            ;; Set file path for saving search history
            youdao-dictionary-search-history-file
            (concat spacemacs-cache-directory ".youdao")
            ;; Enable Chinese word segmentation support
            youdao-dictionary-use-chinese-word-segmentation t))))

(defun chinese/init-pyim ()
  (use-package pyim
    :if (eq 'pinyin chinese-default-input-method)
    :init
    (progn
      (setq pyim-page-tooltip t
            pyim-directory (expand-file-name "pyim/" spacemacs-cache-directory)
            pyim-dcache-directory (expand-file-name "dcache/" pyim-directory)
            default-input-method "pyim")
      (evilified-state-evilify pyim-dm-mode pyim-dm-mode-map))))

(defun chinese/init-find-by-pinyin-dired ()
  (use-package find-by-pinyin-dired
    :defer t))

(defun chinese/init-ace-pinyin ()
  (use-package ace-pinyin
    :defer t
    :init
    (progn
      (if chinese-enable-avy-pinyin
          (setq ace-pinyin-use-avy t))
      (ace-pinyin-global-mode t)
      (spacemacs|hide-lighter ace-pinyin-mode))))

(defun chinese/init-pangu-spacing ()
  (use-package pangu-spacing
    :defer t
    :init (progn (global-pangu-spacing-mode 1)
                 (spacemacs|hide-lighter pangu-spacing-mode)
                 ;; Always insert `real' space in org-mode.
                 (add-hook 'org-mode-hook
                           #'(lambda ()
                              (setq-local pangu-spacing-real-insert-separtor t))))))

(defun chinese/init-chinese-conv ()
  (use-package chinese-conv
    :defer t))

(defun chinese/post-init-org ()
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents))))
