;;; packages.el --- Chinese Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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
        (pyim :toggle chinese-default-input-method)
        (pyim-basedict :toggle (eq chinese-default-input-method 'pinyin))
        (pyim-wbdict :toggle (member chinese-default-input-method '(wubi wubi86 wubi98)))
        (fcitx :toggle chinese-enable-fcitx)
        find-by-pinyin-dired
        (ace-pinyin :toggle chinese-enable-avy-pinyin)
        pangu-spacing
        org
        (youdao-dictionary :toggle chinese-enable-youdao-dict)
        chinese-conv))

(defun chinese/init-fcitx ()
  (use-package fcitx
    :init (fcitx-evil-turn-on)
    :config
    (setq fcitx-active-evil-states '(insert emacs hybrid))
    (when chinese-use-fcitx5
      (setq fcitx-remote-command "fcitx5-remote"))
    (fcitx-default-setup)
    (fcitx-prefix-keys-add "M-m" "C-M-m")
    (when chinese-fcitx-use-dbus
      (setq fcitx-use-dbus t))))

(defun chinese/init-pyim ()
  (use-package pyim
    :if chinese-default-input-method
    :defer t
    :init
    (setq pyim-directory (expand-file-name "pyim/" spacemacs-cache-directory)
          pyim-dcache-directory (expand-file-name "dcache/" pyim-directory)
          pyim-assistant-scheme-enable t
          default-input-method "pyim")
    (autoload 'pyim-dict-manager-mode "pyim-dicts-manager"
      "Major mode for managing pyim dicts")
    (evilified-state-evilify-map pyim-dict-manager-mode-map
      :mode pyim-dict-manager-mode
      :eval-after-load pyim-dict-manager)))

(defun chinese/init-pyim-basedict ()
  "Initialize pyim-basedict"
  (use-package pyim-basedict
    :if (eq chinese-default-input-method 'pinyin)
    :defer t
    :config
    (pyim-basedict-enable)))

(defun chinese/init-pyim-wbdict ()
  "Initialize pyim-wbdict"
  (use-package pyim-wbdict
    :if (member chinese-default-input-method '(wubi wubi86 wubi98))
    :defer t
    :config
    (setq pyim-default-scheme 'wubi)
    (if (eq chinese-default-input-method 'wubi98)
        (pyim-wbdict-v98-enable)
      (pyim-wbdict-v86-enable))))

(defun chinese/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :if chinese-enable-youdao-dict
    :defer t
    :config
    ;; Enable Cache
    (setq url-automatic-caching t
          ;; Set file path for saving search history
          youdao-dictionary-search-history-file
          (concat spacemacs-cache-directory ".youdao")
          ;; Enable Chinese word segmentation support
          youdao-dictionary-use-chinese-word-segmentation t)))

(defun chinese/init-find-by-pinyin-dired ()
  (use-package find-by-pinyin-dired
    :defer t))

(defun chinese/init-ace-pinyin ()
  (use-package ace-pinyin
    :defer t
    :init
    (if chinese-enable-avy-pinyin
        (setq ace-pinyin-use-avy t))
    (ace-pinyin-global-mode t)
    (spacemacs|hide-lighter ace-pinyin-mode)))

(defun chinese/init-pangu-spacing ()
  (use-package pangu-spacing
    :defer t
    :init
    (global-pangu-spacing-mode 1)
    (spacemacs|hide-lighter pangu-spacing-mode)
    ;; Always insert `real' space in org-mode.
    (add-hook 'org-mode-hook
              (lambda ()
                (setq-local pangu-spacing-real-insert-separtor t)))))

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
