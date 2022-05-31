;;; packages.el --- Japanese Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Kenji Miyazaki <kenjimyzk@gmail.com>
;; URL: https://github.com/kenjimyzk/
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
(setq japanese-packages
      '(
        evil-tutor-ja
        migemo
        avy-migemo
        ddskk
        japanese-holidays
        pangu-spacing
        org
        ))

(defun japanese/init-evil-tutor-ja ()
  (use-package evil-tutor-ja
    :defer t))

(defun japanese/init-migemo ()
  (use-package migemo
    :config
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs" "-i" "\a"))
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (setq search-default-regexp-mode nil)
    (cond
     ((eq system-type 'darwin)
      (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
     ((eq system-type 'gnu/linux)
      (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))))
  (migemo-init))

(defun japanese/init-avy-migemo ()
  (use-package avy-migemo
    :config (avy-migemo-mode 1)))

(defun japanese/init-ddskk ()
  (use-package ddskk
    :defer t
    :bind (("C-x j" . skk-mode))))

(defun japanese/init-japanese-holidays ()
  (use-package japanese-holidays
    :config
    (with-eval-after-load "holidays"
      (setq calendar-holidays
            (append japanese-holidays holiday-local-holidays holiday-other-holidays))
      (setq mark-holidays-in-calendar t)
      (setq japanese-holiday-weekend '(0 6)
            japanese-holiday-weekend-marker
            '(holiday nil nil nil nil nil japanese-holiday-saturday))
      (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
      (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
      (add-hook 'calendar-today-visible-hook 'calendar-mark-today))))

(defun japanese/init-pangu-spacing ()
  (use-package pangu-spacing
    :init
    (progn ;; replacing `chinese-two-byte' by `japanese'
      (setq pangu-spacing-chinese-before-english-regexp
            (rx (group-n 1 (category japanese))
                (group-n 2 (in "a-zA-Z0-9"))))
      (setq pangu-spacing-chinese-after-english-regexp
            (rx (group-n 1 (in "a-zA-Z0-9"))
                (group-n 2 (category japanese))))
      (spacemacs|hide-lighter pangu-spacing-mode)
      ;; Always insert `real' space in text-mode including org-mode.
      (setq pangu-spacing-real-insert-separtor t)
      ;; (global-pangu-spacing-mode 1)
      (add-hook 'text-mode-hook 'pangu-spacing-mode))))

(defun japanese/post-init-org ()
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Japanese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents))))
