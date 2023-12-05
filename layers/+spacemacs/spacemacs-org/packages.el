;;; packages.el --- spacemacs-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
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


;;; Commentary:

;;; Code:

(defconst spacemacs-org-packages
  '(
    flyspell
    ;; default-org package does not exist, we invent this package name
    ;; to allow the `org' layer to own the `org' package instead of this
    ;; layer. So it is easier for users to steal the ownership of the
    ;; `org' package.
    (default-org-config :location built-in)
    org-superstar
    (space-doc :location (recipe :fetcher local))
    toc-org
    ))

(defun spacemacs-org/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'org-mode-hook))

(defun spacemacs-org/init-default-org-config ()
  (use-package org
    :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
    :defer t
    :init
    (setq org-startup-with-inline-images t
          org-src-fontify-natively t
          ;; this is consistent with the value of
          ;; `helm-org-headings-max-depth'.
          org-imenu-depth 8)
    :config
    (font-lock-add-keywords
     'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                  (1 font-lock-comment-face prepend)
                  (2 font-lock-function-name-face)
                  (3 font-lock-comment-face prepend))))
    ;; Open links and files with RET in normal state
    (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)))

(defun spacemacs-org/init-org-superstar ()
  (use-package org-superstar
    :defer t
    :init (add-hook 'org-mode-hook 'org-superstar-mode)))

(defun spacemacs-org/init-toc-org ()
  (use-package toc-org
    :defer t
    :init
    (setq toc-org-max-depth 10)
    (add-hook 'org-mode-hook 'toc-org-enable)))

(defun spacemacs-org/init-space-doc ()
  (add-hook 'org-mode-hook 'dotspacemacs//prettify-spacemacs-docs))

;;; packages.el ends here
