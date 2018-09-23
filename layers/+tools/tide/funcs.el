;;; funcs.el --- Tide Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Ting Zhou <ztlevi1993@gmail.coom>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//tide-setup-tide ()
  "Setup tide backend."
  (progn
    (evilified-state-evilify tide-references-mode tide-references-mode-map
      (kbd "C-k") 'tide-find-previous-reference
      (kbd "C-j") 'tide-find-next-reference
      (kbd "C-l") 'tide-goto-reference)
    (tide-setup)
    (unless (tide-current-server)
      (tide-restart-server))
    (tide-hl-identifier-mode +1)))

(defun spacemacs//tide-setup-tide-company (&rest modes)
  "Setup tide auto-completion."
  (spacemacs|add-company-backends
    :backends company-tide
    :modes ,@modes
    :variables
    company-minimum-prefix-length 2
    company-tooltip-align-annotations t)
  (company-mode))

(defun spacemacs//tide-setup-tide-eldoc ()
  "Setup eldoc for tide."
  (eldoc-mode))

(defun spacemacs//tide-set-leader-keys-for-major-modes (&rest modes)
  "Set tide key bindings for major modes"
  (setq keybindingList '("Ee" tide-fix
                         "Ed" tide-add-tslint-disable-next-line
                         "gb" tide-jump-back
                         "gg" tide-jump-to-definition
                         "gt" spacemacs/typescript-jump-to-type-def
                         "gu" tide-references
                         "hh" tide-documentation-at-point
                         "rrs" tide-rename-symbol
                         "sr" tide-restart-server))
  (dolist (m modes)
    (setq tideList (cons m keybindingList))
    (apply 'spacemacs/set-leader-keys-for-major-mode tideList)))

(defun spacemacs//tide-project-root ()
  "Resolve to `projectile-project-root' if `tide-project-root' fails."
  (or tide-project-root
      (or (locate-dominating-file default-directory "tsconfig.json")
          (locate-dominating-file default-directory "jsconfig.json"))
      (projectile-project-root)
      "."))

(defun +javascript|cleanup-tide-processes ()
  "Clean up dangling tsserver processes if there are no more buffers with
`tide-mode' active that belong to that server's project."
  (when tide-mode
    (unless (cl-loop with project-name = (tide-project-name)
                     for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'tide-mode buf)
                             (with-current-buffer buf
                               (string= (tide-project-name) project-name)))
                     return buf)
      (kill-process (tide-current-server)))))

(defun kill-tide-hook ()
  (add-hook 'kill-buffer-hook #'+javascript|cleanup-tide-processes nil t))
