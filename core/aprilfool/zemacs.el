;;; ze-macs.el --- Space-macs 2016 April Fools File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-buffer-name "*ze-macs*")
(setq space-macs-buffer-logo-title "[Z E M A C S]")
(setq space-macs-buffer-version-info "af-1.01")

(define-minor-mode ze-macs-buffer-mode
  "Ze-macs major mode for startup screen."
  :lighter "ToTheMAX"
  (if ze-macs-buffer-mode
      (progn
        (space-macs/set-leader-keys-for-major-mode 'space-macs-buffer-mode
          (kbd "aprilfool") 'next-next-NEXT-millennium)
        (ze-macs//insert-links))
    ;; restore the true one
    (ad-disable-advice 'configuration-layer/initialize
                       'before 'ze-macs/initialize)
    (ad-activate 'configuration-layer/initialize)
    (ad-disable-advice 'space-macs-buffer//inject-version
                       'around 'ze-macs/inject-version)
    (ad-activate 'space-macs-buffer//inject-version)
    (ad-disable-advice 'space-macs-buffer/insert-banner-and-buttons
                       'after 'ze-macs/insert-banner-and-buttons)
    (ad-activate 'space-macs-buffer/insert-banner-and-buttons)
    (load-file (concat space-macs-start-directory "core/core-space-macs-buffer.el"))
    (setq dotspace-macs-startup-banner 'official)
    (kill-buffer)
    (insert "
â”€â”€â”€â–ˆâ”€â”€â–ˆâ”€â–ˆâ–€â–€â–ˆâ”€â–ˆâ–€â–€â–ˆâ”€â–ˆâ–€â–€â–ˆâ”€â–ˆâ”€â”€â–ˆâ”€â”€â”€
â”€â”€â”€â–ˆâ–€â–€â–ˆâ”€â–ˆâ–„â–„â–ˆâ”€â–ˆâ–€â–€â–€â”€â–ˆâ–€â–€â–€â”€â–€â–€â–€â–ˆâ”€â”€â”€
â”€â”€â”€â–ˆâ”€â”€â–ˆâ”€â–ˆâ”€â”€â–ˆâ”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ–„â–„â–ˆâ”€â”€â”€
â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
â”€â”€â”€â–ˆâ–€â–€â–ˆâ”€â–ˆâ–€â–€â–ˆâ”€â–ˆâ–€â–€â–ˆâ”€â–€â–€â–ˆâ–€â–€â”€â–ˆâ”€â”€â”€â”€â”€
â”€â”€â”€â–ˆâ–„â–„â–ˆâ”€â–ˆâ–€â–€â–€â”€â–ˆâ–€â–€â–„â”€â”€â”€â–ˆâ”€â”€â”€â–ˆâ”€â”€â”€â”€â”€
â”€â”€â”€â–ˆâ”€â”€â–ˆâ”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â–ˆâ”€â–„â–„â–ˆâ–„â–„â”€â–ˆâ–„â–„â”€â”€â”€
â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
â”€â”€â–ˆâ–€â–€â–€â”€â–„â–€â–€â–€â–„â”€â–„â–€â–€â–€â–„â”€â–ˆâ”€â”€â”€â”€â–„â–€â–€â–€â”€â”€
â”€â”€â–ˆâ–€â–€â–€â”€â–ˆâ”€â”€â”€â–ˆâ”€â–ˆâ”€â”€â”€â–ˆâ”€â–ˆâ”€â”€â”€â”€â–€â–€â–€â–„â”€â”€
â”€â”€â–ˆâ”€â”€â”€â”€â–€â–„â–„â–„â–€â”€â–€â–„â–„â–„â–€â”€â–ˆâ–„â–„â–„â”€â–„â–„â–„â–€â”€â”€
â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–„â–„â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–„â–„â”€â”€â”€â”€â”€â”€â”€â”€â”€
â”€â”€â”€â”€â”€â”€â”€â–„â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–„â”€â”€â”€â”€â”€â”€
â”€â”€â”€â”€â–„â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–€â”€â”€â”€â”€â”€â”€â”€â”€â–€â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–„â”€â”€â”€
â”€â”€â–„â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–€â–ˆâ–ˆâ–ˆâ–ˆâ–„â”€
â–„â–ˆâ–ˆâ–ˆâ–ˆâ–€â”€â”€â”€â”€â–„â–ˆâ–ˆâ–„â”€â”€â”€â”€â–„â–ˆâ–ˆâ–„â”€â”€â”€â–€â–ˆâ–ˆâ–ˆâ–ˆ
â–ˆâ–ˆâ–€â”€â”€â”€â”€â”€â”€â”€â–ˆâ–ˆâ–ˆâ–ˆâ”€â”€â”€â”€â–ˆâ–ˆâ–ˆâ–ˆâ”€â”€â”€â”€â”€â–€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â”€â”€â”€â”€â”€â”€â–ˆâ–ˆâ–ˆâ–ˆâ”€â”€â”€â”€â–ˆâ–ˆâ–ˆâ–ˆâ”€â”€â”€â”€â”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â”€â”€â”€â”€â”€â”€â–€â–ˆâ–ˆâ–€â”€â”€â”€â”€â–€â–ˆâ–ˆâ–€â”€â”€â”€â”€â”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ–„â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–„â–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ–€â–ˆâ–ˆâ–„â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–„â–ˆâ–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ”€â”€â–€â–ˆâ–ˆâ–„â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–„â–„â–ˆâ”€â–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ”€â”€â”€â”€â–€â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–€â”€â”€â–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ”€â”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ”€â”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ–ˆâ–„â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â–„â–ˆâ–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ”€â”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ”€â”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â–ˆâ–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â”€â–ˆâ”€â”€â”€â–ˆâ–€â”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ”€â”€â”€â–€â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–€â”€â”€â”€â–ˆâ–ˆ
â–ˆâ–ˆâ–„â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–„â–ˆâ–ˆâ–ˆ
â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–„â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–„â–ˆâ–ˆâ–ˆâ–ˆâ–ˆ
â”€â–€â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–„â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–„â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–€â”€
â”€â”€â”€â”€â–€â–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–ˆâ–€â”€â”€â”€ ")
    (space-macs-buffer/goto-buffer)
    (space-macs-buffer//remove-existing-widget-if-exist)))

(defun next-next-NEXT-millennium ()
  "Time to rock on!"
  (interactive)
  (ze-macs-buffer-mode -1))

(defvar ze-macs--links '(("this one" "http://neovim.io")
                        ("link" "http://cdn.meme.am/instances/500x/67641307.jpg")
                        ("Atom" "http://atom.io/")
                        ("implementation tricks" "http://e-macshorrors.com/")))

(defun ze-macs//insert-links ()
  "Replace bracketed texts by their link counterparts."
  (with-current-buffer space-macs-buffer-name
    (save-excursion
      (dolist (l ze-macs--links)
        (re-search-backward (format "\\(\\[%s\\]\\)" (car l)) nil t)
        (make-text-button
         (match-beginning 1)
         (match-end 1)
         'type 'help-url
         'help-args (cdr l))))))

(defadvice configuration-layer/initialize (before ze-macs/initialize activate)
  (setq dotspace-macs-startup-banner "~/.e-macs.d/core/banners/img/ze-macs.png"))

(defadvice space-macs-buffer//inject-version
    (around ze-macs/inject-version activate)
  (let ((e-macs-version "99.9999999")
        (dotspace-macs-distribution "ze-macs")
        (space-macs-version "af-1.01"))
    ad-do-it))

(defadvice space-macs-buffer/insert-banner-and-buttons
    (after ze-macs/insert-banner-and-buttons activate)
  ;; always display the release note
  (space-macs-buffer//insert-release-note-widget
   (concat space-macs-release-notes-directory
           space-macs-buffer-version-info ".txt")))

(add-hook 'e-macs-startup-hook 'ze-macs-buffer-mode t)

(provide 'ze-macs)


