;;; funcs.el --- HTML Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/emmet-expand ()
  (interactive)
  (unless (if (bound-and-true-p yas-minor-mode)
              (call-interactively 'emmet-expand-yas)
            (call-interactively 'emmet-expand-line))
    (indent-for-tab-command)))

(defun spacemacs/impatient-mode ()
  (interactive)
  (if (bound-and-true-p impatient-mode)
      (impatient-mode -1)
    (unless (process-status "httpd")
      (httpd-start))
    (impatient-mode)
    (when (string-match-p "\\.html\\'" (buffer-name))
      (imp-visit-buffer))))

(defun spacemacs/css-expand-statement ()
  "Expand CSS block"
  (interactive)
  (save-excursion
    (end-of-line)
    (search-backward "{")
    (forward-char 1)
    (while (or (eobp) (not (looking-at "}")))
      (let ((beg (point)))
        (newline)
        (search-forward ";")
        (indent-region beg (point))
        ))
    (newline)))

(defun spacemacs/css-contract-statement ()
  "Contract CSS block"
  (interactive)
  (end-of-line)
  (search-backward "{")
  (while (not (looking-at "}"))
    (join-line -1)))

(defun spacemacs//setup-lsp-for-web-mode-buffers ()
  "Start lsp-mode and configure for buffer."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//setup-lsp-for-html-buffer ()
  "If buffer extension is html then turn on lsp."
  (let ((buffer-extension (save-match-data
                             ;; regex to capture extension part from file.html or file.html<whaterver>
                             (when (string-match "\\.\\([^.<]*\\)<*[^.]*$" (buffer-name))
                               (match-string 1 (buffer-name))))))
    (when (string= buffer-extension "html")
      (spacemacs//setup-lsp-for-web-mode-buffers))))
