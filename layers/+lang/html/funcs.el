;;; funcs.el --- HTML Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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
    (join-line -1))
  (beginning-of-line))

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

(defun spacemacs//web-setup-transient-state ()
  (defvar spacemacs--web-ts-full-hint-toggle nil
    "Toggle the state of the web transient state documentation.")

  (defvar spacemacs--web-ts-full-hint nil
    "Display full web transient state documentation.")

  (defvar spacemacs--web-ts-minified-hint nil
    "Display minified web transient state documentation.")

  (defun spacemacs//web-ts-toggle-hint ()
    "Toggle the full hint docstring for the web transient state."
    (interactive)
    (setq spacemacs--web-ts-full-hint-toggle
          (not spacemacs--web-ts-full-hint-toggle)))

  (defun spacemacs//web-ts-hint ()
    "Return a condensed/full hint for the web transient state"
    (concat
     " "
     (if spacemacs--web-ts-full-hint-toggle
         spacemacs--web-ts-full-hint
       (concat "[" (propertize "?" 'face 'hydra-face-red) "] help"
               spacemacs--web-ts-minified-hint))))

  (spacemacs|transient-state-format-hint web
    spacemacs--web-ts-minified-hint "\n
Navigate: _j_ _k_ _J_ _K_ _h_ _l_ Element: _c_ _d_ _D_ _r_ _w_ Other: _p_")

  (spacemacs|transient-state-format-hint web
    spacemacs--web-ts-full-hint
    (format "\n[_?_] toggle help
Navigate^^^^                 Element^^                    Other
[_j_/_k_] next/prev element  [_c_] clone                  [_p_] xpath (display path)
[_J_/_K_] next/prev sibling  [_d_] vanish (keep content)  [_q_] quit
[_h_/_l_] parent/child       [_D_] kill (inkl. content)
^^^^                         [_r_] rename
^^^^                         [_w_] wrap"))

  (spacemacs|define-transient-state web
    :title "Web Transient State"
    :hint-is-doc t
    :dynamic-hint (spacemacs//web-ts-hint)
    :foreign-keys run
    :evil-leader-for-mode (web-mode . ".")
    :bindings
    ("?" spacemacs//web-ts-toggle-hint)
    ;; Navigate
    ("j"  web-mode-element-next)
    ("k"  web-mode-element-previous)
    ("J"  web-mode-element-sibling-next)
    ("gj" web-mode-element-sibling-next)
    ("K"  web-mode-element-sibling-previous)
    ("gk" web-mode-element-sibling-previous)
    ("h"  web-mode-element-parent)
    ("l"  web-mode-element-child)
    ;; Element
    ("c" web-mode-element-clone)
    ("d" web-mode-element-vanish)
    ("D" web-mode-element-kill)
    ("r" web-mode-element-rename)
    ("w" web-mode-element-wrap)
    ;; Other
    ("p" web-mode-dom-xpath)
    ("q" nil :exit t)
    ("<escape>" nil :exit t)))
