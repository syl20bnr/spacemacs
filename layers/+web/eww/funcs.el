;;; funcs.el --- EWW Layer funcs File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Colton Kopsa <coljamkop@gmail.com>
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

;;;; Misc
(defun spacemacs/eww-toggle-render-latex ()
  "Toggle rendering of LaTeX fraagments."
  (interactive)
  (call-interactively #'texfrag-mode)
  (when texfrag-mode
    (eww-reload)))


;;;; Transient State
(defvar spacemacs--eww-ts-use-evil nil
  "Transient State enable Evil key bindings")

(defvar spacemacs--eww-ts-use-writeroom nil
  "Transient State enable writeroot")

(defvar spacemacs--eww-ts-use-zoom-frm nil
  "Transient State enable zoom-fmr key bindings")

(defun spacemacs//eww-key-placeholder ()
  "Just a placeholder for a keybinding."
  (interactive)
  (message "The feature is not available/enabled."))

(defun spacemacs//eww-ts-toggle-hint ()
  "Toggle full hint docstring for EWW transient state."
  (interactive)
  (setq spacemacs--eww-ts-full-hint-toggle
        (not spacemacs--eww-ts-full-hint-toggle)))

(defun spacemacs//eww-ts-hint ()
  "Return a condensed/full hint for the eww transient state"
  (concat
   " "
   (if (not spacemacs--eww-ts-full-hint-toggle)
       (concat "[" (propertize "?" 'face 'hydra-face-red) "] help")
     (let ((result spacemacs--eww-ts-full-hint))
       (cl-loop for (flag . reg) in
                '((spacemacs--eww-ts-use-evil . "\\[[^\\[]+ scroll [^\\[]*")
                  (spacemacs--eww-ts-use-writeroom . "\\[[^\\[]+writeroom[^\\[]*")
                  (spacemacs--eww-ts-use-zoom-frm . "\\[[^\\[]+zoom-in[^\\[]*")
                  (spacemacs--eww-ts-use-zoom-frm . "\\[[^\\[]+zoom-out[^\\[]*")
                  (spacemacs--eww-ts-use-zoom-frm . "\\[[^\\[]+unzoom[^\\[]*"))
                do
                (when-let* (((not (symbol-value flag)))
                            ((string-match reg result))
                            (start (match-beginning 0))
                            (end (match-end 0)))
                  (setq result
                        (concat (substring result 0 start)
                                (make-string (- end start) ?\s)
                                (substring result end)))))
       result))))

(defun spacemacs//eww-setup-transient-state ()
  "Setup EWW transient state with toggleable help hint.

Beware: due to transient state's implementation details this
function must be called in the :init section of `use-package' or
full hint text will not show up!"
  (spacemacs|transient-state-format-hint eww
    spacemacs--eww-ts-full-hint
    "
[_?_] toggle help
 Navigation^^^^^^^^                Layout/Appearance^^            Zoom^^              List/view^^          Other^^
 ----------^^^^^^^^--------------- ---------^^------------------  -----------^^------ -------^^----------- -----^^-----------------------
  [_h_/_j_/_k_/_l_] scroll l/d/u/r [_v_] toggle visual-line-mode  [_+_] zoom-in       [_W_] list buffers   [_r_] reload page
  [_H_/_L_] prev/next eww-buff^^^^ [_w_] toggle writeroom-mode    [_-_] zoom-out      [_S_] list histories [_x_] view in external browser
  [_<_/_>_] history back/forw^^^^  [_c_] toggle colors            [_=_] unzoom        [_B_] list bookmarks [_d_] download
  [_[_/_]_] page previous/next^^^^ [_t_] toggle latex             ^^                  [_V_] view source    [_B_] add bookmark
  [_u_] page up^^^^^^              [_C_] cycle theme              ^^                  ^^                   [_q_] quit
  [_t_] top url^^^^^^")
  (spacemacs|define-transient-state eww
    :title "Eww Transient State"
    :hint-is-doc t
    :dynamic-hint (spacemacs//eww-ts-hint)
    :on-enter (setq which-key-inhibit t)
    :on-exit (setq which-key-inhibit nil)
    :evil-leader-for-mode (eww-mode . ".")
    :bindings
    ("?" spacemacs//eww-ts-toggle-hint)
    ;; Navigation
    ("j" spacemacs//eww-key-placeholder)
    ("k" spacemacs//eww-key-placeholder)
    ("h" spacemacs//eww-key-placeholder)
    ("l" spacemacs//eww-key-placeholder)
    ("<" eww-back-url)
    (">" eww-forward-url)
    ("[" eww-previous-url)
    ("]" eww-next-url)
    ("H" spacemacs/eww-jump-previous-buffer)
    ("L" spacemacs/eww-jump-next-buffer)
    ("u" eww-up-url)
    ("t" eww-top-url)
    ;; Layout/Appearance
    ("w" spacemacs//eww-key-placeholder)
    ("v" visual-line-mode)
    ("c" eww-toggle-colors)
    ("t" spacemacs/eww-toggle-render-latex)
    ("C" spacemacs/cycle-spacemacs-theme)
    ;; Zoom
    ("+" spacemacs//eww-key-placeholder)
    ("-" spacemacs//eww-key-placeholder)
    ("=" spacemacs//eww-key-placeholder)
    ;; Lit/view
    ("W" eww-list-buffers)
    ("S" eww-list-histories)
    ("B" eww-list-bookmarks)
    ("V" eww-view-source)
    ;; Other
    ("r" eww-reload)
    ("x" eww-browse-with-external-browser :exit t)
    ("d" eww-download)
    ("B" eww-add-bookmark)
    ("q" nil :exit t)))


;;;; EWW Buffers

(defun spacemacs//eww-get-buffers ()
  "Update and return the list of EWW buffers."
  (let ((current-buffers (buffer-list))
        (current-eww-buffers))
    ;; add new eww buffers to a temporary list
    (dolist (buffer current-buffers)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'eww-mode)
                   (not (memq buffer spacemacs--eww-buffers)))
          (push buffer current-eww-buffers))))
    (unless (or current-eww-buffers spacemacs--eww-buffers)
      (user-error "No EWW buffers"))
    ;; remove deleted buffers
    (dolist (buffer spacemacs--eww-buffers)
      (when (not (memq buffer current-buffers))
        (delq buffer spacemacs--eww-buffers)))
    ;; append new buffers
    (nconc spacemacs--eww-buffers current-eww-buffers)))

(defun spacemacs//eww-next-buffer ()
  "Return the next EWW buffer or cycle to the oldest one."
  (let* ((eww-buffers (spacemacs//eww-get-buffers))
         (eww-buffer-pos (seq-position eww-buffers (current-buffer))))
    (if (eql eww-buffer-pos (1- (length eww-buffers)))
        (car eww-buffers)
      (nth (1+ eww-buffer-pos) eww-buffers))))

(defun spacemacs//eww-previous-buffer ()
  "Return the previous EWW buffer or cycle to the latest one."
  (let* ((eww-buffers (spacemacs//eww-get-buffers))
         (eww-buffer-pos (seq-position eww-buffers (current-buffer))))
    (if (zerop eww-buffer-pos)
        (car (last eww-buffers))
      (nth (1- eww-buffer-pos) eww-buffers))))

(defun spacemacs/eww-jump-next-buffer ()
  "Open the next EWW buffer or cycle to the oldest one."
  (interactive)
  (pop-to-buffer-same-window (spacemacs//eww-next-buffer)))

(defun spacemacs/eww-jump-previous-buffer ()
  "Open the previous EWW buffer or cycle to the latest one."
  (interactive)
  (pop-to-buffer-same-window (spacemacs//eww-previous-buffer)))

;;; funcs.el ends here
