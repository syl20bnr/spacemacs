;;; funcs.el --- Version control functions File
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


(defun spacemacs/diff-mode-revert-hunk ()
  (interactive)
  (diff-apply-hunk t))

(defun spacemacs/vcs-next-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-next-hunk)
       (git-gutter  'git-gutter:next-hunk)))))

(defun spacemacs/vcs-previous-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-previous-hunk)
       (git-gutter  'git-gutter:previous-hunk)))))

(defun spacemacs/vcs-revert-hunk ()
  (interactive)
  (let ((current-prefix-arg t)
        (inhibit-modification-hooks t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-revert-hunk)
       (git-gutter  'git-gutter:revert-hunk)))))

(defun spacemacs/vcs-stage-hunk ()
  (interactive)
  (if (eq 'diff-hl version-control-diff-tool)
      (message "Staging not available")
    (let ((current-prefix-arg t))
      (call-interactively
       (cl-case version-control-diff-tool
         (git-gutter  'git-gutter:stage-hunk)
         (git-gutter+ 'git-gutter+-stage-hunks))))))

(defun spacemacs/vcs-show-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-diff-goto-hunk)
       (git-gutter  'git-gutter:popup-hunk)))))

(defun spacemacs/vcs-enable-margin ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)))))

(defun spacemacs/vcs-disable-margin ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)))))

(defun spacemacs/vcs-enable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)))))

(defun spacemacs/vcs-disable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)))))

(defun spacemacs/vcs-show-help ()
  (interactive)
  (setq version-control--ms-doc-toggle
        (logxor version-control--ms-doc-toggle 1)))

(defun spacemacs/vcs-margin-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     diff-hl-mode)
    (git-gutter  (bound-and-true-p git-gutter-mode))))

(defun spacemacs/vcs-margin-global-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     global-diff-hl-mode)
    (git-gutter  global-git-gutter-mode)))

(spacemacs|add-toggle version-control-margin
  :status (spacemacs/vcs-margin-p)
  :on (spacemacs/vcs-enable-margin)
  :off (spacemacs/vcs-disable-margin)
  :documentation "Enable diff margins."
  :evil-leader "Td")

(spacemacs|add-toggle version-control-margin-globally
  :status (spacemacs/vcs-margin-global-p)
  :on (spacemacs/vcs-enable-margin-globally)
  :off (spacemacs/vcs-disable-margin-globally)
  :documentation "Enable diff margins globally."
  :evil-leader "T C-d")

(defun spacemacs//smerge-ts-hint ()
  "Return a hint for the smerge transient state.
Return a string indicating the index of the current conflict and
the number of conflicts detected by `smerge-mode'."
  (concat
   (cl-loop for ol being the overlays
            with pos = (point)
            if (eq (overlay-get ol 'smerge) 'conflict)
            count ol into total
            and if (<= (overlay-start ol) pos)
            count ol into idx
            finally return (format "conflict [%d/%d]" idx total))
   (if spacemacs--smerge-ts-full-hint-toggle
       spacemacs--smerge-ts-full-hint
     (concat "  (["
             (propertize "?" 'face 'hydra-face-red)
             "] help)"))))

(defun spacemacs//smerge-ts-toggle-hint ()
  "Toggle the full hint docstring for the smerge transient state."
  (interactive)
  (setq spacemacs--smerge-ts-full-hint-toggle
        (not spacemacs--smerge-ts-full-hint-toggle)))
