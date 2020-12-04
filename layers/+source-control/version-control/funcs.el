;;; funcs.el --- Version control functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/diff-mode-revert-hunk ()
  (interactive)
  (diff-apply-hunk t))

(defun space-macs/vcs-next-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-next-hunk)
       (git-gutter  'git-gutter:next-hunk)
       (git-gutter+ 'git-gutter+-next-hunk)))))

(defun space-macs/vcs-previous-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-previous-hunk)
       (git-gutter  'git-gutter:previous-hunk)
       (git-gutter+ 'git-gutter+-previous-hunk)))))

(defun space-macs/vcs-revert-hunk ()
  (interactive)
  (let ((current-prefix-arg t)
        (inhibit-modification-hooks t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-revert-hunk)
       (git-gutter  'git-gutter:revert-hunk)
       (git-gutter+ 'git-gutter+-revert-hunks)))))

(defun space-macs/vcs-stage-hunk ()
  (interactive)
  (if (eq 'diff-hl version-control-diff-tool)
      (message "Staging not available")
    (let ((current-prefix-arg t))
      (call-interactively
       (cl-case version-control-diff-tool
         (git-gutter  'git-gutter:stage-hunk)
         (git-gutter+ 'git-gutter+-stage-hunks))))))

(defun space-macs/vcs-show-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-diff-goto-hunk)
       (git-gutter  'git-gutter:popup-hunk)
       (git-gutter+ 'git-gutter+-show-hunk-inline-at-point)))))

(defun space-macs/vcs-enable-margin ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun space-macs/vcs-disable-margin ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun space-macs/vcs-enable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun space-macs/vcs-disable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun space-macs/vcs-show-help ()
  (interactive)
  (setq version-control--ms-doc-toggle
        (logxor version-control--ms-doc-toggle 1)))

(defun space-macs/vcs-margin-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     diff-hl-mode)
    (git-gutter  (bound-and-true-p git-gutter-mode))
    (git-gutter+ (bound-and-true-p git-gutter+-mode))))

(defun space-macs/vcs-margin-global-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     global-diff-hl-mode)
    (git-gutter  global-git-gutter-mode)
    (git-gutter+ global-git-gutter+-mode)))

(space-macs|add-toggle version-control-margin
  :status (space-macs/vcs-margin-p)
  :on (space-macs/vcs-enable-margin)
  :off (space-macs/vcs-disable-margin)
  :documentation "Enable diff margins."
  :evil-leader "Td")

(space-macs|add-toggle version-control-margin-globally
  :status (space-macs/vcs-margin-global-p)
  :on (space-macs/vcs-enable-margin-globally)
  :off (space-macs/vcs-disable-margin-globally)
  :documentation "Enable diff margins globally."
  :evil-leader "T C-d")

(defun space-macs//smerge-ts-hint ()
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
   (if space-macs--smerge-ts-full-hint-toggle
       space-macs--smerge-ts-full-hint
     (concat "  (["
             (propertize "?" 'face 'hydra-face-red)
             "] help)"))))

(defun space-macs//smerge-ts-toggle-hint ()
  "Toggle the full hint docstring for the smerge transient state."
  (interactive)
  (setq space-macs--smerge-ts-full-hint-toggle
        (not space-macs--smerge-ts-full-hint-toggle)))

(defun space-macs//git-gutter+-refresh-in-all-buffers ()
  (git-gutter+-in-all-buffers (when git-gutter+-mode (git-gutter+-refresh))))


