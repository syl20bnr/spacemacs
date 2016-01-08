;;; funcs.el --- Version control functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun version-control/next-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-next-hunk)
       (git-gutter  'git-gutter:next-hunk)
       (git-gutter+ 'git-gutter+-next-hunk)))))

(defun version-control/previous-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-previous-hunk)
       (git-gutter  'git-gutter:previous-hunk)
       (git-gutter+ 'git-gutter+-previous-hunk)))))

(defun version-control/revert-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-revert-hunk)
       (git-gutter  'git-gutter:revert-hunk)
       (git-gutter+ 'git-gutter+-revert-hunks)))))

(defun version-control/stage-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     (message "staging not available."))
       (git-gutter  'git-gutter:stage-hunk)
       (git-gutter+ 'git-gutter+-stage-hunks)))))

(defun version-control/show-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-diff-goto-hunk)
       (git-gutter  'git-gutter:popup-hunk)
       (git-gutter+ 'git-gutter+-show-hunk-inline-at-point)))))

(defun version-control/enable-margin ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun version-control/disable-margin ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun version-control/enable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun version-control/disable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun version-control/show-help ()
  (interactive)
  (setq version-control--ms-doc-toggle
        (logxor version-control--ms-doc-toggle 1)))

(defun version-control/margin-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     diff-hl-mode)
    (git-gutter  git-gutter-mode)
    (git-gutter+ git-gutter+-mode)))

(defun version-control/margin-global-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     global-diff-hl-mode)
    (git-gutter  global-git-gutter-mode)
    (git-gutter+ global-git-gutter+-mode)))

(spacemacs|add-toggle version-control-margin
  :status (version-control/margin-p)
  :on (version-control/enable-margin)
  :off (version-control/disable-margin)
  :documentation "Enable diff margins."
  :evil-leader "Td")

(spacemacs|add-toggle version-control-margin-globally
  :status (version-control/margin-global-p)
  :on (version-control/enable-margin-globally)
  :off (version-control/disable-margin-globally)
  :documentation "Enable diff margins globally."
  :evil-leader "T C-d")
