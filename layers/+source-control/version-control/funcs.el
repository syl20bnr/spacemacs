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

(defun spacemacs/vcs-next-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-next-hunk)
       (git-gutter  'git-gutter:next-hunk)
       (git-gutter+ 'git-gutter+-next-hunk)))))

(defun spacemacs/vcs-previous-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-previous-hunk)
       (git-gutter  'git-gutter:previous-hunk)
       (git-gutter+ 'git-gutter+-previous-hunk)))))

(defun spacemacs/vcs-revert-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-revert-hunk)
       (git-gutter  'git-gutter:revert-hunk)
       (git-gutter+ 'git-gutter+-revert-hunks)))))

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
       (git-gutter  'git-gutter:popup-hunk)
       (git-gutter+ 'git-gutter+-show-hunk-inline-at-point)))))

(defun spacemacs/vcs-enable-margin ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun spacemacs/vcs-disable-margin ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun spacemacs/vcs-enable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun spacemacs/vcs-disable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case version-control-diff-tool
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun spacemacs/vcs-show-help ()
  (interactive)
  (setq version-control--ms-doc-toggle
        (logxor version-control--ms-doc-toggle 1)))

(defun spacemacs/vcs-margin-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     diff-hl-mode)
    (git-gutter  (bound-and-true-p git-gutter-mode))
    (git-gutter+ (bound-and-true-p git-gutter+-mode))))

(defun spacemacs/vcs-margin-global-p ()
  (interactive)
  (cl-case version-control-diff-tool
    (diff-hl     global-diff-hl-mode)
    (git-gutter  global-git-gutter-mode)
    (git-gutter+ global-git-gutter+-mode)))

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
