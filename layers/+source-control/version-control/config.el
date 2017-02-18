;;; config.el --- Version Control configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar version-control-global-margin t
  "If non-nil, will show diff margins globally.")

(defvar version-control-diff-tool 'git-gutter+
  "Options are `git-gutter', `git-gutter+', and `diff-hl' to show
version-control markers.")

(defvar version-control-diff-side 'right
  "Side on which to show version-control markers.
Options are `left' and `right'.")

;; unchanged face
(defface git-gutter+-unchanged
  '((t (:background "yellow")))
  "face for unchanged lines"
  :group 'git-gutter+)
(defface git-gutter:unchanged
  '((t (:background "yellow")))
  "face for unchanged lines"
  :group 'git-gutter+)

;; change face
(defface git-gutter+-modified
  '((t (:foreground "magenta" :weight bold)))
  "face for modified lines"
  :group 'git-gutter+)
(defface git-gutter:modified
  '((t (:foreground "magenta" :weight bold)))
  "face for modified lines"
  :group 'git-gutter+)
(defface diff-hl-change
  '((default :foreground "blue3")
    (((class color) (min-colors 88) (background light))
     :background "#ddddff")
    (((class color) (min-colors 88) (background dark))
     :background "#333355"))
  "Face used to highlight changed lines."
  :group 'diff-hl)

;; added face
(defface git-gutter+-added
  '((t (:foreground "green" :weight bold)))
  "face for added lines"
  :group 'git-gutter+)
(defface git-gutter:added
  '((t (:foreground "green" :weight bold)))
  "face for added lines"
  :group 'git-gutter+)
(defface diff-hl-insert
  '((default :inherit diff-added)
    (((class color)) :foreground "green4"))
  "Face used to highlight inserted lines."
  :group 'diff-hl)

;; deleted face
(defface git-gutter+-deleted
  '((t (:foreground "red" :weight bold)))
  "face for deleted lines"
  :group 'git-gutter+)
(defface git-gutter:deleted
  '((t (:foreground "red" :weight bold)))
  "face for deleted lines"
  :group 'git-gutter+)
(defface diff-hl-delete
  '((default :inherit diff-removed)
    (((class color)) :foreground "red3"))
  "Face used to highlight deleted lines."
  :group 'diff-hl)
