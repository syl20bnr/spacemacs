;;; packages.el --- epub layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Jeremy Dormitzer <jeremy.dormitzer@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst epub-packages '(nov))

(defun epub/init-nov ()
  (use-package nov
    :defer t
    :mode ("\\.epub\\'" . nov-mode)
    :config
    (evilified-state-evilify-map nov-mode-map
      :mode nov-mode
      :bindings
      (kbd "H") 'nov-previous-document
      (kbd "L") 'nov-next-document
      (kbd "[") 'nov-previous-document
      (kbd "]") 'nov-next-document
      (kbd "d") 'nov-scroll-up
      (kbd "u") 'nov-scroll-down
      (kbd "J") 'nov-scroll-up
      (kbd "K") 'nov-scroll-down
      (kbd "gm") 'nov-display-metadata
      (kbd "gr") 'nov-render-document
      (kbd "gt") 'nov-goto-toc
      (kbd "gv") 'nov-view-source
      (kbd "gV") 'nov-view-content-source)))
