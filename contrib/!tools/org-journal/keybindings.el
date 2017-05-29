;;; keybindings.el --- org-journal Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Mattias Lundell & Contributors
;;
;; Author: Mattias Lundell <mattias@lundell.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(evil-leader/set-key
  "jj" 'org-journal-new-entry)

(evil-leader/set-key-for-mode 'org-journal-mode
  "mn" 'org-journal-open-next-entry
  "mp" 'org-journal-open-previous-entry)
