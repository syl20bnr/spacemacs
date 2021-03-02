;;; funcs.el --- fasd Layer packages File for Spacemacs
;;
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun ivy-search-from-action (x)
  (if (file-directory-p x)
      (spacemacs/counsel-search dotspacemacs-search-tools nil x)
    (message "Selected item is not a directory path")))
