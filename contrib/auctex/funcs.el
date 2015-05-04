;;; funcs.el --- Auctex Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; functions
(defun auctex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command auctex-build-command 'TeX-master-file -1)))
    ;; (setq build-proc (TeX-command auctex-build-command 'TeX-master-file -1))
    ;; ;; Sometimes, TeX-command returns nil causing an error in set-process-sentinel
    ;; (when build-proc
    ;;   (set-process-sentinel build-proc 'auctex//build-sentinel))))

(defun auctex//build-sentinel (process event)
  (if (string= event "finished\n")
      (TeX-view)
    (message "Errors! Check with C-`")))

(defun auctex//autofill ()
  "Check whether the pointer is ucrrently inside on the
environments described in `auctex-nofill-env' and if so, inhibits
the automatic filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment auctex-nofill-env))))
    (when do-auto-fill
      (do-auto-fill))))

(defun auctex/auto-fill-mode ()
  "Toggle uato-fill-mode using the custom auto-fill function."
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'auctex//autofill))
