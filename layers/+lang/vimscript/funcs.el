;;; funcs.el --- vimscript Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//vimscript-backend ()
  "Returns selected backend."
  (if vimscript-backend
      vimscript-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-vimscript))))

(defun spacemacs//vimscript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//vimscript-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes vimrc-mode))))

(defun spacemacs//vimscript-setup-backend ()
  "Conditionally setup vimscript backend."
  (pcase (spacemacs//vimscript-backend)
    (`lsp (lsp))))
