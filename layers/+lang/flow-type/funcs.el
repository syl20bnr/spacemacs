;;; funcs.el --- flow-type layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jonathan del Strother <jdelStrother@gmail.com>
;;         Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; flow-js2-mode

(defun flow-type/activate-flow-js2-mode ()
  (require 'flow-minor-mode)
  (require 'flow-js2-mode)
  (activate-flow-js2-mode))


;; flow-minor-mode

(defun flow-type/may-activate-flow-minor-mode ()
  (require 'flow-minor-mode)
  (flow-minor-enable-automatically))

(defun flow-type/enable-eldoc ()
  (require 'flow-minor-mode)
  (if (and flow-type-enable-eldoc-type-info (flow-minor-configured-p))
      (set (make-local-variable 'eldoc-documentation-function) #'flow-type/known-type-at-pos)))

(defun flow-type/known-type-at-pos ()
  (require 'flow-minor-mode)
  ;; You'll get '(unknown)' while cursoring over comments, whitespace, keywords, etc
  ;; Don't bother reporting type information for those instances:
  (let ((type (flow-minor-type-at-pos)))
    (if (not (string-match "^\\(flow is still initializing\\|(unknown)\\)" type))
        type)))
