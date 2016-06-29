;;; config.el --- smart-tabs layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Curtis Mackie <curtis@mackie.ninja>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Variables:

(defvar smart-tabs-default-insinuations nil
  "A list of values that will be passed to `smart-tabs-insinuate'
on package load. Only languages supported by `smart-tabs-mode' by default
(i.e. c, c++, java, javascript, cperl, python, ruby, or nxml) should be
included here. Defaults to nil.")

(defvar smart-tabs-respect-evil-customizations nil
  "If nil, loading `smart-tabs-mode' will locally redefine some evil
customize values to make `evil-indent' work with smart tabs.
Set this to t if you want to always respect those values.")

;;; Code:

;;; Utility function that simply enables indent-tabs-mode
;;; This is used as an argument to add-hook
(defun smart-tabs/enable-tabs-mode ()
  (setq indent-tabs-mode t))

;;; Macro that combines adding new language support, insinuating it, and hooking
;;; indent-tabs-mode in one place
(defmacro smart-tabs-add-language-and-insinuate (lang mode-hook advice-list &rest body)
  `(progn
     (smart-tabs-add-language-support ,lang ,mode-hook ,advice-list ,@body)
     (smart-tabs-insinuate ',lang)
     (add-hook ',mode-hook 'smart-tabs/enable-tabs-mode)))

(defun smart-tabs/no-tabs-mode (orig-fun &rest args)
  (let ((indent-tabs-mode nil))
    (apply orig-fun args)))

;;; config.el ends here
