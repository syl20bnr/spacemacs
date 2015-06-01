;;; packages.el --- Go Layer packages File for Spacemacs
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

;; variables

(spacemacs|defvar-company-backends go-mode)

(defvar spacemacs-go-run-current-buffer-other-window nil)
(defvar spacemacs-go-godoc-port 6060)

(when (fboundp 'eww)
  (add-to-list 'browse-url-browser-function
               `(,(format "http://localhost:%d/pkg" spacemacs-go-godoc-port) . ,#'(lambda (filename unknown)
                                                                                    (eww filename)))))
