;;; flyspell-correct-ido.el --- Correcting words with flyspell via ido interface -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016-2022 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Version: 0.6.1
;; Package-Requires: ((flyspell-correct "0.6.1") (emacs "24.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This package provides ido interface for flyspell-correct package.
;;
;; Points of interest are `flyspell-correct-wrapper',
;; `flyspell-correct-previous' and `flyspell-correct-next'.
;;
;; Example usage:
;;
;;   (require 'flyspell-correct-ido)
;;   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
;;
;; Or via use-package:
;;
;;   (use-package flyspell-correct-ido
;;     :bind ("C-M-;" . flyspell-correct-wrapper)
;;     :init
;;     (setq flyspell-correct-interface #'flyspell-correct-ido))
;;
;;; Code:
;;

;; Requires

(require 'flyspell-correct)
(require 'ido)

;;;###autoload
(defun flyspell-correct-ido (candidates word)
  "Run `ido-completing-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (let ((completing-read-function
         (lambda (prompt collection &rest _)
           (ido-completing-read prompt (all-completions "" collection)
                                nil nil nil nil word))))
    (flyspell-correct-completing-read candidates word)))

(setq flyspell-correct-interface #'flyspell-correct-ido)

(provide 'flyspell-correct-ido)

;;; flyspell-correct-ido.el ends here
