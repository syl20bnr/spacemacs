;;; flyspell-correct-helm.el --- Correcting words with flyspell via helm interface -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016-2022 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Version: 0.6.1
;; Package-Requires: ((flyspell-correct "0.6.1") (helm "1.9.0") (emacs "24"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This package provides helm interface for flyspell-correct package.
;;
;; Points of interest are `flyspell-correct-wrapper',
;; `flyspell-correct-previous' and `flyspell-correct-next'.
;;
;; Example usage:
;;
;;   (require 'flyspell-correct-helm)
;;   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
;;
;; Or via use-package:
;;
;;   (use-package flyspell-correct-helm
;;     :bind ("C-M-;" . flyspell-correct-wrapper)
;;     :init
;;     (setq flyspell-correct-interface #'flyspell-correct-helm))
;;
;;; Code:
;;

;; Requires

(require 'flyspell-correct)
(require 'helm)

;; Interface implementation

(defun flyspell-correct-helm--always-match (_)
  "Return non-nil for any CANDIDATE."
  t)

(defun flyspell-correct-helm--option-candidates (word)
  "Return a set of options for the given WORD."
  (let ((opts (list (cons (format "Save \"%s\"" word)
                          (cons 'save word))
                    (cons (format "Accept (session) \"%s\"" word)
                          (cons 'session word))
                    (cons (format "Accept (buffer) \"%s\"" word)
                          (cons 'buffer word))
                    (cons (format "Skip \"%s\"" word)
                          (cons 'skip word))
                    (cons (format "Stop at \"%s\"" word)
                          (cons 'stop word)))))
    (unless (string= helm-pattern "")
      (setq opts
            (append opts
                    (list (cons (format "Save \"%s\"" helm-pattern)
                                (cons 'save helm-pattern))
                          (cons (format "Accept (session) \"%s\"" helm-pattern)
                                (cons 'session helm-pattern))
                          (cons (format "Accept (buffer) \"%s\"" helm-pattern)
                                (cons 'buffer helm-pattern))))))
    opts))

(defun flyspell-correct-helm (candidates word)
  "Run `helm' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (helm :sources (list (helm-build-sync-source
                           (format "Suggestions for \"%s\" in dictionary \"%s\""
                                   word (or ispell-local-dictionary
                                            ispell-dictionary
                                            "Default"))
                         :candidates candidates
                         :action 'identity
                         :candidate-number-limit 9999
                         :fuzzy-match t)
                       (helm-build-sync-source "Options"
                         :candidates (lambda ()
                                       (flyspell-correct-helm--option-candidates word))
                         :action 'identity
                         :candidate-number-limit 9999
                         :match 'flyspell-correct-helm--always-match
                         :volatile t))
        :buffer "*Helm Flyspell*"
        :prompt "Correction: "))

(setq flyspell-correct-interface #'flyspell-correct-helm)

(provide 'flyspell-correct-helm)

;;; flyspell-correct-helm.el ends here
