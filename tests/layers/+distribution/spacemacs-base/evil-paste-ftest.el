;;; -ftest.el --- Spacemacs Functional Test File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Alberto Zaccagni <me@lazywithclass.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;; ---------------------------------------------------------------------------
;; evil-mc-paste-before
;; evil-mc-paste-after
;; ---------------------------------------------------------------------------

(require 'mocker)

;; FIXME I don't think this is that ok, should we `provide` this lib?
(load-file "../../../../layers/+spacemacs/spacemacs-evil/funcs.el")

(ert-deftest test-evil-mc-paste-before--copy-copy-paste-paste-cycle ()
  ;; copy something1
  ;; copy something2
  ;; paste
  ;; C-p should paste something1
  ;; C-n should paste something2

  (mocker-let
      ((spacemacs//paste-transient-state-p
        ()
        ((:output ())))

       (spacemacs/paste-transient-state/evil-paste-before
        ()
        ((:input-matcher (lambda () t) :output 42)))

       (insert-for-yank
        (string)
        ((:input-matcher (lambda (x) t) :output-generator (lambda (x) x)))))

    (setq kill-ring nil)
    (kill-new "1")
    (kill-new "2")
    (kill-new "3")
    (evil-paste-after)
    (evil-paste-pop)
    (evil-paste-pop)

    ))
