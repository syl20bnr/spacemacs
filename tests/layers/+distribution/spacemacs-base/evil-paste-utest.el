;;; -utest.el --- Spacemacs Unit Test File
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

(ert-deftest test-evil-mc-paste-before--error-on-kill-ring-empty ()
  (should-error(spacemacs/evil-mc-paste-before)))

(ert-deftest test-evil-mc-paste-after--error-on-kill-ring-empty ()
  (should-error(spacemacs/evil-mc-paste-after)))

;; I am not particularly happy with the following as they don't check that
;; the functionality is working, it just asserts on functions

(ert-deftest test-evil-mc-paste-before--disable-paste-transient-state ()
  (mocker-let
   ((spacemacs//paste-transient-state-p
     ()
     ((:output ())))
    (evil-paste-before
     (count &optional register handler)
     ((:input-matcher (lambda (x y z) t) :output 42))))
   (should (= (spacemacs/evil-mc-paste-before 1) 42))
   (should (equal this-command 'evil-paste-before))))

(ert-deftest test-evil-mc-paste-before--enable-paste-transient-state ()
  (mocker-let
   ((spacemacs//paste-transient-state-p
     ()
     ((:output t)))
    (spacemacs/paste-transient-state/evil-paste-before
     ()
     ((:record-cls 'mocker-stub-record))))
   (spacemacs/evil-mc-paste-before))
  (should (equal this-command 'evil-paste-before)))

(ert-deftest test-evil-mc-paste-after--disable-paste-transient-state ()
  (mocker-let
   ((spacemacs//paste-transient-state-p
     ()
     ((:output ())))
    (evil-paste-after
     (count &optional register handler)
     ((:input-matcher (lambda (x y z) t) :output 42))))
   (should (= (spacemacs/evil-mc-paste-after 1) 42))
   (should (equal this-command 'evil-paste-after))))

(ert-deftest test-evil-mc-paste-after--enable-paste-transient-state ()
  (mocker-let
   ((spacemacs//paste-transient-state-p
     ()
     ((:output t)))
    (spacemacs/paste-transient-state/evil-paste-after
      ()
      ((:record-cls 'mocker-stub-record))))
   (spacemacs/evil-mc-paste-after))
  (should (equal this-command 'evil-paste-after)))
