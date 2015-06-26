;;; core-evilify-keymap-utest.el --- Spacemacs Unit Test File
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
(require 'mocker)
(require 'core-evilify-keymap)

;; ---------------------------------------------------------------------------
;; spacemacs//evilify-next-event
;; ---------------------------------------------------------------------------

;; TO UPDATE

;; (ert-deftest test-evilify-choose-rebind-event--h-to-H ()
;;   (let* ((event ?h)
;;          (map (let ((keymap (make-sparse-keymap)))
;;                 (define-key keymap "h" 'ignore)
;;                 keymap))
;;          (result (spacemacs//evilify-next-event map event)))
;;     (should (equal ?H result))))

;; (ert-deftest test-evilify-choose-rebind-event--h-to-C-h ()
;;   (let* ((event ?h)
;;          (map (let ((keymap (make-sparse-keymap)))
;;                 (define-key keymap "h" 'ignore)
;;                 (define-key keymap "H" 'ignore)
;;                 keymap))
;;          (result (spacemacs//evilify-next-event map event)))
;;     (should (equal (string-to-char "\C-h") result))))

;; (ert-deftest test-evilify-choose-rebind-event--h-to-C-H ()
;;   (let* ((event ?h)
;;          (map (let ((keymap (make-sparse-keymap)))
;;                 (define-key keymap "h" 'ignore)
;;                 (define-key keymap "H" 'ignore)
;;                 (define-key keymap (kbd "C-h") 'ignore)
;;                 keymap))
;;          (result (spacemacs//evilify-next-event map event)))
;;     (should (equal (aref (kbd "C-S-H") 0) result))))

;; (ert-deftest test-evilify-choose-rebind-event--h-to-nil ()
;;   (let* ((event ?h)
;;          (map (let ((keymap (make-sparse-keymap)))
;;                 (define-key keymap "h" 'ignore)
;;                 (define-key keymap "H" 'ignore)
;;                 (define-key keymap (kbd "C-h") 'ignore)
;;                 (define-key keymap (kbd "C-S-h") 'ignore)
;;                 keymap))
;;          (result (spacemacs//evilify-next-event map event)))
;;     (should (equal nil result))))
