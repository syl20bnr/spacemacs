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
;; spacemacs//evilify-entry-func
;; ---------------------------------------------------------------------------

;; full keymap

(ert-deftest test-evilify-keymap-entry-func--full-keymap ()
  (let ((map (make-keymap)))
    (should (eq 'spacemacs//evilify-char-table
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) nil)))))

;; command

(ert-deftest test-evilify-keymap-entry-func--command-s ()
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'func)
    (should (eq 'spacemacs//evilify-ascii-event-command-binding
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?s))))))

(ert-deftest test-evilify-keymap-entry-func--command-s-nil-evilified-keys ()
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'func)
    (should (eq 'spacemacs//evilify-ascii-event-command-binding
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) nil)))))

(ert-deftest test-evilify-keymap-entry-func--command-S ()
  (let ((map (make-sparse-keymap)))
    (define-key map "S" 'func)
    (should (eq 'spacemacs//evilify-ascii-event-command-binding
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?S))))))

(ert-deftest test-evilify-keymap-entry-func--command-C-s ()
  (let ((map (make-sparse-keymap))
        (ekeys (list (string-to-char "\C-s"))))
    (define-key map (kbd "C-s") 'func)
    (should (eq 'spacemacs//evilify-ascii-event-command-binding
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) ekeys)))))

(ert-deftest test-evilify-keymap-entry-func--command-C-S-s ()
  (let ((map (make-sparse-keymap))
        (ekeys (list (+ (expt 2 25) (string-to-char "\C-s")))))
    (define-key map (kbd "C-S-s") 'func)
    (should (eq 'spacemacs//evilify-shift-ascii-event
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) ekeys)))))

;; keymap

(ert-deftest test-evilify-keymap-entry-func--keymap-s ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap)))
    (define-key map "s" submap)
    (should (eq 'spacemacs//evilify-ascii-event-keymap-binding
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?s))))))

(ert-deftest test-evilify-keymap-entry-func--keymap-s-nil-evilified-keys ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap)))
    (define-key map "s" submap)
    (should (eq 'spacemacs//evilify-ascii-event-keymap-binding
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) nil)))))

(ert-deftest test-evilify-keymap-entry-func--keymap-S ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap)))
    (define-key map "S" submap)
    (should (eq 'spacemacs//evilify-ascii-event-keymap-binding
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?S))))))

(ert-deftest test-evilify-keymap-entry-func--keymap-C-s ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap))
        (ekeys (list (string-to-char "\C-s"))))
    (define-key map (kbd "C-s") submap)
    (should (eq 'spacemacs//evilify-ascii-event-keymap-binding
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) ekeys)))))

(ert-deftest test-evilify-keymap-entry-func--keymap-C-S-s ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap))
        (ekeys (list (+ (expt 2 25) (string-to-char "\C-s")))))
    (define-key map (kbd "C-S-s") submap)
    (should (eq 'spacemacs//evilify-shift-ascii-event
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) ekeys)))))

(ert-deftest test-evilify-keymap-entry-func--ignore-lambda-s ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap)))
    (define-key map "s" (lambda () (interactive) 'dummy))
    (should (eq 'ignore
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?s))))))

;; ignore lambdas

(ert-deftest test-evilify-keymap-entry-func--ignore-lambda-S ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap)))
    (define-key map "S" (lambda () (interactive) 'dummy))
    (should (eq 'ignore
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?S))))))

(ert-deftest test-evilify-keymap-entry-func--ignore-lambda-s-nil-evilified-keys ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap)))
    (define-key map "s" (lambda () (interactive) 'dummy))
    (should (eq 'ignore
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?a))))))

(ert-deftest test-evilify-keymap-entry-func--ignore-lambda-C-s ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap))
        (ekeys (list (string-to-char "\C-s"))))
    (define-key map (kbd "C-s") (lambda () (interactive) 'dummy))
    (should (eq 'ignore
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) ekeys)))))

(ert-deftest test-evilify-keymap-entry-func--ignore-lambda-C-S-s ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap))
        (ekeys (list (+ (expt 2 25) (string-to-char "\C-s")))))
    (define-key map (kbd "C-S-s") (lambda () (interactive) 'dummy))
    (should (eq 'spacemacs//evilify-shift-ascii-event
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) ekeys)))))

;; remap

(ert-deftest test-evilify-keymap-entry-func--ignore-remap ()
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'func)
    (define-key map [remap func] 'func2)
    (should (eq 'ignore
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?s))))))

;; ignore if not in passed evilified keys

(ert-deftest test-evilify-keymap-entry-func--ignore-command-s-not-evilified ()
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'func)
    (should (eq 'ignore
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?a))))))

(ert-deftest test-evilify-keymap-entry-func--ignore-keymap-s-not-evilified ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap)))
    (define-key map "s" submap)
    (should (eq 'ignore
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?a))))))

(ert-deftest test-evilify-keymap-entry-func--ignore-lambda-s-not-evilified ()
  (let ((map (make-sparse-keymap))
        (submap (make-sparse-keymap)))
    (define-key map "s" (lambda () (interactive) 'dummy))
    (should (eq 'ignore
                (spacemacs//evilify-entry-func (nth 0 (cdr map)) '(?a))))))

;; ---------------------------------------------------------------------------
;; spacemacs//evilify-ascii-event-command-binding
;; ---------------------------------------------------------------------------

(ert-deftest test-evilify-ascii-event-command-binding--s ()
  (let ((evil-evilified-state-map (let ((map (make-sparse-keymap)))
                                    (define-key map "s" 'evil-func)
                                    map))
        (map (make-sparse-keymap)))
    (define-key map "s" 'func)
    (spacemacs//evilify-ascii-event-command-binding (nth 0 (cdr map)) map)
    (should (equal '(keymap
                     (83 . (lambda () (interactive)
                             (call-interactively (quote func))))
                     (remap . (keymap (func . spacemacs/evilified-func))))
                   map))))

;; (ert-deftest test-evilify-ascii-event-command-binding--s-and-S ()
;;   (let ((evil-evilified-state-map (let ((map (make-sparse-keymap)))
;;                                     (define-key map "s" 'evil-func)
;;                                     map))
;;         (map (make-sparse-keymap)))
;;     (define-key map "s" 'func1)
;;     (define-key map "S" 'func2)
;;     (spacemacs//evilify-ascii-event-command-binding (nth 1 (cdr map)) map)
;;     (should (equal '(keymap
;;                      (83 . (lambda () (interactive)
;;                              (call-interactively (quote func))))
;;                      (remap . (keymap (func . spacemacs/evilified-func))))
;;                    map))))

;; ---------------------------------------------------------------------------
;; spacemacs/evilify-map
;; ---------------------------------------------------------------------------

;; (setq evil-evilified-state-map (let ((map (make-sparse-keymap)))
;;                                  (define-key map "s" 'evil-func)
;;                                  map))

;; (ert-deftest test-evilify-map--called-twice-with-command-s ()
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "s" 'func)
;;     (spacemacs/evilify-map map)
;;     (spacemacs/evilify-map map)
;;     (should (equal '(keymap
;;                      (83 . (lambda () (interactive)
;;                              (call-interactively (quote func))))
;;                      (remap . (keymap (func . spacemacs/evilified-func))))
;;                    map))))


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
