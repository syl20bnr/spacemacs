;;; core-evilified-state-ftest.el --- Spacemacs Functional Test File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'evil-evilified-state)

;; ---------------------------------------------------------------------------
;; evilified-state-evilify-map
;; ---------------------------------------------------------------------------

;; commands

(ert-deftest test-evilify-map--s ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func)
    (evilified-state-evilify-map input-map)
    (message "%s" input-map)
    (should (equal '((115 . func)
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (83 . func)
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-2-evilified ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     (define-key evil-map "S" 'evil-func2)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 . func)
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (19 . func)
                      (83 . evil-func2)
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func1)
    (define-key input-map "S" 'func2)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 . func1)
                     (83 . func2)
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (19 . func2)
                      (83 . func1)
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-reversed-order ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "S" 'func2)
    (define-key input-map "s" 'func1)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 . func1)
                     (83 . func2)
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (19 . func2)
                      (83 . func1)
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-2-evilified ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func1)
                                     (define-key evil-map "S" 'evil-func2)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func1)
    (define-key input-map "S" 'func2)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 . func1)
                     (83 . func2)
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (33554451 . func2)
                      (19 . func1)
                      (83 . evil-func2)
                      (115 . evil-func1)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-2-evilified-reversed-order ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "S" 'evil-func2)
                                     (define-key evil-map "s" 'evil-func1)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "S" 'func2)
    (define-key input-map "s" 'func1)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 . func1)
                     (83 . func2)
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (33554451 . func2)
                      (19 . func1)
                      (83 . evil-func2)
                      (115 . evil-func1)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-C-s ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func1)
    (define-key input-map "S" 'func2)
    (define-key input-map (kbd "C-s") 'func3)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 . func1)
                     (83 . func2)
                     (19 . func3)
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (33554451 . func3)
                      (19 . func2)
                      (83 . func1)
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-C-s-shuffled ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func1)
    (define-key input-map (kbd "C-s") 'func3)
    (define-key input-map "S" 'func2)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 . func1)
                     (83 . func2)
                     (19 . func3)
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (33554451 . func3)
                      (19 . func2)
                      (83 . func1)
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-C-s-2-evilified ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func1)
                                     (define-key evil-map "S" 'evil-func2)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func1)
    (define-key input-map "S" 'func2)
    (define-key input-map (kbd "C-s") 'func3)
    (mocker-let
     ((message (msg &rest args)
               ((:record-cls 'mocker-stub-record
                             :output nil :occur 1))))
     (evilified-state-evilify-map input-map)
     (should (equal '((115 . func1)
                      (83 . func2)
                      (19 . func3)
                      (evilified-state
                       keymap "Auxiliary keymap for Evilified state"
                       (33554451 . func2)
                       (19 . func1)
                       (83 . evil-func2)
                       (115 . evil-func1)))
                    (evilified-state--sort-keymap input-map))))))

(ert-deftest test-evilify-map--s-C-s-S-2-evilified-shuffled ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func1)
                                     (define-key evil-map "S" 'evil-func2)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func1)
    (define-key input-map (kbd "C-s") 'func3)
    (define-key input-map "S" 'func2)
    (mocker-let
     ((message (msg &rest args)
               ((:record-cls 'mocker-stub-record
                             :output nil :occur 1))))
     (evilified-state-evilify-map input-map)
     (should (equal '((115 . func1)
                      (83 . func2)
                      (19 . func3)
                      (evilified-state
                       keymap "Auxiliary keymap for Evilified state"
                       (33554451 . func2)
                       (19 . func1)
                       (83 . evil-func2)
                       (115 . evil-func1)))
                    (evilified-state--sort-keymap input-map))))))

;; keymaps

(ert-deftest test-evilify-map--s-keymap ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap))
         (submap (make-sparse-keymap)))
    (define-key input-map "s" submap)
    (define-key submap "t" 'func)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 keymap (116 . func))
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (83 keymap (116 . func))
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-keymap-2-evilified ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func1)
                                     (define-key evil-map "S" 'evil-func2)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap))
         (submap (make-sparse-keymap)))
    (define-key input-map "s" submap)
    (define-key submap "t" 'func)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 keymap (116 . func))
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (19 keymap (116 . func))
                      (83 . evil-func2)
                      (115 . evil-func1)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-keymaps ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap))
         (submap (make-sparse-keymap)))
    (define-key input-map "s" submap)
    (define-key input-map "S" submap)
    (define-key submap "t" 'func)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 keymap (116 . func))
                     (83 keymap (116 . func))
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (19 keymap (116 . func))
                      (83 keymap (116 . func))
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-keymaps-2-evilified ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func1)
                                     (define-key evil-map "S" 'evil-func2)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap))
         (submap (make-sparse-keymap)))
    (define-key input-map "s" submap)
    (define-key input-map "S" submap)
    (define-key submap "t" 'func)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 keymap (116 . func))
                     (83 keymap (116 . func))
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (33554451 keymap (116 . func))
                      (19 keymap (116 . func))
                      (83 . evil-func2)
                      (115 . evil-func1)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-C-s-keymaps ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap))
         (submap (make-sparse-keymap)))
    (define-key input-map "s" submap)
    (define-key input-map "S" submap)
    (define-key input-map (kbd "C-s") submap)
    (define-key submap "t" 'func)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 keymap (116 . func))
                     (83 keymap (116 . func))
                     (19 keymap (116 . func))
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (33554451 keymap (116 . func))
                      (19 keymap (116 . func))
                      (83 keymap (116 . func))
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--s-S-C-s-keymaps-2-evilified ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func1)
                                     (define-key evil-map "S" 'evil-func2)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap))
         (submap (make-sparse-keymap)))
    (define-key input-map "s" submap)
    (define-key input-map "S" submap)
    (define-key input-map (kbd "C-s") submap)
    (define-key submap "t" 'func)
    (mocker-let
     ((message (msg &rest args)
               ((:record-cls 'mocker-stub-record
                             :output nil :occur 1))))
     (evilified-state-evilify-map input-map)
     (should (equal '((115 keymap (116 . func))
                      (83 keymap (116 . func))
                      (19 keymap (116 . func))
                      (evilified-state
                       keymap "Auxiliary keymap for Evilified state"
                       (33554451 keymap (116 . func))
                       (19 keymap (116 . func))
                       (83 . evil-func2)
                       (115 . evil-func1)))
                    (evilified-state--sort-keymap input-map))))))

;; ;; commands and keymaps

(ert-deftest test-evilify-map--s-command-and-keymap ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap))
         (submap (make-sparse-keymap)))
    (define-key input-map "s" 'func)
    (define-key input-map "S" submap)
    (define-key submap "t" 'func)
    (evilified-state-evilify-map input-map)
    (should (equal '((115 . func)
                     (83 keymap (116 . func))
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (19 keymap (116 . func))
                      (83 . func)
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

;; ;; idem-potency

(ert-deftest test-evilify-map--idem-potent ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap))
         (submap (make-sparse-keymap)))
    (define-key input-map "e" 'func1)
    (define-key input-map (kbd "C-c C-x") 'func3)
    (define-key input-map "s" 'func2)
    (define-key input-map "S" submap)
    (define-key submap "t" 'func)
    (dotimes (_ 10)
      (evilified-state-evilify-map input-map))
    (should (equal '((115 . func2)
                     (101 . func1)
                     (83 keymap (116 . func))
                     (3 keymap (24 . func3))
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (19 keymap (116 . func))
                      (83 . func2)
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

;; eval-after-load

(ert-deftest test-evilify-map--eval-after-load-already-loaded ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func)
    ;; pass a feature already loaded at the time of calling
    (evilified-state-evilify-map input-map :eval-after-load core-funcs)
    (should (equal '((115 . func)
                     (evilified-state
                      keymap "Auxiliary keymap for Evilified state"
                      (83 . func)
                      (115 . evil-func)))
                   (evilified-state--sort-keymap input-map)))))

(ert-deftest test-evilify-map--eval-after-load-not-loaded ()
  (let* ((evil-evilified-state-map (let ((evil-map (make-sparse-keymap)))
                                     (define-key evil-map "s" 'evil-func)
                                     evil-map))
         (evil-evilified-state-map-original (copy-keymap
                                             evil-evilified-state-map))
         (input-map (make-sparse-keymap)))
    (define-key input-map "s" 'func)
    (evilified-state-evilify-map input-map :eval-after-load dummy-feature)
    ;; unmodified keymap since `dummy-feature' is not loaded
    (should (equal '((115 . func))
                   (evilified-state--sort-keymap input-map)))))
