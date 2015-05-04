;;; funcs.el --- Auto-completion functions File
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

(spacemacs|add-toggle auto-completion
                      :status
                      (if (eq 'company auto-completion-front-end)
                          company-mode
                        auto-complete-mode)
                      :on
                      (progn
                        (if (eq 'company auto-completion-front-end)
                            (company-mode)
                          (auto-complete-mode))
                        (message "Enabled auto-completion (using %S)."
                                 auto-completion-front-end))
                      :off
                      (progn
                        (if (eq 'company auto-completion-front-end)
                            (company-mode -1)
                          (auto-complete-mode -1))
                        (message "Disabled auto-completion."))
                      :documentation "Activate auto-completion."
                      :evil-leader "ta")

;; auto-completion key bindings functions

(defun spacemacs//auto-completion-set-RET-key-behavior (package behavior)
  "Bind RET key appropriately for the given PACKAGE and BEHAVIOR."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete behavior)
        (define-key map [return] 'company-complete-selection)
        (define-key map (kbd "RET") 'company-complete-selection))
       (t
        (define-key map [return] 'nil)
        (define-key map (kbd "RET") 'nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun spacemacs//auto-completion-set-TAB-key-behavior (package behavior)
  "Bind TAB key appropriately for the given PACKAGE and BEHAVIOR."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete behavior)
        (define-key map (kbd "TAB") 'company-complete-selection)
        (define-key map (kbd "<tab>") 'company-complete-selection))
       ((eq 'cycle behavior)
        (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
        (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
        (define-key map (kbd "<S-tab>")
          'spacemacs//company-complete-common-or-cycle-backward)
        (define-key map (kbd "<backtab>")
          'spacemacs//company-complete-common-or-cycle-backward))
       (t
        (define-key map (kbd "TAB") nil)
        (define-key map (kbd "<tab>") nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun spacemacs//auto-completion-setup-key-sequence (package keys)
  "Setup the key sequence to complete current selection"
  (when keys
    (let ((first-key (elt keys 0))
          (second-key (elt keys 1)))
      (cond
       ((eq 'company package)
        (define-key company-active-map (kbd (char-to-string first-key))
          'spacemacs//auto-completion-key-sequence-start))
       (t (message "Not yet implemented for package %S" package)))
      (define-key evil-insert-state-map (kbd (char-to-string second-key))
        'spacemacs//auto-completion-key-sequence-end)
      (define-key evil-emacs-state-map (kbd (char-to-string second-key))
        'spacemacs//auto-completion-key-sequence-end))))

;; key sequence to complete selection

(defvar spacemacs--auto-completion-time nil)
(defvar spacemacs--auto-completion-complete-last-candidate nil)

(defun spacemacs//auto-completion-key-sequence-start ()
  "Initiate auto-completion sequence."
  (interactive)
  (self-insert-command 1)
  (setq spacemacs--auto-completion-complete-last-candidate
        (cond
         ((bound-and-true-p company-mode)
          (nth company-selection company-candidates))))
  (when spacemacs--auto-completion-complete-last-candidate
    (setq spacemacs--auto-completion-time (current-time))))

(defun spacemacs//auto-completion-key-sequence-end ()
  "Check if the auto-completion key sequence has been entered."
  (interactive)
  (if (or (null spacemacs--auto-completion-time)
          (< 0.1 (float-time (time-since spacemacs--auto-completion-time))))
      (self-insert-command 1)
    ;; if the auto-completion menu is still active then we don't need to delete
    ;; the last inserted first key of the sequence
    (cond
     ((bound-and-true-p company-mode)
      (unless company-candidates
         (delete-char -1))
      (let ((company-idle-delay))
        (company-auto-begin)
        (company-finish spacemacs--auto-completion-complete-last-candidate)))))
  (setq spacemacs--auto-completion-time nil))
