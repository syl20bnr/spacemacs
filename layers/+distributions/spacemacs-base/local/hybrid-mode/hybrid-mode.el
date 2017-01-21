;;; hybrid-mode.el --- Put one foot in the church of Emacs

;; Copyright (C) 2012-2016 Sylvain Benner & Contributors
;;
;; Authors: Justin Burkett <justin@burkett.cc>
;;          Chris Ewald <chrisewald@gmail.com>
;; Keywords: convenience editing
;; Created: 12 Aug 2015
;; Version: 1.00
;; Package-Requires: ((emacs "24") (evil "1.0.9"))
;; URL: https://github.com/syl20bnr/spacemacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'evil)

(defvar hybrid-mode-enable-hjkl-bindings)

(defcustom hybrid-mode-default-state 'normal
  "Value of `evil-default-state' for hybrid-mode."
  :group 'spacemacs
  :type 'symbol)

(defcustom hybrid-mode-enable-hjkl-bindings nil
  "If non nil then packages configuration should enable hjkl navigation."
  :group 'spacemacs
  :type 'boolean)

(defcustom hybrid-mode-enable-evilified-state t
  "If non nil then evilified states is enabled in buffer supporting it."
  :group 'spacemacs
  :type 'boolean)

(defvar hybrid-mode-default-state-backup evil-default-state
  "Backup of `evil-default-state'.")

(defadvice evil-insert-state (around hybrid-insert-to-hybrid-state disable)
  "Forces Hybrid state."
  (evil-hybrid-state))

(defadvice evil-evilified-state (around hybrid-evilified-to-hybrid-state disable)
  "Forces Hybrid state."
  (if (equal -1 (ad-get-arg 0))
      ad-do-it
    (if hybrid-mode-enable-evilified-state
        ad-do-it
      ;; seems better to set the emacs state instead of hybrid for evilified
      ;; buffers
      (evil-emacs-state))))

;;;###autoload
(define-minor-mode hybrid-mode
  "Global minor mode to replace insert state by hybrid state."
  :global t
  :lighter " hybrid"
  :group 'spacemacs
  (if hybrid-mode
      (enable-hybrid-editing-style)
    (disable-hybrid-editing-style)))

(defun enable-hybrid-editing-style ()
  "Enable the hybrid editing style."
  (setq hybrid-mode-default-state-backup evil-default-state
        evil-default-state hybrid-mode-default-state)
  ;; replace evil states by `hybrid state'
  (ad-enable-advice 'evil-insert-state
                    'around 'hybrid-insert-to-hybrid-state)
  (ad-enable-advice 'evil-evilified-state
                    'around 'hybrid-evilified-to-hybrid-state)
  (ad-activate 'evil-insert-state)
  (ad-activate 'evil-evilified-state)
  ;; key bindings hooks for dynamic switching of editing styles
  (run-hook-with-args 'spacemacs-editing-style-hook 'hybrid)
  ;; initiate `hybrid state'
  (hybrid-mode//update-states-for-current-buffers 'hybrid))

(defun disable-hybrid-editing-style ()
  "Disable the hybrid editing style (reverting to 'vim style)."
  (setq evil-default-state hybrid-mode-default-state-backup)
  ;; restore evil states
  (ad-disable-advice 'evil-insert-state
                     'around 'hybrid-insert-to-hybrid-state)
  (ad-disable-advice 'evil-evilified-state
                     'around 'hybrid-evilified-to-hybrid-state)
  (ad-activate 'evil-insert-state)
  (ad-activate 'evil-evilified-state)
  ;; restore key bindings
  (run-hook-with-args 'spacemacs-editing-style-hook 'vim)
  ;; restore the states
  (hybrid-mode//update-states-for-current-buffers 'vim))

;; This code is from evil insert state definition, any change upstream
;; should be reflected here
;; see https://bitbucket.org/lyro/evil/src/a25b848c90c7942fe89d9ec283c6bb44fb7b3cf4/evil-states.el?fileviewer=file-view-default#evil-states.el-74
(evil-define-state hybrid
  "Hybrid state for hybrid mode."
  :tag " <H> "
  :cursor (bar . 2)
  :message "-- HYBRID --"
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-cleanup-insert-state evil-stop-track-last-insertion)
  :input-method t
  (cond
   ((evil-hybrid-state-p)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
    (add-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (setq evil-maybe-remove-spaces t)
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step)))
   (t
    (remove-hook 'post-command-hook #'evil-maybe-remove-spaces)
    (remove-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (evil-maybe-remove-spaces t)
    (setq evil-insert-repeat-info evil-repeat-info)
    (evil-set-marker ?^ nil t)
    (unless (eq evil-want-fine-undo t)
      (evil-end-undo-step))
    (when evil-move-cursor-back
      (when (or (evil-normal-state-p evil-next-state)
                (evil-motion-state-p evil-next-state))
        (evil-move-cursor-back))))))

(define-key evil-hybrid-state-map [escape] 'evil-normal-state)

;; Override stock evil function `evil-insert-state-p'
(defun evil-insert-state-p (&optional state)
  "Whether the current state is insert."
  (and evil-local-mode
       (memq (or state evil-state) '(insert hybrid))))

(defun hybrid-mode//update-states-for-current-buffers (style)
  "Update the active state in all current buffers given current STYLE."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond
       ((eq 'hybrid style)
        (if (memq major-mode evil-evilified-state-modes)
            (evil-evilified-state)
          (funcall (intern (format "evil-%S-state"
                                   hybrid-mode-default-state)))))
       ((and (eq 'vim style)
             (memq evil-state '(hybrid emacs)))
        (cond
         ((memq major-mode evil-evilified-state-modes) (evil-evilified-state))
         ((memq major-mode evil-motion-state-modes) (evil-motion-state))
         (t (evil-normal-state))))))))

(provide 'hybrid-mode)
