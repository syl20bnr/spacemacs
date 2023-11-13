;;; window-purpose-fixes.el --- fix integration issues with other features -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021 Bar Magal & contributors

;; Author: Bar Magal
;; Package: purpose

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; This files contains fixes that allow Purpose to work well with other
;; features, or to allow other features to work well with Purpose.

;;; Code:

(require 'window-purpose-switch)
(require 'window-purpose-configuration)

(defvar purpose-fix-togglers-hook nil
  "List of functions that toggle fixes.
Each function in the list is responsible to toggle a fix. Each
function should take no argument, and enable its fix if
`purpose-mode' is non-nil, and disable it if `purpose-mode' is
nil. Functions should be added to this variable via `add-hook'.")

;; variable `purpose-mode' may not be defined yet, so declare it here (without a
;; value) to satisfy the byte-compiler.
(defvar purpose-mode)

(defun purpose-fix-toggle-advice (symbol where function)
  "Add advice or remove advice, depending on the value of `purpose-mode'.
If `purpose-mode' is active, then add FUNCTION to SYMBOL
according to WHERE, otherwise remove FUNCTION from SYMBOL. Refer
to `advice-add' and `advice-remove' for further details of
SYMBOL, WHERE and FUNCTION."
  (if purpose-mode
      (advice-add symbol where function)
    (advice-remove symbol function)))

(defmacro purpose-fix-install-advice-toggler (symbol where function)
  "Creates an advice toggler and optionally toggle on the advice.
SYMBOL WHERE and FUNCTION have the same meaning as `advice-add'."
  (declare (debug (functionp symbolp function-form)))
  (let* ((symbol-name (cond ((symbolp symbol) symbol)
                            ((consp symbol) (cadr symbol))))
         (toggler-name (intern (format "purpose--fix-%s-advice-toggler"
                                       symbol-name))))
    `(progn
       (purpose-fix-toggle-advice ,symbol ,where ,function)
       (unless (fboundp #',toggler-name)
         (defun ,toggler-name ()
           (purpose-fix-toggle-advice ,symbol ,where ,function)))
       (add-hook 'purpose-fix-togglers-hook #',toggler-name))))

(defun purpose--fix-edebug ()
  "Integrates Edebug with Purpose."

  (with-eval-after-load 'edebug
    (defun purpose--edebug-pop-to-buffer-advice (buffer &optional window)
      "Reimplements `edebug-pop-to-buffer' using `pop-to-buffer'

Since `edebug-pop-to-buffer' simply splits the last selected
window before the minibuffer was popped up, the window it picks
to display a edebug buffer does not respect `window-purpose' as
all.  This advice reimplements it by replacing the window
spliting logic with `pop-to-buffer'."
      (setq window
            (cond
             ((and (edebug-window-live-p window)
                   (eq (window-buffer window) buffer))
              window)
             ((eq (window-buffer) buffer)
              (selected-window))
             ((get-buffer-window buffer 0))
             (t (get-buffer-window (pop-to-buffer buffer)))))
      (set-window-buffer window buffer)
      (select-window window)
      (unless (memq (framep (selected-frame)) '(nil t pc))
        (x-focus-frame (selected-frame)))
      (set-window-hscroll window 0))

    (purpose-fix-install-advice-toggler
     #'edebug-pop-to-buffer
     :override
     #'purpose--edebug-pop-to-buffer-advice)))

;;; `compilation-next-error-function' sometimes hides the compilation buffer
;;; when Purpose is on. Solution: make the buffer's window dedicated while
;;; executing `compilation-next-error-function'

(defun purpose--fix-compilation-next-error-function ()
  "Integrate Purpose and `compilation-next-error-function'."

  (with-eval-after-load 'compile
    (defun purpose--compilation-next-error-function (oldfun &rest args)
      "Prevents `compilation-next-error-function'from hiding the compilation buffer.

This is done by ensuring that the buffer is dedicated for the
duration of the function."

      (let* ((compilation-window (get-buffer-window (marker-buffer (point-marker))))
             (old-window-dedicated-p (window-dedicated-p compilation-window)))
        (if (not compilation-window)
            (apply oldfun args)
          (set-window-dedicated-p compilation-window t)
          (unwind-protect
              (apply oldfun args)
            (set-window-dedicated-p compilation-window old-window-dedicated-p)))))

    (purpose-fix-install-advice-toggler
     #'compilation-next-error-function
     :around
     #'purpose--compilation-next-error-function)))


(defun purpose--fix-isearch ()
  "Set `isearch--display-help-action'.

Prevents `isearch-describe-*' commands from bypassing purpose."
  (with-eval-after-load 'isearch
    (setq isearch--display-help-action '(purpose--action-function . nil))))


(defun purpose--fix-next-error ()
  "Integrate `window-purpose' and `next-error'.

Under `next-error-follow-minor-mode', `next-error-no-select' will
override window-purpose's `display-buffer-overriding-action'.
This will result in source buffers not displaying in the
purpose-dedicated window for source code in complex window
layouts.  This fix makes sure `next-error' works with
window-purpose."
  (defun purpose--next-error (oldfun &rest args)
    "Make sure `next-error' respects `purspose--action-function'."
    (interactive (lambda (spec) (advice-eval-interactive-spec spec)))
    (let ((display-buffer-overriding-action '(purpose--action-function . nil)))
      (apply oldfun args)))

  (purpose-fix-install-advice-toggler #'next-error :around #'purpose--next-error))


;;; Hydra's *LV* buffer should be ignored by Purpose
(defun purpose--fix-hydra-lv ()
  "Add hydra's LV buffer to Purpose's ignore list."
  (with-eval-after-load 'lv
    (add-to-list 'purpose-action-function-ignore-buffer-names "^ \\*LV\\*$")))



;;; Helm's buffers should be ignored, and they should have their own purpose
(defvar purpose--helm-conf
  (purpose-conf "helm"
                :regexp-purposes '(("^\\*Helm" . helm)
                                   ("^\\*helm" . helm)))
  "Purpose configuration for helm.")
(defun purpose--fix-helm ()
  "Fix issues with helm.
Add helm's buffers to Purposes's ignore list.
Install helm's purpose configuration."
  (with-eval-after-load 'helm
    (add-to-list 'purpose-action-function-ignore-buffer-names "^\\*Helm"))
  (with-eval-after-load 'helm
    (add-to-list 'purpose-action-function-ignore-buffer-names "^\\*helm"))
  (with-eval-after-load 'helm
    (purpose-set-extension-configuration :helm purpose--helm-conf)))



;;; Neotree handles display of its own buffer and of opening files from the
;;; neotree buffer in a way that doesn't work well with Purpose.
;;; We override how neotree displays its buffer.  When neotree tries to open a
;;; file in a fancy way, we let it.
;;; The integration issues fixed here:
;;; - calling `neotree' from a dedicated window opened neotree on the right,
;;;   with 2 or 3 horizontally-split windows showing the buffer from the
;;;   dedicated window (see https://github.com/jaypei/emacs-neotree/issues/124)
;;; - with one 'edit' window, one 'general' window, select the 'general' window,
;;;   then call `neotree'. from neotree window, open an 'edit' buffer in a new
;;;   split (press '-' or '|'). 'general' window is split in two, the new 'edit'
;;;   buffer is displayed in the 'edit' window instead of the old 'edit' buffer

(defun purpose--fix-create-neo-window ()
  "Create neotree window, with Purpose."
  (let* ((buffer (neo-global--get-buffer t))
         (window (display-buffer buffer)))
    (neo-window--init window buffer)
    (neo-global--attach)
    (neo-global--reset-width)
    window))

(defun purpose--fix-display-neotree (buffer alist)
  "Display neotree window, with Purpose."
  (let* ((first-window (frame-root-window))
         (side (or (and (eq neo-window-position 'left) 'left) 'right))
         (new-window (split-window first-window nil side)))
    (purpose-change-buffer buffer new-window 'window alist)
    new-window))

(defun purpose--fix-neotree-1 ()
  "Integrate neotree with Purpose.
Override the display and creation of the neotree window.
When opening files from the neotree window, use Purpose only when
necessary.
Note: Don't call this function before `neotree' is loaded."
  (defun purpose-fix-neotree-create-window-advice (oldfun &rest args)
    "Override `neo-global--create-window' with `purpose--fix-create-neo-window'.
When `purpose--active-p' is nil, call original `neo-global--create-window'."
    (if purpose--active-p
        (purpose--fix-create-neo-window)
      (apply oldfun args)))

  (defun purpose-fix-neotree-open-file-advice (oldfun full-path &optional arg)
    "When ARG is nil, make sure Purpose is off while executing `neo-open-file'."
    (if (and purpose--active-p (null arg))
        (find-file full-path)
      (without-purpose (funcall oldfun full-path arg))))

  ;; using purpose 'Neotree, because using 'neotree causes problems with
  ;; `purpose-special-action-sequences' ('neotree is also a function, so
  ;; `purpose--special-action-sequence' will try to call it)
  (purpose-set-extension-configuration
   :neotree
   (purpose-conf "Neotree" :name-purposes `((,neo-buffer-name . Neotree))))
  (add-to-list 'purpose-special-action-sequences
               '(Neotree purpose-display-reuse-window-buffer
                         purpose-display-reuse-window-purpose
                         purpose--fix-display-neotree))

  (purpose-fix-install-advice-toggler
   #'neo-global--create-window
   :around
   #'purpose-fix-neotree-create-window-advice)
  (purpose-fix-install-advice-toggler
   #'neo-open-file
   :around
   #'purpose-fix-neotree-open-file-advice))

(defun purpose--fix-neotree ()
  "Call `purpose--fix-neotree-1' after `neotree' is loaded."
  (with-eval-after-load 'neotree
    (purpose--fix-neotree-1)))



;;; `org-no-popups' suppresses `display-buffer-alist', so we should suppress
;;; purpose-mode as well. However, since it's a macro and evaluated during
;;; byte-compilation, we can't use advice for it. Instead, we wrap functions
;;; that use the macro.
;;; known functions: `org-get-location', `org-switch-to-buffer-other-window'
(defun purpose--fix-org-no-popups-1 ()
  "Make Purpose inactive during some functions that use `org-no-popups'.
Don't call this function before `org' is loaded."
  (defun purpose--fix-org-switch-to-buffer-other-window (oldfun &rest args)
    "Make Purpose inactive during `org-switch-to-buffer-other-window'."
    (without-purpose (apply oldfun args)))
  (defun purpose--fix-org-get-location (oldfun &rest args)
    "Make Purpose inactive during `org-get-location'."
    (without-purpose (apply oldfun args)))
  (defun purpose--fix-org-goto-location (oldfun &rest args)
    "Make Purpose inactive during `org-goto-location'."
    (without-purpose (apply oldfun args)))

  (purpose-fix-install-advice-toggler
   #'org-switch-to-buffer-other-window
   :around
   #'purpose--fix-org-switch-to-buffer-other-window)
  (purpose-fix-install-advice-toggler
   #'org-get-location
   :around
   #'purpose--fix-org-get-location)
  (purpose-fix-install-advice-toggler
   #'org-goto-location
   :around
   #'purpose--fix-org-goto-location))

(defun purpose--fix-org-no-popups ()
  "Call `purpose--fix-org-no-popups-1' after `org' is loaded."
  (with-eval-after-load 'org
    (purpose--fix-org-no-popups-1)))



;;; popwin uses `switch-to-buffer' when it replicates the window tree, so
;;; Purpose has to be deactivated during that time. this affects guide-key too
(defun purpose--fix-popwin-1 ()
  "Make Purpose inactive during `popwin:replicate-window-config'.
Don't call this function before `popwin' is loaded."
  (defun purpose--fix-popwin-replicate (oldfun &rest args)
    "Make Purpose inactive during `popwin:replicate-window-config'."
    (without-purpose (apply oldfun args)))

  (purpose-fix-install-advice-toggler
   #'popwin:replicate-window-config
   :around
   #'purpose--fix-popwin-replicate))

(defun purpose--fix-popwin ()
  "Call `purpose--fix-popwin-1' after `popwin' is loaded."
  (with-eval-after-load 'popwin
    (purpose--fix-popwin-1)))



;;; Use a seperate purpose for guide-key window (not 'general)
(defun purpose--fix-guide-key ()
  "Use a seperate purpose for guide-key window."
  (with-eval-after-load 'guide-key
    (purpose-set-extension-configuration
     :guide-key
     (purpose-conf
      "guide-key"
      :name-purposes `((,guide-key/guide-buffer-name . guide-key))))))



;;; Use a seperate purpose for which-key window (not 'general), don't interfere
;;; with how which-key opens a window/frame
(defun purpose--fix-which-key ()
  "Don't interfere with which-key, and use a seperate which-key purpose."
  (with-eval-after-load 'which-key
    (add-to-list 'purpose-action-function-ignore-buffer-names
                 (regexp-quote which-key-buffer-name))
    (purpose-set-extension-configuration
     :which-key
     (purpose-conf
      "which-key"
      :name-purposes `((,which-key-buffer-name . which-key))))))



;;; Let magit-popup use its own way of opening a help window (see https://github.com/syl20bnr/spacemacs/issues/9570)
(defun purpose--fix-magit-popup ()
  "Let magit-popup display help windows the way it wants."
  (with-eval-after-load 'magit-popup
    (defun purpose--fix-magit-popup-help (oldfun &rest args)
      "Make Purpose inactive during `magit-popup-describe-function'."
      (without-purpose (apply oldfun args)))
    (defun purpose--fix-magit-popup-help (oldfun &rest args)
      "Make Purpose inactive during `magit-popup-manpage'."
      (without-purpose (apply oldfun args)))

    (purpose-fix-install-advice-toggler
     #'magit-popup-describe-function
     :around
     #'purpose--fix-magit-popup-help)
    (purpose-fix-install-advice-toggler
     #'magit-popup-manpage
     :around
     #'purpose--fix-magit-popup-help)))



;;; Zone buffers should always open in the same window
(defun purpose--fix-zone ()
  "Zone buffers should always open in the same window."
  (with-eval-after-load 'zone
    (purpose-set-extension-configuration
     :zone
     (purpose-conf "zone" :name-purposes '(("*zone*" . Zone))))
    (add-to-list 'purpose-special-action-sequences
                 '(Zone display-buffer-same-window))))


(defun purpose--fix-whitespace ()
  "Integrate `window-purpose' with `whitespace'."
  (with-eval-after-load 'whitespace
    (defun purpose--whitespace-display-window-advice (buffer)
      "Stops `whitespace-display-window' from splitting and shrinking windows."
      (with-current-buffer buffer
        (special-mode)
        (goto-char (point-min)))
      (switch-to-buffer buffer))

    (purpose-fix-install-advice-toggler
     #'whitespace-display-window
     :override
     #'purpose--whitespace-display-window-advice)))


;;; install fixes

(defun purpose-fix-install (&rest exclude)
  "Install fixes for integrating Purpose with other features.
EXCLUDE is a list of integrations to skip.  Known members of EXCLUDE
are:
- 'edebug : don't integrate with edebug
- 'compilation-next-error-function : don't integrate with
  `compilation-next-error-function'.
- 'isearch : don't integrate with isearch
- 'next-error : don't integrate with `next-error'
- 'lv : don't integrate with lv (hydra)
- 'helm : don't integrate with helm
- 'neotree : don't integrate with neotree
- 'org : don't integrate with org
- 'popwin : don't integrate with popwin
- 'guide-key : don't integrate with guide-key
- 'which-key : don't integrate with which-key
- 'whitespace : don't integrate with whitespace"
  (interactive)
  (unless (member 'edebug exclude)
    (purpose--fix-edebug))
  (unless (member 'compilation-next-error-function exclude)
    (purpose--fix-compilation-next-error-function))
  (unless (member 'isearch exclude)
    (purpose--fix-isearch))
  (unless (member 'next-error exclude)
    (purpose--fix-next-error))
  (unless (member 'lv exclude)
    (purpose--fix-hydra-lv))
  (unless (member 'helm exclude)
    (purpose--fix-helm))
  (unless (member 'neotree exclude)
    (purpose--fix-neotree))
  (unless (member 'org exclude)
    (purpose--fix-org-no-popups))
  (unless (member 'popwin exclude)
    (purpose--fix-popwin))
  (unless (member 'guide-key exclude)
    (purpose--fix-guide-key))
  (unless (member 'which-key exclude)
    (purpose--fix-which-key))
  (unless (member 'magit-popup exclude)
    (purpose--fix-magit-popup))
  (unless (member 'zone exclude)
    (purpose--fix-zone))
  (unless (member 'whitespace exclude)
    (purpose--fix-whitespace)))

(provide 'window-purpose-fixes)
;;; window-purpose-fixes.el ends here
