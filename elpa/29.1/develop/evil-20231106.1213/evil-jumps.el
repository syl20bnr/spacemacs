;;; evil-jumps.el --- Jump list implementation -*- lexical-binding: t -*-

;; Author: Bailey Ling <bling at live.ca>

;; Version: 1.15.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'evil-core)
(require 'evil-states)

;;; Code:

(defgroup evil-jumps nil
  "Evil jump list configuration options."
  :prefix "evil-jumps"
  :group 'evil)

(defcustom evil-jumps-cross-buffers t
  "When non-nil, the jump commands can cross borders between buffers.
Otherwise the jump commands act only within the current buffer."
  :type 'boolean)

(defcustom evil-jumps-max-length 100
  "The maximum number of jumps to keep track of."
  :type 'integer)

(defcustom evil-jumps-pre-jump-hook nil
  "Hooks to run just before jumping to a location in the jump list."
  :type 'hook)

(defcustom evil-jumps-post-jump-hook nil
  "Hooks to run just after jumping to a location in the jump list."
  :type 'hook)

(defcustom evil-jumps-ignored-file-patterns '("COMMIT_EDITMSG$" "TAGS$")
  "List of regexps to exclude file path from inclusion in the jump list."
  :type '(repeat string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar savehist-additional-variables)

(defvar evil--jumps-jumping nil)

(defvar evil--jumps-jumping-backward nil
  "Set by `evil--jump-backward', used and cleared in the
`post-command-hook' by `evil--jump-handle-buffer-crossing'")

(eval-when-compile (defvar evil--jumps-debug nil))

(defvar evil--jumps-buffer-targets "\\*\\(new\\|scratch\\)\\*"
  "Regexp to determine if buffer with `buffer-name' is a valid jump target.")

(defvar evil--jumps-window-jumps (make-hash-table)
  "Hashtable which stores all jumps on a per window basis.")

(defvar evil-jumps-history nil
  "History of `evil-mode' jumps that are persisted with `savehist'.")

(cl-defstruct evil-jumps-struct
  ring
  (idx -1)
  previous-pos)

;; Is inlining this really worth it?
(defsubst evil--jumps-message (format &rest args)
  (when (eval-when-compile evil--jumps-debug)
    (with-current-buffer (get-buffer-create "*evil-jumps*")
       (goto-char (point-max))
       (insert (apply #'format format args) "\n"))))

(defun evil--jumps-get-current (&optional window)
  (unless window (setq window (selected-window)))
  (let ((jump-struct (gethash window evil--jumps-window-jumps)))
    (unless jump-struct
      (setq jump-struct (make-evil-jumps-struct))
      (puthash window jump-struct evil--jumps-window-jumps))
    jump-struct))

(defun evil--jumps-get-jumps (struct)
  (let ((ring (evil-jumps-struct-ring struct)))
    (unless ring
      (setq ring (make-ring evil-jumps-max-length))
      (setf (evil-jumps-struct-ring struct) ring))
    ring))

(defun evil--jumps-get-window-jump-list ()
  (let ((struct (evil--jumps-get-current)))
    (evil--jumps-get-jumps struct)))

(defun evil--jumps-savehist-load ()
  (add-to-list 'savehist-additional-variables 'evil-jumps-history)
  (let ((ring (make-ring evil-jumps-max-length)))
    (cl-loop for jump in (reverse evil-jumps-history)
             do (ring-insert ring jump))
    (setf (evil-jumps-struct-ring (evil--jumps-get-current)) ring))
  (add-hook 'savehist-save-hook #'evil--jumps-savehist-sync)
  (remove-hook 'savehist-mode-hook #'evil--jumps-savehist-load))

(defun evil--jumps-savehist-sync ()
  "Update the printable value of window jumps for `savehist'."
  (setq evil-jumps-history
        (delq nil (mapcar #'(lambda (jump)
                              (let* ((mark (car jump))
                                     (pos (if (markerp mark)
                                              (marker-position mark)
                                            mark))
                                     (file-name (cadr jump)))
                                (when (and (not (file-remote-p file-name))
                                           (file-exists-p file-name)
                                           pos)
                                  (list pos file-name))))
                          (ring-elements (evil--jumps-get-window-jump-list))))))

(defun evil--jumps-current-file-name ()
  "Get the current buffer file name for `evil--jumps-push'."
  (or buffer-file-name
      (when (derived-mode-p 'dired-mode) default-directory)))

(defun evil--jumps-jump (idx shift)
  (let ((target-list (evil--jumps-get-window-jump-list)))
    (evil--jumps-message "jumping from %s by %s" idx shift)
    (evil--jumps-message "target list = %s" target-list)
    (setq idx (+ idx shift))
    (let ((current-file-name (or (evil--jumps-current-file-name) (buffer-name)))
          (size (ring-length target-list)))
      (unless evil-jumps-cross-buffers
        ;; skip jump marks pointing to other buffers
        (while (and (< idx size) (>= idx 0)
                    (not (string= current-file-name (cadr (ring-ref target-list idx)))))
          (setq idx (+ idx shift))))
      (when (and (< idx size) (>= idx 0))
        ;; actual jump
        (run-hooks 'evil-jumps-pre-jump-hook)
        (let* ((place (ring-ref target-list idx))
               (pos (car place))
               (file-name (cadr place)))
          (setq evil--jumps-jumping t)
          (unless (string= current-file-name file-name)
            (if (string-match-p evil--jumps-buffer-targets file-name)
                (switch-to-buffer file-name)
              (find-file file-name)))
          (setq evil--jumps-jumping nil)
          (goto-char pos)
          (setf (evil-jumps-struct-idx (evil--jumps-get-current)) idx)
          (run-hooks 'evil-jumps-post-jump-hook))))))

(defun evil--jumps-push ()
  "Push the current cursor/file position to the jump list."
  (let ((target-list (evil--jumps-get-window-jump-list))
        (file-name (evil--jumps-current-file-name))
        (buffer-name (buffer-name))
        (current-pos (point-marker))
        (first-pos nil)
        (first-file-name nil)
        (excluded nil))
    (when (and (not file-name)
               (string-match-p evil--jumps-buffer-targets buffer-name))
      (setq file-name buffer-name))
    (when file-name
      (dolist (pattern evil-jumps-ignored-file-patterns)
        (when (string-match-p pattern file-name)
          (setq excluded t)))
      (unless excluded
        (unless (ring-empty-p target-list)
          (setq first-pos (car (ring-ref target-list 0)))
          (setq first-file-name (car (cdr (ring-ref target-list 0)))))
        (unless (and (equal first-pos current-pos)
                     (equal first-file-name file-name))
          (evil--jumps-message "pushing %s on %s" current-pos file-name)
          (ring-insert target-list `(,current-pos ,file-name)))))
    (evil--jumps-message "%s %s"
                         (selected-window)
                         (and (not (ring-empty-p target-list))
                              (ring-ref target-list 0)))))

(evil-define-command evil-show-jumps ()
  "Display the contents of the jump list."
  :repeat nil
  (evil-with-view-list
    :name "evil-jumps"
    :mode "Evil Jump List"
    :format [("Jump" 5 nil)
             ("Marker" 8 nil)
             ("File/text" 1000 t)]
    :entries (let* ((jumps (evil--jumps-savehist-sync))
                    (count 0))
               (cl-loop for jump in jumps
                        collect `(nil [,(number-to-string (cl-incf count))
                                       ,(number-to-string (car jump))
                                       (,(cadr jump))])))
    :select-action #'evil--show-jumps-select-action))

(defun evil--show-jumps-select-action (jump)
  (let ((position (string-to-number (elt jump 1)))
        (file (car (elt jump 2))))
    (kill-buffer)
    (switch-to-buffer (find-file file))
    (goto-char position)))

(defun evil-set-jump (&optional pos)
  "Set jump point at POS.
POS defaults to point."
  (save-excursion
    (when (markerp pos)
      (set-buffer (marker-buffer pos)))

    (unless (or (region-active-p) (evil-visual-state-p))
      (push-mark pos t))

    (unless evil--jumps-jumping
      ;; clear out intermediary jumps when a new one is set
      (let* ((struct (evil--jumps-get-current))
             (target-list (evil--jumps-get-jumps struct))
             (idx (evil-jumps-struct-idx struct)))
        (cl-loop repeat idx
                 do (ring-remove target-list))
        (setf (evil-jumps-struct-idx struct) -1))
      (when pos
        (goto-char pos))
      (evil--jumps-push))))
(put 'evil-set-jump 'permanent-local-hook t)

(defun evil--jump-backward (count)
  (setq evil--jumps-jumping-backward t)
  (let ((count (or count 1)))
    (evil-motion-loop (nil count)
      (let* ((struct (evil--jumps-get-current))
             (idx (evil-jumps-struct-idx struct)))
        (evil--jumps-message "jumping back %s" idx)
        (when (= idx -1)
          (setq idx 0)
          (setf (evil-jumps-struct-idx struct) 0)
          (evil--jumps-push))
        (evil--jumps-jump idx 1)))))

(defun evil--jump-forward (count)
  (let ((count (or count 1)))
    (evil-motion-loop (nil count)
      (let* ((struct (evil--jumps-get-current))
             (idx (evil-jumps-struct-idx struct)))
        (when (= idx -1)
          (setq idx 0)
          (setf (evil-jumps-struct-idx struct) 0)
          (evil--jumps-push))
        (evil--jumps-jump idx -1)))))

(defun evil--jumps-window-configuration-hook (&rest _args)
  (let* ((window-list (window-list-1 nil nil t))
         (existing-window (selected-window))
         (new-window (previous-window)))
    (when (and (not (eq existing-window new-window))
               (> (length window-list) 1))
      (let* ((target-jump-struct (evil--jumps-get-current new-window)))
        (if (not (ring-empty-p (evil--jumps-get-jumps target-jump-struct)))
            (evil--jumps-message "target window %s already has %s jumps" new-window
                                 (ring-length (evil--jumps-get-jumps target-jump-struct)))
          (evil--jumps-message "new target window detected; copying %s to %s" existing-window new-window)
          (let* ((source-jump-struct (evil--jumps-get-current existing-window))
                 (source-list (evil--jumps-get-jumps source-jump-struct)))
            (when (= (ring-length (evil--jumps-get-jumps target-jump-struct)) 0)
              (setf (evil-jumps-struct-previous-pos target-jump-struct) (evil-jumps-struct-previous-pos source-jump-struct))
              (setf (evil-jumps-struct-idx target-jump-struct) (evil-jumps-struct-idx source-jump-struct))
              (setf (evil-jumps-struct-ring target-jump-struct) (ring-copy source-list)))))))
    ;; delete obsolete windows
    (maphash (lambda (key _val)
               (unless (member key window-list)
                 (evil--jumps-message "removing %s" key)
                 (remhash key evil--jumps-window-jumps)))
             evil--jumps-window-jumps)))
(put 'evil--jumps-window-configuration-hook 'permanent-local-hook t)

(defun evil--jump-hook (&optional command)
  "`pre-command-hook' for evil-jumps.
Set jump point if COMMAND has a non-nil `:jump' property. Otherwise,
save the current position in case the command being executed will
change the current buffer."
  (setq command (or command this-command))
  (if (evil-get-command-property command :jump)
      (evil-set-jump)
    (setf (evil-jumps-struct-previous-pos (evil--jumps-get-current))
          (point-marker))))
(put 'evil--jump-hook 'permanent-local-hook t)

(defun evil--jump-handle-buffer-crossing ()
  (let ((jumping-backward evil--jumps-jumping-backward))
    (setq evil--jumps-jumping-backward nil)
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (let* ((struct (evil--jumps-get-current window))
               (previous-pos (evil-jumps-struct-previous-pos struct)))
          (when previous-pos
            (setf (evil-jumps-struct-previous-pos struct) nil)
            (if (and
                 ;; `evil-jump-backward' (and other backward jumping
                 ;; commands) needs to be handled specially. When
                 ;; jumping backward multiple times, calling
                 ;; `evil-set-jump' is always wrong: If you jump back
                 ;; twice and we call `evil-set-jump' after the second
                 ;; time, we clear the forward jump list and
                 ;; `evil--jump-forward' won't work.

                 ;; The first time you jump backward, setting a jump
                 ;; point is sometimes correct. But we don't do it
                 ;; here because this function is called after
                 ;; `evil--jump-backward' has updated our position in
                 ;; the jump list so, again, `evil-set-jump' would
                 ;; break `evil--jump-forward'.
                 (not jumping-backward)
                 (let ((previous-buffer (marker-buffer previous-pos)))
                   (and previous-buffer
                        (not (eq previous-buffer (window-buffer window))))))
                (evil-set-jump previous-pos)
              (set-marker previous-pos nil))))))))
(put 'evil--jump-handle-buffer-crossing 'permanent-local-hook t)

(if (bound-and-true-p savehist-loaded)
    (evil--jumps-savehist-load)
  (add-hook 'savehist-mode-hook #'evil--jumps-savehist-load))

(defun evil--jumps-install-or-uninstall ()
  (if evil-local-mode
      (progn
        (add-hook 'pre-command-hook #'evil--jump-hook nil t)
        (add-hook 'post-command-hook #'evil--jump-handle-buffer-crossing nil t)
        (add-hook 'next-error-hook #'evil-set-jump nil t)
        (add-hook 'window-configuration-change-hook #'evil--jumps-window-configuration-hook nil t))
    (remove-hook 'pre-command-hook #'evil--jump-hook t)
    (remove-hook 'post-command-hook #'evil--jump-handle-buffer-crossing t)
    (remove-hook 'next-error-hook #'evil-set-jump t)
    (remove-hook 'window-configuration-change-hook #'evil--jumps-window-configuration-hook t)))

(add-hook 'evil-local-mode-hook #'evil--jumps-install-or-uninstall)

(provide 'evil-jumps)

;;; evil-jumps.el ends here
