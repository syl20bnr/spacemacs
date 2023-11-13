;;; polymode-compat.el --- Various compatibility fixes for other packages -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2022  Free Software Foundation, Inc.
;; Version: 0.1
;; URL: https://github.com/polymode/polymode
;; Keywords: emacs
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode-core)
(require 'advice nil t)

(defgroup polymode-compat nil
  "Polymode compatibility settings."
  :group 'polymode)


;;; emacs 25 compat

(unless (fboundp 'assoc-delete-all)

  (defun assoc-delete-all (key alist &optional test)
    "Delete from ALIST all elements whose car is KEY.
Compare keys with TEST.  Defaults to `equal'.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
    (unless test (setq test #'equal))
    (while (and (consp (car alist))
	            (funcall test (caar alist) key))
      (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr))
	             (funcall test (caar tail-cdr) key))
	        (setcdr tail (cdr tail-cdr))
	      (setq tail tail-cdr))))
    alist)

  (defun assq-delete-all (key alist)
    "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
    (assoc-delete-all key alist #'eq)))



;;; Various Wrappers for Around Advice

(defvar *span* nil)

;; advice doesn't provide named symbols. So we need to define specialized
;; wrappers for some key functions (unfinished)
(defmacro pm-define-wrapp-protected (fun)
  "Declare protected function with the name fun--pm-wrapped.
Return new name (symbol). FUN is an unquoted name of a function."
  (let* ((fun-name (symbol-name fun))
         (new-fun (intern (format "%s--pm-wrapped" fun-name)))
         (new-doc (format "  Error Protected function created with `pm-define-protected-wrapp'.\n\n%s"
                          (or (documentation fun) ""))))
    `(progn
       (defun ,new-fun (&rest args)
         ,new-doc
         (condition-case err
             (apply ',fun args)
           (error (message "(%s %s): %s"
                           ,fun-name
                           (mapconcat (lambda (x) (format "%s" x)) args " ")
                           (error-message-string err)))))
       ',new-fun)))

(defun pm-apply-protected (fun args)
  (when fun
    (condition-case-unless-debug err
        (apply fun args)
      (error (message "(%s %s): %s %s"
                      (if (symbolp fun)
                          (symbol-name fun)
                        "anonymous")
                      (mapconcat (lambda (x) (format "%s" x)) args " ")
                      (error-message-string err)
                      ;; (or (and (symbolp fun) "")
                      ;;     (replace-regexp-in-string "\n" "" (format "[%s]" fun)))
                      "[M-x pm-debug-mode RET for more info]")
             nil))))

(defun pm-override-output-position (orig-fun &rest args)
  "Restrict returned value of ORIG-FUN to fall into the current span.
When this function is called from within `pm-map-over-spans' the
dynamic variable *span* has precedence over the span at point.
ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let ((range (or (pm-span-to-range *span*)
                       (pm-innermost-range)))
            (pos (pm-apply-protected orig-fun args)))
        (and pos
             (min (max pos (car range))
                  (cdr range))))
    (apply orig-fun args)))

(defun pm-override-output-cons (orig-fun &rest args)
  "Restrict returned (beg . end) of ORIG-FUN to fall into the current span.
When this function is called from within `pm-map-over-spans' the
dynamic variable *span* has precedence over span at point. This
will break badly if (point) is not inside expected range. ARGS
are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let ((range (or (pm-span-to-range *span*)
                       (pm-innermost-range)))
            (be (pm-apply-protected orig-fun args)))
        (let ((out (and be
                        (cons (and (car be)
                                   (min (max (car be) (car range))
                                        (cdr range)))
                              (and (cdr be)
                                   (max (min (cdr be) (cdr range))
                                        (car range)))))))
          out))
    (apply orig-fun args)))

(defun pm-narrowed-override-output-cons (orig-fun &rest args)
  "Restrict returned (beg . end) of ORIG-FUN to fall into the current span.
Run ORIG-FUN with buffer narrowed to span. When this function is
called from within `pm-map-over-spans' the dynamic variable
*span* has precedence over span at point. ARGS are passed to
ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let ((*span* (or *span* (pm-innermost-span))))
        (pm-with-narrowed-to-span *span*
          (apply #'pm-override-output-cons orig-fun args)))
    (apply orig-fun args)))

(defun pm-substitute-beg-end (orig-fun beg end &rest args)
  "Execute ORIG-FUN with first BEG and END arguments limited to current span.
When this function is called from within `pm-map-over-spans' the
dynamic variable *span* has precedence over span at point.
 ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let* ((pos (if (and (<= (point) end) (>=  (point) beg))
                      (point)
                    end))
             (range (or (pm-span-to-range *span*)
                        (pm-innermost-range pos)))
             (new-beg (max beg (car range)))
             (new-end (min end (cdr range))))
        (pm-apply-protected orig-fun (append (list new-beg new-end) args)))
    (apply orig-fun beg end args)))

(defun pm-execute-narrowed-to-span (orig-fun &rest args)
  "Execute ORIG-FUN narrowed to the current span.
When this function is called from within `pm-map-over-spans' the
dynamic variable *span* has precedence over span at point. ARGS
are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (pm-with-narrowed-to-span *span*
        (pm-apply-protected orig-fun args))
    (apply orig-fun args)))



;;; LSP (lsp-mode and eglot)
;;
;; Emacs modifications `after-change-functions' to LSP insertions
;; https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
;;
;; INSERT: (50 56 0) means insert 6 chars starting at pos 50
;;   {"range": {"start": {"line": 1, "character": 0},
;;              "end"  : {"line": 1, "character": 0}},
;;              "text": "insert"}
;;
;; DELETE: (50 50 6) means delete 6 chars starting at pos 50
;;   {"range": {"start": {"line": 1, "character": 0},
;;              "end"  : {"line": 1, "character": 6}},
;;              "text": ""}
;;
;; REPLACE: (50 60 6) means delete 6 chars starting at pos 50, and replace
;; them with 10 chars
;;   {"range": {"start": {"line": 1, "character": 0},
;;              "end" :  {"line": 1, "character": 6}},
;;              "text": "new-insert"}
;;
;; INSERT:
;;   before-change:(obeg,oend)=(50,50)
;;   after-change:(nbeg,nend,olen)=(50,56,0)
;;
;; DELETE:
;;   before-change:(obeg,oend)=(50,56)
;;   after-change:(nbeg,nend,len)=(50,50,6)
;;
;; REPLACE:
;;   before-change:(obeg,oend)=(50,56)
;;   lsp-on-change:(nbeg,nend,olen)=(50,60,6)

(defun pm--lsp-text-document-content-change-event (beg end len)
  "Make a TextDocumentContentChangeEvent body for BEG to END, of length LEN."
  (if (zerop len)
      ;; insertion
      (pm--lsp-change-event beg end (buffer-substring-no-properties beg end))
    (if (pm--lsp-simple-change-p beg len)
        (let ((end-pos pm--lsp-before-change-end-position)
              (text (buffer-substring-no-properties beg end)))
          ;; if beg == end deletion, otherwise replacement
          (pm--lsp-change-event beg end-pos text))
      (pm--lsp-full-change-event))))

(defvar-local pm--lsp-before-change-end-position nil)
(defun pm--lsp-position (pos)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (let ((char (if (eq pm/chunkmode (nth 3 (pm-innermost-span pos)))
                      (- (point) (line-beginning-position))
                    0)))
        (list :line (1- (line-number-at-pos pos))
              :character char)))))

(defun pm--lsp-change-event (beg end text)
  (list
   :range (list
           :start (if (listp beg) beg (pm--lsp-position beg))
           :end   (if (listp end) end (pm--lsp-position end)))
   :text text))

(defun pm--lsp-full-change-event ()
  (list :text (pm--lsp-text)))

(defun pm--lsp-text (&optional beg end)
  (save-restriction
    (widen)
    (setq beg (or beg (point-min)))
    (setq end (or end (point-max)))
    (let ((cmode major-mode)
          (end-eol (save-excursion (goto-char end)
                                   (point-at-eol)))
          line-acc acc)
      (pm-map-over-modes
       (lambda (sbeg send)
         (let ((beg1 (max sbeg beg))
               (end1 (min send end))
               (rem))
           (if (eq cmode major-mode)
               (progn
                 (when (eq sbeg beg1)
                   ;; first line of mode; use line-acc
                   (setq acc (append line-acc acc))
                   (setq line-acc nil))
                 ;; if cur-mode follows after end on same line, accumulate the
                 ;; last line but not the actual text
                 (when (< beg1 end)
                   (push (buffer-substring-no-properties beg1 end1) acc)))
             (goto-char beg1)
             (if (<= end1 (point-at-eol))
                 (when (< beg1 end1) ; don't accumulate on last line
                   (push (make-string (- end1 beg1) ? ) line-acc))
               (while (< (point-at-eol) end1)
                 (push "\n" acc)
                 (forward-line 1))
               (setq line-acc (list (make-string (- end1 (point)) ? )))))))
       beg end-eol)
      (apply #'concat (reverse acc)))))

;; We cannot compute original change location when modifications are complex
;; (aka multiple changes are combined). In those cases we send an entire
;; document.
(defun pm--lsp-simple-change-p (beg len)
  "Non-nil if the after change BEG and LEN match before change range."
  (let ((bcr (pm--prop-get :before-change-range)))
    (and (eq beg (car bcr))
         (eq len (- (cdr bcr) (car bcr))))))

;; advises
(defun polymode-lsp-buffer-content (orig-fun)
  (if (and polymode-mode pm/polymode)
      (pm--lsp-text)
    (funcall orig-fun)))

(defun polymode-lsp-change-event (orig-fun beg end len)
  (if (and polymode-mode pm/polymode)
      (pm--lsp-text-document-content-change-event beg end len)
    (funcall orig-fun beg end len)))

(defvar-local polymode-lsp-integration t)

(with-eval-after-load "lsp-mode"
  (when polymode-lsp-integration
    (add-to-list 'polymode-run-these-after-change-functions-in-other-buffers 'lsp-on-change)
    ;; (add-to-list 'polymode-run-these-before-change-functions-in-other-buffers 'lsp-before-change)
    ;; FIXME: add auto-save?
    (add-to-list 'polymode-run-these-before-save-functions-in-other-buffers 'lsp--before-save)
    (dolist (sym '(lsp-lens--after-save lsp-on-save))
      (add-to-list 'polymode-run-these-after-save-functions-in-other-buffers sym))
    ;; (add-to-list 'polymode-move-these-minor-modes-from-old-buffer 'lsp-headerline-breadcrumb-mode)
    (pm-around-advice 'lsp--buffer-content #'polymode-lsp-buffer-content)
    (pm-around-advice 'lsp--text-document-content-change-event #'polymode-lsp-change-event)))

;; (advice-remove 'lsp--buffer-content #'polymode-lsp-buffer-content)
;; (advice-remove 'lsp--text-document-content-change-event #'polymode-lsp-change-event)


;;; Flyspel
(defun pm--flyspel-dont-highlight-in-chunkmodes (beg end _poss)
  (or (car (get-text-property beg :pm-span))
      (car (get-text-property end :pm-span))))
(add-hook 'flyspell-incorrect-hook
          #'pm--flyspel-dont-highlight-in-chunkmodes nil t)

;;; C/C++/Java
(pm-around-advice 'c-before-context-fl-expand-region #'pm-override-output-cons)
;; (advice-remove 'c-before-context-fl-expand-region #'pm-override-output-cons)
(pm-around-advice 'c-state-semi-safe-place #'pm-override-output-position)
;; (advice-remove 'c-state-semi-safe-place #'pm-override-output-position)
;; c-font-lock-fontify-region calls it directly
;; (pm-around-advice 'font-lock-default-fontify-region #'pm-substitute-beg-end)
(pm-around-advice 'c-determine-limit #'pm-execute-narrowed-to-span)


;;; Python
(declare-function pm--first-line-indent "polymode-methods")
(defun pm--python-dont-indent-to-0 (fun)
  "Fix indent FUN not to cycle to 0 indentation."
  (if (and polymode-mode pm/type)
      (let ((last-command (unless (eq (pm--first-line-indent) (current-indentation))
                            last-command)))
        (funcall fun))
    (funcall fun)))

(pm-around-advice 'python-indent-line-function #'pm--python-dont-indent-to-0)


;;; Core Font Lock
(defvar font-lock-beg)
(defvar font-lock-end)
(defun pm-check-for-real-change-in-extend-multiline (fun)
  "Protect FUN from inf-looping at ‘point-max’.
FUN is `font-lock-extend-region-multiline'. Propagate only real
changes."
  ;; fixme: report this ASAP!
  (let ((obeg font-lock-beg)
        (oend font-lock-end)
        (change (funcall fun)))
    (and change
         (not (eq obeg font-lock-beg))
         (not (eq oend font-lock-end)))))

(pm-around-advice #'font-lock-extend-region-multiline
                  #'pm-check-for-real-change-in-extend-multiline)


;;; Editing

;; (pm-around-advice 'fill-paragraph #'pm-execute-narrowed-to-span)
;; (advice-remove 'fill-paragraph #'pm-execute-narrowed-to-span)

;; Synchronization of points does not work always as expected because some low
;; level functions move indirect buffers' points when operate in the base
;; buffer. See comment in `polymode-with-current-base-buffer'.

;; (defun polymode-with-save-excursion (orig-fun &rest args)
;;   "Execute ORIG-FUN surrounded with `save-excursion'.
;; This function is intended to be used in advises of functions
;; which modify the buffer in the background and thus trigger
;; `pm-switch-to-buffer' on next post-command hook in a wrong place.
;; ARGS are passed to ORIG-FUN."
;;   (if polymode-mode
;;       (save-excursion
;;         (apply orig-fun args))
;;     (apply orig-fun args)))
;;
;; `save-buffer` misbehaves because after each replacement modification hooks
;; are triggered and poly buffer is switched in unpredictable fashion (#93).
;; This happens because basic-save-buffer uses save-buffer but not
;; save-excursion. Thus if base and indirect buffer don't have same point, at
;; the end of the function inner buffer will have the point from the base
;; buffer. Can be reproduced with (add-hook 'before-save-hook
;; 'delete-trailing-whitespace nil t) in the base buffer.
;;
;; (pm-around-advice 'basic-save-buffer #'polymode-with-save-excursion)
;; (advice-remove 'basic-save-buffer #'polymode-with-save-excursion)

;; Query replace were probably misbehaving due to unsaved match data (#92). The
;; following is probably not necessary. (pm-around-advice 'perform-replace
;; 'pm-execute-inhibit-modification-hooks)

;; No longer needed. See comment at pm-switch-to-buffer.
;; (defun polymode-newline-remove-hook-in-orig-buffer (fn &rest args)
;;   "`newline' temporary sets `post-self-insert-hook' and removes it in wrong buffer.
;; This ARGS are passed to `newline'."
;;   (if polymode-mode
;;       (let* ((cbuf (current-buffer))
;;              (old-hook (buffer-local-value 'post-self-insert-hook cbuf)))
;;         (prog1 (apply fn args)
;;           (unless (eq cbuf (current-buffer))
;;             (unless (eq old-hook (buffer-local-value 'post-self-insert-hook cbuf))
;;               (with-current-buffer cbuf
;;                 (if old-hook
;;                     (setq post-self-insert-hook old-hook)
;;                   (kill-local-variable 'post-self-insert-hook)))))))
;;     (apply fn args)))

;; (pm-around-advice 'newline #'polymode-newline-remove-hook-in-orig-buffer)


;;; DESKTOP SAVE #194 #240

;; NB: desktop-save will not save indirect buffer.
;; For base buffer, if it's hidden as per #34, we will save it unhide by removing left whitespaces.

(defun polymode-fix-desktop-buffer-info (fn buffer)
  "Unhide poly-mode base buffer which is hidden as per #34.
This is done by modifying `uniquify-buffer-base-name' to `pm--core-buffer-name'."
  (with-current-buffer buffer
    (let ((out (funcall fn buffer)))
      (when (and polymode-mode
                 (not (buffer-base-buffer))
                 (not (car out)))
        (setf (car out) pm--core-buffer-name))
      out)))

(declare-function desktop-buffer-info "desktop")
(with-eval-after-load "desktop"
  (advice-add #'desktop-buffer-info :around #'polymode-fix-desktop-buffer-info))

(defun polymode-fix-desktop-save-buffer-p (_ bufname &rest _args)
  "Dont save polymode buffers which are indirect buffers."
  (with-current-buffer bufname
    (not (and polymode-mode
              (buffer-base-buffer)))))

(declare-function desktop-save-buffer-p "desktop")
(with-eval-after-load "desktop"
  (advice-add #'desktop-save-buffer-p :before-while #'polymode-fix-desktop-save-buffer-p))


;;; MATLAB #199
;; matlab-mode is an old non-standard mode which doesn't trigger
;; `after-change-major-mode-hook`. As a result polymode cannot detect that
;; font-lock-mode is on and sets the `poly-lock-allow-fontification` to nil.
;; Explicitly trigger font-lock as a workaround.
(add-hook 'matlab-mode-hook (lambda () (font-lock-mode t)))


;;; Undo Tree (#230)
;; Not clear why this fix works, or even why the problem occurs.
(declare-function make-undo-tree "undo-tree")
(defvar buffer-undo-tree)
(defun polymode-init-undo-tree-maybe ()
  (when (and (boundp 'undo-tree-mode)
             undo-tree-mode
             (null buffer-undo-tree))
    (setq buffer-undo-tree (make-undo-tree))))

(with-eval-after-load 'undo-tree
  (add-hook 'polymode-init-inner-hook #'polymode-init-undo-tree-maybe))


;;; EVIL
(declare-function evil-change-state "evil-core")
(defun polymode-switch-buffer-keep-evil-state-maybe (old-buffer new-buffer)
  (when (and (boundp 'evil-state)
             evil-state)
    (let ((old-state (buffer-local-value 'evil-state old-buffer))
          (new-state (buffer-local-value 'evil-state new-buffer)))
      (unless (eq old-state new-state)
        (with-current-buffer new-buffer
          (evil-change-state old-state))))))

(with-eval-after-load 'evil-core
  (add-hook 'polymode-after-switch-buffer-hook #'polymode-switch-buffer-keep-evil-state-maybe))


;;; HL line
(defvar hl-line-mode)
(defvar global-hl-line-mode)
(declare-function hl-line-unhighlight "hl-line")
(declare-function global-hl-line-unhighlight "hl-line")
(add-to-list 'polymode-move-these-minor-modes-from-old-buffer 'hl-line-mode)
(defun polymode-switch-buffer-hl-unhighlight (old-buffer _new-buffer)
  (with-current-buffer old-buffer
    ;; We are moving hl-line-mode already
    (when hl-line-mode
      (hl-line-unhighlight))
    (when global-hl-line-mode
      (global-hl-line-unhighlight))))
(with-eval-after-load 'hl-line
  (add-hook 'polymode-after-switch-buffer-hook #'polymode-switch-buffer-hl-unhighlight))


;;; YAS

(with-eval-after-load "yasnippet"
  (add-hook 'yas-before-expand-snippet-hook #'polymode-disable-post-command)
  (add-hook 'yas-after-exit-snippet-hook #'polymode-enable-post-command))

(provide 'polymode-compat)


;;; Multiple cursors

(defvar mc--executing-command-for-fake-cursor)
(defun polymode-disable-post-command-with-multiple-cursors (orig-fun &rest args)
  (unless mc--executing-command-for-fake-cursor
    (polymode-disable-post-command)
    (apply orig-fun args)
    (polymode-enable-post-command)))

(with-no-warnings
  (with-eval-after-load "multiple-cursors-core"
    (advice-add #'mc/execute-this-command-for-all-cursors :around
                #'polymode-disable-post-command-with-multiple-cursors)))

;;; polymode-compat.el ends here
