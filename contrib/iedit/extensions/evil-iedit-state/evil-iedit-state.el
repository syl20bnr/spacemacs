(require 'evil)
(require 'iedit)

(evil-define-state iedit
  "`iedit state' interfacing iedit mode."
  :tag " <E> "
  :enable (normal)
  :cursor box
  :message "-- IEDIT --"
  ;; force iedit mode
  (if (evil-replace-state-p) (call-interactively 'iedit-mode)))

(evil-define-state iedit-insert
  "Replace insert state in `iedit state'."
  :tag " <Ei> "
  :enable (insert)
  :cursor (bar . 2)
  :message "-- IEDIT-INSERT --")

(defun evil-iedit-state/iedit-mode (&optional arg)
  "Start `iedit-mode'."
  (interactive "P")
  (if (fboundp 'ahs-clear) (ahs-clear))
  (iedit-mode arg)
  (evil-iedit-state))

(defun evil-iedit-state/quit-iedit-mode ()
  "Quit iedit-mode and return to `normal state'."
  (interactive)
  (iedit-done)
  (evil-normal-state))

(defmacro evil-iedit-state||swith-to-insert-state-after-command (command &optional interactive)
  "Call COMMAND and switch to iedit-insert state.
If INTERACTIVE is non-nil then COMMAND is called interactively."
  `(progn
     (if ,interactive
         (call-interactively ',command)
       (funcall ',command))
     ;; required to correctly update the cursors
     (evil-iedit-state)
     (evil-iedit-insert-state)))

(defun evil-iedit-state//goto-overlay-start ()
  "Return the position of the start of the current overlay."
  (let ((overlay (iedit-find-current-occurrence-overlay)))
    (if overlay
        (goto-char (overlay-start overlay))
      (call-interactively 'evil-digit-argument-or-evil-beginning-of-line))))

(defun evil-iedit-state//goto-overlay-end ()
  "Return the position of the end of the current overlay."
  (let ((overlay (iedit-find-current-occurrence-overlay)))
    (if overlay
        (goto-char (overlay-end overlay))
      (call-interactively 'evil-end-of-line))))

(defun evil-iedit-state/evil-beginning-of-line (count)
  "Go to the beginning of the current overlay."
  (interactive "p")
  (evil-iedit-state//goto-overlay-start))

(defun evil-iedit-state/evil-end-of-line ()
  "Go to the beginning of the current overlay."
  (interactive)
  (evil-iedit-state//goto-overlay-end))

(defun evil-iedit-state/evil-append-line ()
  "Put the point at then end of current overlay and switch to
`iedit-insert state'."
  (interactive)
  (evil-iedit-state||swith-to-insert-state-after-command
   evil-iedit-state//goto-overlay-end))

(defun evil-iedit-state/evil-insert-line ()
  "Put the point at then end of current overlay and switch to
`iedit-insert state'."
  (interactive)
  (evil-iedit-state||swith-to-insert-state-after-command
   evil-iedit-state//goto-overlay-start))

(defun evil-iedit-state/substitute ()
  "Wipe all the occurrences and switch in `iedit-insert state'"
  (interactive)
  (iedit-delete-occurrences)
  (evil-iedit-insert-state))

(defun evil-iedit-state/evil-change ()
  "Wipe all the occurrences and switch in `iedit-insert state'"
  (interactive)
  (evil-iedit-state||swith-to-insert-state-after-command evil-change t))

(defun evil-iedit-state/evil-append ()
  "Append and switch to `iedit-insert state'"
  (interactive)
  (evil-iedit-state||swith-to-insert-state-after-command evil-append t))

(defun evil-iedit-state/evil-open-below ()
  "Insert new line below and switch to `iedit-insert state'"
  (interactive)
  (evil-iedit-state||swith-to-insert-state-after-command evil-open-below t))

(defun evil-iedit-state/evil-open-above ()
  "Insert new line above and switch to `iedit-insert state'"
  (interactive)
  (evil-iedit-state||swith-to-insert-state-after-command evil-open-above t))

(defun evil-iedit-state/evil-substitute ()
  "Append and switch to `iedit-insert state'"
  (interactive)
  (evil-iedit-state||swith-to-insert-state-after-command evil-substitute t))

(defun evil-iedit-state/paste-replace (count)
  "Replace the selection with the yanked text."
  (interactive "P")
  (iedit-delete-occurrences)
  (evil-paste-before count))

;; expand-region integration, add an "e" command
(eval-after-load 'expand-region
  '(progn
     (defun evil-iedit-state/iedit-mode-from-expand-region (&optional arg)
       "Start `iedit-mode'."
       (interactive "P")
       (evil-iedit-state/iedit-mode arg)
       ;; hack to leave expand-region temporary overlay map
       ;; we choose a letter that is not in `iedit state'
       (setq unread-command-events (listify-key-sequence "kj")))

     (defadvice er/prepare-for-more-expansions-internal
         (around iedit/prepare-for-more-expansions-internal activate)
       ad-do-it
       (let ((default-msg (car ad-return-value))
             (default-bindings (cdr ad-return-value)))
         (message "%s" default-bindings)
         (setq ad-return-value
               (cons (concat default-msg ", e to edit")
                     (add-to-list 'default-bindings
                                  '("e" evil-iedit-state/iedit-mode-from-expand-region))))))))

;; redefine iedit-done to prevent iedit from putting the occurrence on the
;; kill-ring for no useful reason.
(defun iedit-done ()
  "Exit Iedit mode.
Save the current occurrence string locally and globally.  Save
the initial string globally."
  (when iedit-buffering
      (iedit-stop-buffering))
  (setq iedit-last-occurrence-local (iedit-current-occurrence-string))
  (setq iedit-last-occurrence-global iedit-last-occurrence-local)
  (setq iedit-last-initial-string-global iedit-initial-string-local)
  ;; this is the hack
  ;; (if iedit-last-occurrence-local
  ;; (kill-new iedit-last-occurrence-local)) ; Make occurrence the latest kill in the kill ring.
  (setq iedit-num-lines-to-expand-up 0)
  (setq iedit-num-lines-to-expand-down 0)
  (iedit-cleanup)
  (setq iedit-initial-string-local nil)
  (setq iedit-mode nil)
  (force-mode-line-update)
  (remove-hook 'kbd-macro-termination-hook 'iedit-done t)
  (remove-hook 'change-major-mode-hook 'iedit-done t)
  (remove-hook 'iedit-aborting-hook 'iedit-done t)
  (run-hooks 'iedit-mode-end-hook))

(define-key evil-iedit-state-map "#"   'iedit-number-occurrences)
(define-key evil-iedit-state-map "$"   'evil-iedit-state/evil-end-of-line)
(evil-redirect-digit-argument evil-iedit-state-map "0" 'evil-iedit-state/evil-beginning-of-line)
(define-key evil-iedit-state-map "a"   'evil-iedit-state/evil-append)
(define-key evil-iedit-state-map "A"   'evil-iedit-state/evil-append-line)
(define-key evil-iedit-state-map "c"   'evil-iedit-state/evil-change)
(define-key evil-iedit-state-map "D"   'iedit-delete-occurrences)
(define-key evil-iedit-state-map "F"   'iedit-restrict-function)
(define-key evil-iedit-state-map "gg"  'iedit-goto-first-occurrence)
(define-key evil-iedit-state-map "G"   'iedit-goto-last-occurrence)
(define-key evil-iedit-state-map "i"   'evil-iedit-insert-state)
(define-key evil-iedit-state-map "I"   'evil-iedit-state/evil-insert-line)
(define-key evil-iedit-state-map "J"   'iedit-expand-down-a-line)
(define-key evil-iedit-state-map "K"   'iedit-expand-up-a-line)
(define-key evil-iedit-state-map "L"   'iedit-restrict-current-line)
(define-key evil-iedit-state-map "n"   'iedit-next-occurrence)
(define-key evil-iedit-state-map "N"   'iedit-prev-occurrence)
(define-key evil-iedit-state-map "o"   'evil-iedit-state/evil-open-below)
(define-key evil-iedit-state-map "O"   'evil-iedit-state/evil-open-above)
(define-key evil-iedit-state-map "p"   'evil-iedit-state/paste-replace)
(define-key evil-iedit-state-map "s"   'evil-iedit-state/evil-substitute)
(define-key evil-iedit-state-map "S"   'evil-iedit-state/substitute)
(define-key evil-iedit-state-map "V"   'iedit-toggle-unmatched-lines-visible)
(define-key evil-iedit-state-map "U"   'iedit-upcase-occurrences)
(define-key evil-iedit-state-map (kbd "C-U") 'iedit-downcase-occurrences)
(define-key evil-iedit-state-map (kbd "C-g")'evil-iedit-state/quit-iedit-mode)
(define-key evil-iedit-state-map [tab] 'iedit-toggle-selection)
(define-key evil-iedit-state-map [backspace] 'iedit-blank-occurrences)
(define-key evil-iedit-state-map [escape]    'evil-iedit-state/quit-iedit-mode)

(define-key evil-iedit-insert-state-map (kbd "C-g") 'evil-iedit-state/quit-iedit-mode)
(define-key evil-iedit-insert-state-map [escape]    'evil-iedit-state)

;; unbound iedit commands:
;; toggle buffering
;; toggle case sensitive

(provide 'evil-iedit-state)
