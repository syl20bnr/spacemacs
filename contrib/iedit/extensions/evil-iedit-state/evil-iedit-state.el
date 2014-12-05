(require 'evil)
(require 'iedit)

(evil-define-state iedit
  "`iedit state' interfacing iedit mode."
  :tag " <E> "
  :cursor box
  :suppress-keymap t
  :input-method nil
  ;; force iedit mode
  (if (evil-replace-state-p) (call-interactively 'iedit-mode)))

(evil-define-state iedit-insert
  "Replace insert state in `iedit state'."
  :tag " <Ei> "
  :enable (insert)
  :cursor (bar . 2))

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

(defun evil-iedit-state/replace ()
  "Wipe all the occurrences and switch in `iedit-insert state'"
  (interactive)
  (iedit-delete-occurrences)
  (evil-iedit-insert-state))

(defun evil-iedit-state/evil-change ()
  "Wipe all the occurrences and switch in `iedit-insert state'"
  (interactive)
  (call-interactively 'evil-change)
  (evil-iedit-state)  ; required to correctly update the cursors
  (evil-iedit-insert-state))

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
       (setq unread-command-events (listify-key-sequence "w")))

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

(define-key evil-iedit-state-map "#"  'iedit-number-occurrences)
(define-key evil-iedit-state-map "b"  'iedit-blank-occurrences)
(define-key evil-iedit-state-map "B"  'iedit-toggle-buffering)
(define-key evil-iedit-state-map "c"  'evil-iedit-state/evil-change)
(define-key evil-iedit-state-map "d"  'iedit-delete-occurrences)
(define-key evil-iedit-state-map "D"  'iedit-downcase-occurrences)
(define-key evil-iedit-state-map "f"  'iedit-restrict-function)
(define-key evil-iedit-state-map "gg" 'iedit-goto-first-occurrence)
(define-key evil-iedit-state-map "G"  'iedit-goto-last-occurrence)
(define-key evil-iedit-state-map "i"  'evil-iedit-insert-state)
(define-key evil-iedit-state-map "h"  'evil-backward-char)
(define-key evil-iedit-state-map "j"  'evil-next-visual-line)
(define-key evil-iedit-state-map "k"  'evil-previous-visual-line)
(define-key evil-iedit-state-map "l"  'evil-forward-char)
(define-key evil-iedit-state-map "I"  'iedit-toggle-case-sensitive)
(define-key evil-iedit-state-map "J"  'iedit-expand-down-a-line)
(define-key evil-iedit-state-map "K"  'iedit-expand-up-a-line)
(define-key evil-iedit-state-map "L"  'iedit-restrict-current-line)
(define-key evil-iedit-state-map "n"  'iedit-next-occurrence)
(define-key evil-iedit-state-map "N"  'iedit-prev-occurrence)
(define-key evil-iedit-state-map "p"  'evil-iedit-state/paste-replace)
(define-key evil-iedit-state-map "r"  'evil-iedit-state/replace)
(define-key evil-iedit-state-map "v"  'iedit-toggle-unmatched-lines-visible)
(define-key evil-iedit-state-map "u"  'undo-tree-undo)
(define-key evil-iedit-state-map "U"  'iedit-upcase-occurrences)
(define-key evil-iedit-state-map [escape] 'evil-iedit-state/quit-iedit-mode)
(define-key evil-iedit-insert-state-map [escape] 'evil-iedit-state)
(define-key evil-iedit-insert-state-map [(shift return)] 'evil-iedit-state/quit-iedit-mode)

(provide 'evil-iedit-state)
