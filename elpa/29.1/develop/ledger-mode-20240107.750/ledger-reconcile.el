;;; ledger-reconcile.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;; Reconcile mode


;;; Commentary:
;; Code to handle reconciling Ledger files with outside sources

;;; Code:

(require 'easymenu)
(require 'ledger-init)

(require 'ledger-xact)
(require 'ledger-occur)
(require 'ledger-commodities)
(require 'ledger-exec)
(require 'ledger-navigate)
(require 'ledger-state)
(declare-function ledger-insert-effective-date "ledger-mode" (&optional date))
(declare-function ledger-read-account-with-prompt "ledger-mode" (prompt))
(declare-function ledger-read-date "ledger-mode" (prompt))

(defvar ledger-buf nil)
(defvar ledger-bufs nil)
(defvar ledger-acct nil)
(defvar ledger-target nil)

(defgroup ledger-reconcile nil
  "Options for Ledger-mode reconciliation"
  :group 'ledger)

(define-obsolete-variable-alias
  'ledger-recon-buffer-name
  'ledger-reconcile-buffer-name
  "2023-12-15")

(defcustom ledger-reconcile-buffer-name "*Reconcile*"
  "Name to use for reconciliation buffer."
  :type 'string
  :group 'ledger-reconcile)

(defcustom ledger-narrow-on-reconcile t
  "If t, show only transactions matching the reconcile regex in the main buffer."
  :type 'boolean
  :group 'ledger-reconcile)

(defcustom ledger-buffer-tracks-reconcile-buffer t
  "If t, move point in the ledger buffer when it moves in the reconcile buffer.
When the cursor is moved to a new transaction in the reconcile
buffer then that transaction will be shown in its source buffer."
  :type 'boolean
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-force-window-bottom nil
  "If t, show the reconcile window below the register window and resize."
  :type 'boolean
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-toggle-to-pending t
  "If t, then toggle between uncleared and pending.
reconcile-finish will mark all pending posting cleared."
  :type 'boolean
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-default-date-format ledger-default-date-format
  "Date format for the reconcile buffer.
Default is `ledger-default-date-format'."
  :type 'string
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-target-prompt-string "Target amount for reconciliation "
  "Prompt for reconcile target."
  :type 'string
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-buffer-header "Reconciling account %s\n\n"
  "Default header string for the reconcile buffer.

If non-nil, the name of the account being reconciled will be substituted
        into the '%s'.  If nil, no header will be displayed."
  :type 'string
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-buffer-line-format "%(date)s %-4(code)s %-50(payee)s %-30(account)s %15(amount)s\n"
  "Format string for the ledger reconcile posting format.
Available fields are date, status, code, payee, account,
amount.  The format for each field is %WIDTH(FIELD), WIDTH can be
preceded by a minus sign which mean to left justify and pad the
field.  WIDTH is the minimum number of characters to display;
if string is longer, it is not truncated unless
`ledger-reconcile-buffer-payee-max-chars' or
`ledger-reconcile-buffer-account-max-chars' is defined."
  :type 'string
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-buffer-payee-max-chars -1
  "If positive, truncate payee name right side to max number of characters."
  :type 'integer
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-buffer-account-max-chars -1
  "If positive, truncate account name left side to max number of characters."
  :type 'integer
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-sort-key "(0)"
  "Key for sorting reconcile buffer.

Possible values are \"(date)\", \"(amount)\", \"(payee)\" or \"(0)\" for
no sorting, i.e. using ledger file order."
  :type 'string
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-insert-effective-date nil
  "If t, prompt for effective date when clearing transactions.

If this is a function, it is called with no arguments with point
at the posting to be cleared.  The return value is then used as
described above."
  :type '(choice boolean function)
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-finish-force-quit nil
  "If t, will force closing reconcile window after \\[ledger-reconcile-finish]."
  :type 'boolean
  :group 'ledger-reconcile)

;; s-functions below are copied from Magnars' s.el
;; prefix ledger-reconcile- is added to not conflict with s.el
(defun ledger-reconcile-s-pad-left (len padding s)
  "If S is shorter than LEN, pad it with PADDING on the left."
  (let ((extra (max 0 (- len (length s)))))
    (concat (make-string extra (string-to-char padding))
            s)))
(defun ledger-reconcile-s-pad-right (len padding s)
  "If S is shorter than LEN, pad it with PADDING on the right."
  (let ((extra (max 0 (- len (length s)))))
    (concat s
            (make-string extra (string-to-char padding)))))
(defun ledger-reconcile-s-left (len s)
  "Return up to the LEN first chars of S."
  (if (> (length s) len)
      (substring s 0 len)
    s))
(defun ledger-reconcile-s-right (len s)
  "Return up to the LEN last chars of S."
  (let ((l (length s)))
    (if (> l len)
        (substring s (- l len) l)
      s)))

(defun ledger-reconcile-truncate-right (str len)
  "Truncate STR right side with max LEN characters, and pad with '…' if truncated."
  (if (and (>= len 0) (> (length str) len))
      (ledger-reconcile-s-pad-right len "…" (ledger-reconcile-s-left (- len 1) str))
    str))

(defun ledger-reconcile-truncate-left (str len)
  "Truncate STR left side with max LEN characters, and pad with '…' if truncated."
  (if (and (>= len 0) (> (length str) len))
      (ledger-reconcile-s-pad-left len "…" (ledger-reconcile-s-right (- len 1) str))
    str))

(defun ledger-reconcile-get-cleared-or-pending-balance (buffer account)
  "Use BUFFER to Calculate the cleared or pending balance of the ACCOUNT."

  ;; these vars are buffer local, need to hold them for use in the
  ;; temp buffer below

  (with-temp-buffer
    ;; note that in the line below, the --format option is
    ;; separated from the actual format string.  emacs does not
    ;; split arguments like the shell does, so you need to
    ;; specify the individual fields in the command line.
    (ledger-exec-ledger buffer (current-buffer)
                        "balance" "--real" "--limit" "cleared or pending" "--empty" "--collapse"
                        "--format" "%(scrub(display_total))" account)
    (ledger-split-commodity-string
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun ledger-display-balance ()
  "Display the cleared-or-pending balance.
And calculate the target-delta of the account being reconciled."
  (interactive)
  (let* ((pending (ledger-reconcile-get-cleared-or-pending-balance ledger-buf ledger-acct)))
    (when pending
      (if ledger-target
          (message "Cleared and Pending balance: %s,   Difference from target: %s"
                   (ledger-commodity-to-string pending)
                   (ledger-commodity-to-string (ledger-subtract-commodity ledger-target pending)))
        (message "Pending balance: %s"
                 (ledger-commodity-to-string pending))))))

(defun ledger-is-stdin (file)
  "True if ledger FILE is standard input."
  (or
   (equal file "")
   (equal file "<stdin>")
   (equal file "/dev/stdin")))

(defun ledger-reconcile-get-buffer (where)
  "Return a buffer from WHERE the transaction is."
  (if (bufferp (car where))
      (car where)
    (error "Function ledger-reconcile-get-buffer: Buffer not set")))

(defun ledger-reconcile-insert-effective-date ()
  "Prompt for an effective date and insert it at point, if enabled.

If the value of variable `ledger-reconcile-insert-effective-date'
is a function, it is called with the point where the effective
date would be inserted.  If it returns non-nil, prompt for an
effective date and insert it at point.  If it is not a function,
do the same if its value is non-nil."
  (when (if (functionp ledger-reconcile-insert-effective-date)
            (save-excursion (funcall ledger-reconcile-insert-effective-date))
          ledger-reconcile-insert-effective-date)
    (ledger-insert-effective-date)))

(defun ledger-reconcile-toggle ()
  "Toggle the current transaction, and mark the reconcile window."
  (interactive)
  (beginning-of-line)
  (let ((where (get-text-property (point) 'where))
        (inhibit-read-only t)
        status)
    (when (ledger-reconcile-get-buffer where)
      (with-current-buffer (ledger-reconcile-get-buffer where)
        (ledger-navigate-to-line (cdr where))
        (forward-char)
        (setq status (ledger-toggle-current (if ledger-reconcile-toggle-to-pending
                                                'pending
                                              'cleared)))
        ;; Ask for effective date & insert it, if enabled
        (ledger-reconcile-insert-effective-date))
      ;; remove the existing face and add the new face
      (remove-text-properties (line-beginning-position)
                              (line-end-position)
                              (list 'font-lock-face))
      (cond ((eq status 'pending)
             (add-text-properties (line-beginning-position)
                                  (line-end-position)
                                  (list 'font-lock-face 'ledger-font-reconciler-pending-face )))
            ((eq status 'cleared)
             (add-text-properties (line-beginning-position)
                                  (line-end-position)
                                  (list 'font-lock-face 'ledger-font-reconciler-cleared-face )))
            (t
             (add-text-properties (line-beginning-position)
                                  (line-end-position)
                                  (list 'font-lock-face 'ledger-font-reconciler-uncleared-face )))))
    (forward-line)
    (beginning-of-line)
    (ledger-display-balance)))

(defun ledger-reconcile-refresh ()
  "Force the reconciliation window to refresh.
Return the number of uncleared xacts found."
  (interactive)
  (let ((inhibit-read-only t)
        (line (count-lines (point-min) (point))))
    (erase-buffer)
    (prog1
        (ledger-do-reconcile ledger-reconcile-sort-key)
      (set-buffer-modified-p t)
      (ledger-reconcile-ensure-xacts-visible)
      (goto-char (point-min))
      (forward-line line))))

(defun ledger-reconcile-refresh-after-save ()
  "Refresh the reconcile window after the ledger buffer is saved."
  (let ((curbufwin (get-buffer-window (current-buffer)))
        (curpoint (point))
        (reconcile-buf (get-buffer ledger-reconcile-buffer-name)))
    (when (buffer-live-p reconcile-buf)
      (with-current-buffer reconcile-buf
        (ledger-reconcile-refresh)
        (set-buffer-modified-p nil))
      (when curbufwin
        (select-window  curbufwin)
        (goto-char curpoint)
        (recenter)
        (ledger-highlight-xact-under-point)))))

(defun ledger-reconcile-add ()
  "Use ledger xact to add a new transaction."
  (interactive)
  (with-current-buffer ledger-buf
    (let ((date (ledger-read-date "Date: "))
          (text (read-string "Transaction: ")))
      (ledger-add-transaction (concat date " " text))))
  (ledger-reconcile-refresh))

(defun ledger-reconcile-delete ()
  "Delete the transactions pointed to in the reconcile window."
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (ledger-reconcile-get-buffer where)
      (with-current-buffer (ledger-reconcile-get-buffer where)
        (ledger-navigate-to-line (cdr where))
        (ledger-delete-current-transaction (point)))
      (let ((inhibit-read-only t))
        (delete-region (line-beginning-position)
                       (min (1+ (line-end-position)) (point-max)))
        (set-buffer-modified-p t))
      (ledger-reconcile-refresh)
      (ledger-reconcile-visit t))))

(defun ledger-reconcile-visit (&optional come-back)
  "Recenter ledger buffer on transaction and COME-BACK if non-nil."
  (interactive)
  (beginning-of-line)
  (let* ((where (get-text-property (1+ (point)) 'where))
         (target-buffer (if where
                            (ledger-reconcile-get-buffer where)
                          nil))
         (cur-win (get-buffer-window (get-buffer ledger-reconcile-buffer-name))))
    (when target-buffer
      (switch-to-buffer-other-window target-buffer)
      (ledger-navigate-to-line (cdr where))
      (forward-char)
      (recenter)
      (ledger-highlight-xact-under-point)
      (forward-char -1)
      (when (and come-back cur-win)
        (select-window cur-win)
        (get-buffer ledger-reconcile-buffer-name)))))


(defun ledger-reconcile-save ()
  "Save the ledger buffer."
  (interactive)
  (with-selected-window (selected-window) ; restoring window is needed because after-save-hook will modify window and buffers
    (dolist (buf (cons ledger-buf ledger-bufs))
      (with-current-buffer buf
        (basic-save-buffer)))))


(defun ledger-reconcile-finish ()
  "Mark all pending posting or transactions as cleared.
Depends on ledger-clear-whole-transactions, save the buffers and
exit reconcile mode if `ledger-reconcile-finish-force-quit'"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((where (get-text-property (point) 'where))
            (face  (get-text-property (point) 'font-lock-face)))
        (if (eq face 'ledger-font-reconciler-pending-face)
            (with-current-buffer (ledger-reconcile-get-buffer where)
              (ledger-navigate-to-line (cdr where))
              (ledger-toggle-current 'cleared))))
      (forward-line 1)))
  (ledger-reconcile-save)
  (when ledger-reconcile-finish-force-quit
    (ledger-reconcile-quit)))


(defun ledger-reconcile-quit ()
  "Quit the reconcile window without saving ledger buffer."
  (interactive)
  (let ((reconcile-buf (get-buffer ledger-reconcile-buffer-name))
        buf)
    (if reconcile-buf
        (with-current-buffer reconcile-buf
          (ledger-reconcile-quit-cleanup)
          (setq buf ledger-buf)
          ;; Make sure you delete the window before you delete the buffer,
          ;; otherwise, madness ensues
          (delete-window (get-buffer-window reconcile-buf))
          (kill-buffer reconcile-buf)
          (set-window-buffer (selected-window) buf)))))

(defun ledger-reconcile-quit-cleanup ()
  "Cleanup all hooks established by reconcile mode."
  (interactive)
  (let ((buf ledger-buf))
    (if (buffer-live-p buf)
        (with-current-buffer buf
          (remove-hook 'after-save-hook 'ledger-reconcile-refresh-after-save t)
          (when ledger-narrow-on-reconcile
            (ledger-occur-mode -1)
            (ledger-highlight-xact-under-point))))))

(defun ledger-marker-where-xact-is (emacs-xact posting)
  "Find the position of the EMACS-XACT in the `ledger-buf'.
POSTING is used in `ledger-clear-whole-transactions' is nil."
  (let ((buf (if (ledger-is-stdin (nth 0 emacs-xact))
                 ledger-buf
               (find-file-noselect (nth 0 emacs-xact)))))
    (cons
     buf
     (if (or ledger-clear-whole-transactions
             ;; The posting might not be part of the ledger buffer. This can
             ;; happen if the account to reconcile is the default account. In
             ;; that case, we just behave as if ledger-clear-whole-transactions
             ;; was turned on. See #58 for more info.
             (= -1 (nth 0 posting)))
         (nth 1 emacs-xact)  ;; return line-no of xact
       (nth 0 posting))))) ;; return line-no of posting

(defun ledger-reconcile-compile-format-string (fstr)
  "Return a function that implements the format string in FSTR."
  (let (fields
        (start 0))
    (while (string-match "(\\(.*?\\))" fstr start)
      (setq fields (cons (intern (match-string 1 fstr)) fields))
      (setq start (match-end 0)))
    (setq fields (cl-list* 'format (replace-regexp-in-string "(.*?)" "" fstr) (nreverse fields)))
    `(lambda (date code status payee account amount)
       ,fields)))



(defun ledger-reconcile-format-posting (beg where fmt date code status payee account amount)
  "Format posting for the reconcile buffer."
  (insert (funcall fmt date code status payee account amount))

                                        ; Set face depending on cleared status
  (if status
      (if (eq status 'pending)
          (set-text-properties beg (1- (point))
                               (list 'font-lock-face 'ledger-font-reconciler-pending-face
                                     'where where))
        (set-text-properties beg (1- (point))
                             (list 'font-lock-face 'ledger-font-reconciler-cleared-face
                                   'where where)))
    (set-text-properties beg (1- (point))
                         (list 'font-lock-face 'ledger-font-reconciler-uncleared-face
                               'where where))))

(defun ledger-reconcile-format-xact (xact fmt)
  "Format XACT using FMT."
  (dolist (posting (nthcdr 5 xact))
    (let ((beg (point))
          (where (ledger-marker-where-xact-is xact posting)))
      (ledger-reconcile-format-posting beg
                                       where
                                       fmt
                                       (ledger-format-date (nth 2 xact))  ; date
                                       (if (nth 3 xact) (nth 3 xact) "")  ; code
                                       (nth 3 posting)  ; status
                                       (ledger-reconcile-truncate-right
                                        (nth 4 xact)  ; payee
                                        ledger-reconcile-buffer-payee-max-chars)
                                       (ledger-reconcile-truncate-left
                                        (nth 1 posting)  ; account
                                        ledger-reconcile-buffer-account-max-chars)
                                       (nth 2 posting)))))  ; amount

(defun ledger-do-reconcile (&optional sort)
  "SORT the uncleared transactions in the account.
The sorted results are displayed in in the *Reconcile* buffer.
Return a count of the uncleared transactions."
  (let* ((buf ledger-buf)
         (account ledger-acct)
         (sort-by (if sort
                      sort
                    "(date)"))
         (xacts
          (with-temp-buffer
            (ledger-exec-ledger buf (current-buffer)
                                "--uncleared" "--real" "emacs" "--sort" sort-by account)
            (goto-char (point-min))
            (unless (eobp)
              (if (looking-at "(")
                  (read (current-buffer))))))
         (fmt (ledger-reconcile-compile-format-string ledger-reconcile-buffer-line-format)))
    (if (> (length xacts) 0)
        (progn
          (if ledger-reconcile-buffer-header
              (insert (format ledger-reconcile-buffer-header account)))
          (dolist (xact xacts)
            (ledger-reconcile-format-xact xact fmt))
          (goto-char (point-max))
          (delete-char -1)) ;gets rid of the extra line feed at the bottom of the list
      (insert (concat "There are no uncleared entries for " account)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)

    (length xacts)))

(defun ledger-reconcile-ensure-xacts-visible ()
  "Ensure the last of the visible transactions in the ledger buffer is visible.
This is achieved by placing that transaction at the bottom of the main window.
The key to this is to ensure the window is selected when the buffer point is
moved and recentered.  If they aren't strange things happen."

  (let ((reconcile-window (get-buffer-window (get-buffer ledger-reconcile-buffer-name))))
    (when reconcile-window
      (fit-window-to-buffer reconcile-window)
      (with-current-buffer ledger-buf
        (add-hook 'kill-buffer-hook 'ledger-reconcile-quit nil t)
        (if (get-buffer-window ledger-buf)
            (select-window (get-buffer-window ledger-buf)))
        (recenter))
      (select-window reconcile-window)
      (ledger-reconcile-visit t))
    (add-hook 'post-command-hook 'ledger-reconcile-track-xact nil t)))

(defun ledger-reconcile-track-xact ()
  "Recenter the ledger buffer on the transaction at point in the reconcile buffer."
  (if (and ledger-buffer-tracks-reconcile-buffer
           (member this-command (list 'next-line
                                      'previous-line
                                      'mouse-set-point
                                      'ledger-reconcile-toggle
                                      'end-of-buffer
                                      'beginning-of-buffer)))
      (save-excursion
        (ledger-reconcile-visit t))))

(defun ledger-reconcile-open-windows (buf rbuf)
  "Ensure that the ledger buffer BUF is split by RBUF."
  (if ledger-reconcile-force-window-bottom
      ;;create the *Reconcile* window directly below the ledger buffer.
      (set-window-buffer (split-window (get-buffer-window buf) nil nil) rbuf)
    (pop-to-buffer rbuf)))

(defun ledger-reconcile-check-valid-account (account)
  "Check to see if ACCOUNT exists in the ledger file."
  (if (> (length account) 0)
      (save-excursion
        (goto-char (point-min))
        (search-forward account nil t))))

(defun ledger-reconcile (&optional account target)
  "Start reconciling, prompt for ACCOUNT."
  (interactive)
  (let ((account (or account (ledger-read-account-with-prompt "Account to reconcile")))
        (buf (current-buffer))
        (rbuf (get-buffer ledger-reconcile-buffer-name)))

    (when (ledger-reconcile-check-valid-account account)
      (if rbuf ;; *Reconcile* already exists
          (with-current-buffer rbuf
            (set 'ledger-acct account) ;; already buffer local
            (when (not (eq buf rbuf))
              ;; called from some other ledger-mode buffer
              (ledger-reconcile-quit-cleanup)
              (setq ledger-buf buf)) ;; should already be buffer-local

            (unless (get-buffer-window rbuf)
              (ledger-reconcile-open-windows buf rbuf)))

        ;; no reconcile-buffer, starting from scratch.

        (with-current-buffer (setq rbuf
                                   (get-buffer-create ledger-reconcile-buffer-name))
          (ledger-reconcile-open-windows buf rbuf)
          (ledger-reconcile-mode)
          (make-local-variable 'ledger-target)
          (set (make-local-variable 'ledger-buf) buf)
          (set (make-local-variable 'ledger-acct) account)))

      (add-hook 'after-save-hook 'ledger-reconcile-refresh-after-save nil t)

      ;; Narrow the ledger buffer
      (if ledger-narrow-on-reconcile
          (ledger-occur (regexp-quote account)))

      (with-current-buffer rbuf
        (if (> (ledger-reconcile-refresh) 0)
            (ledger-reconcile-change-target target)
          (ledger-display-balance))))))

(defvar ledger-reconcile-mode-abbrev-table)

(defun ledger-reconcile-change-target (&optional target)
  "Change the TARGET amount for the reconciliation process."
  (interactive)
  (setq ledger-target (or target (ledger-read-commodity-string ledger-reconcile-target-prompt-string)))
  (ledger-display-balance))

(defmacro ledger-reconcile-change-sort-key-and-refresh (sort-by)
  "Set the sort-key to SORT-BY."
  `(lambda ()
     (interactive)

     (setq ledger-reconcile-sort-key ,sort-by)
     (ledger-reconcile-refresh)))

(defvar ledger-reconcile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] #'ledger-reconcile-visit)
    (define-key map [return] #'ledger-reconcile-visit)
    (define-key map [(control ?x) (control ?s)] #'ledger-reconcile-save)
    (define-key map [(control ?l)] #'ledger-reconcile-refresh)
    (define-key map [(control ?c) (control ?c)] #'ledger-reconcile-finish)
    (define-key map [? ] #'ledger-reconcile-toggle)
    (define-key map [?a] #'ledger-reconcile-add)
    (define-key map [?d] #'ledger-reconcile-delete)
    (define-key map [?g] #'ledger-reconcile);
    (define-key map [?n] #'next-line)
    (define-key map [?p] #'previous-line)
    (define-key map [?t] #'ledger-reconcile-change-target)
    (define-key map [?s] #'ledger-reconcile-save)
    (define-key map [?q] #'ledger-reconcile-quit)
    (define-key map [?b] #'ledger-display-balance)

    (define-key map [(control ?c) (control ?o)] (ledger-reconcile-change-sort-key-and-refresh "(0)"))

    (define-key map [(control ?c) (control ?a)] (ledger-reconcile-change-sort-key-and-refresh "(amount)"))

    (define-key map [(control ?c) (control ?d)] (ledger-reconcile-change-sort-key-and-refresh "(date)"))

    (define-key map [(control ?c) (control ?p)] (ledger-reconcile-change-sort-key-and-refresh "(payee)"))
    map)
  "Keymap for `ledger-reconcile-mode'.")

(easy-menu-define ledger-reconcile-mode-menu ledger-reconcile-mode-map
  "Ledger reconcile menu"
  `("Reconcile"
    ["Save" ledger-reconcile-save]
    ["Refresh" ledger-reconcile-refresh]
    ["Finish" ledger-reconcile-finish]
    "---"
    ["Reconcile New Account" ledger-reconcile]
    "---"
    ["Change Target Balance" ledger-reconcile-change-target]
    ["Show Cleared Balance" ledger-display-balance]
    "---"
    ["Sort by payee" ,(ledger-reconcile-change-sort-key-and-refresh "(payee)")]
    ["Sort by date" ,(ledger-reconcile-change-sort-key-and-refresh "(date)")]
    ["Sort by amount" ,(ledger-reconcile-change-sort-key-and-refresh "(amount)")]
    ["Sort by file order" ,(ledger-reconcile-change-sort-key-and-refresh "(0)")]
    "---"
    ["Toggle Entry" ledger-reconcile-toggle]
    ["Add Entry" ledger-reconcile-add]
    ["Delete Entry" ledger-reconcile-delete]
    "---"
    ["Next Entry" next-line]
    ["Visit Source" ledger-reconcile-visit]
    ["Previous Entry" previous-line]
    "---"
    ["Quit" ledger-reconcile-quit]
    ))

(define-derived-mode ledger-reconcile-mode text-mode "Reconcile"
  "A mode for reconciling ledger entries.")

(provide 'ledger-reconcile)

;;; ledger-reconcile.el ends here
