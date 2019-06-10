;;; spacemacs-xclipboard.el --- Add support for xclipboard in the terminal
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Authors: Charles Weill <weill@google.com>
;;          Google LLC.
;;; Commentary:
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Code:

(defun spacemacs/xclipboard-get-display ()
  (shell-command-to-string "if [[ -n $TMUX ]]; then
      export DISPLAY=$(tmux show-environment | grep -o '^DISPLAY.*$' | sed 's/DISPLAY=//')
    fi
    if [[ -z $DISPLAY ]]; then
      export DISPLAY=:0
    fi
    printf $DISPLAY")
  )

(defun spacemacs//xclipboard-get-copy-command ()
  (shell-command-to-string "command_exists() {
      local command=\"$1\"
      type \"$command\" >/dev/null 2>&1
    }

    # Installing reattach-to-user-namespace is recommended on OS X
    if command_exists \"pbcopy\"; then
        if command_exists \"reattach-to-user-namespace\"; then
            printf \"reattach-to-user-namespace pbcopy\"
        else
            printf \"pbcopy\"
        fi
    elif command_exists \"clip.exe\"; then # WSL clipboard command
        printf \"clip.exe\"
    elif command_exists \"xsel\"; then
        printf \"xsel -ib\"
    elif command_exists \"putclip\"; then # cygwin clipboard command
        printf \"putclip\"
    fi")
  )

(defun spacemacs//xclipboard-get-paste-command ()
  (shell-command-to-string "command_exists() {
      local command=\"$1\"
      type \"$command\" >/dev/null 2>&1
    }

    # Installing reattach-to-user-namespace is recommended on OS X
    if command_exists \"pbpaste\"; then
        if command_exists \"reattach-to-user-namespace\"; then
            printf \"reattach-to-user-namespace pbpaste\"
        else
            printf \"pbpaste\"
        fi
    elif command_exists \"paste.exe\"; then # WSL clipboard command
        printf \"paste.exe\"
    elif command_exists \"xsel\"; then
        printf \"xsel -ob\"
    elif command_exists \"getclip\"; then # cygwin clipboard command
        printf \"getclip\"
    fi")
  )

(defun spacemacs/xclipboard-copy ()
  "Copies selection to x-clipboard."
  (interactive)
  (if (display-graphic-p)
    (progn
      (message "Copied region to x-clipboard!")
      (call-interactively 'clipboard-kill-ring-save)
      )
    (if (region-active-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) (format "DISPLAY=%s %s" (spacemacs/xclipboard-get-display) (spacemacs//xclipboard-get-copy-command)))
        (message (format "Copied region to clipboard \"%s\"!" (spacemacs/xclipboard-get-display)))
        (deactivate-mark)
        )
      (message "No region active; can't copy to clipboard!")
      )
    )
  )

(defun spacemacs/xclipboard-paste ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
    (progn
      (clipboard-yank)
      (message "graphics active")
      )
    (insert (shell-command-to-string (format "DISPLAY=%s %s" (spacemacs/xclipboard-get-display) (spacemacs//xclipboard-get-paste-command))))
    )
  (message (format "Pasted from clipboard \"%s\"!" (spacemacs/xclipboard-get-display)))
  )

(provide 'spacemacs-xclipboard)
