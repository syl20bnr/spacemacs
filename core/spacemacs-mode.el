(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

(defvar spacemacs-min-version "24.3"
  "Mininal required version of Emacs.")

(define-derived-mode spacemacs-mode special-mode "spacemacs-mode"
  "Spacemacs major mode for startup screen."
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  (setq cursor-type nil)
  ;; no welcome buffer
  (setq inhibit-startup-screen t)
  ;; motion state since this is a special mode
  (eval-after-load 'evil
    '(add-to-list 'evil-motion-state-modes 'spacemacs-mode)))

(defun spacemacs/emacs-version-ok ()
  (not (version< emacs-version spacemacs-min-version)))

(defun display-startup-echo-area-message ()
  "Change the default welcome message of minibuffer to another one."
  (message "Spacemacs is ready."))

(defvar spacemacs-title-length 70)
(defvar spacemacs-loading-counter 0)
(defvar spacemacs-loading-text "Loading")
(defvar spacemacs-loading-done-text "Ready!")
(defvar spacemacs-loading-dots-chunk-count 3)
(defvar spacemacs-loading-dots-count
  (- spacemacs-title-length
     (length spacemacs-loading-text)
     (length spacemacs-loading-done-text)))
(defvar spacemacs-loading-dots-chunk-size
  (/ spacemacs-loading-dots-count spacemacs-loading-dots-chunk-count))
(defvar spacemacs-loading-dots-chunk-threshold 0)

(defun spacemacs/buffer ()
  "Create and initialize the spacemacs startup buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*spacemacs*"))
  (spacemacs-mode)
  (let ((buffer-read-only nil))
    (insert-file-contents (concat spacemacs-core-directory "banner.txt"))
    (unless (spacemacs/emacs-version-ok)
      (spacemacs/append-to-buffer
       (format "\nError: Minimal required Emacs version for Spacemacs is %s "
               spacemacs-min-version))
      (spacemacs/append-to-buffer (format
                                   "whereas current Emacs version is %s.\n"
                                   emacs-version))
      (spacemacs/append-to-buffer "Spacemacs is disabled.\n")
      (setq inhibit-startup-screen t)
      (redisplay))))

(defun spacemacs/append-to-buffer (msg &optional messagebuf)
  "Append MSG to spacemacs buffer. If MESSAGEBUF is not nil then MSG is
 also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))))

(defun spacemacs/replace-last-line-of-buffer (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG. If MESSAGEBUF is
 not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))))

(defun spacemacs/loading-animation ()
  "Display LOADING-TITLE with trailing dots of max length
SPACEMACS-TITLE-LENGTH. New loading title is displayed by chunk
of size LOADING-DOTS-CHUNK-THRESHOLD."
  (setq spacemacs-loading-counter (1+ spacemacs-loading-counter))
  (if (>= spacemacs-loading-counter spacemacs-loading-dots-chunk-threshold)
      (progn 
        (setq spacemacs-loading-counter 0)
        (let ((i 0))
          (while (< i spacemacs-loading-dots-chunk-size)
            (setq spacemacs-loading-text (concat spacemacs-loading-text "."))
            (setq i (1+ i))))
        (spacemacs/replace-last-line-of-buffer spacemacs-loading-text)
        (redisplay))))

