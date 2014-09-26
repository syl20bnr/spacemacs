(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

(define-derived-mode spacemacs-mode special-mode "spacemacs-mode"
  "Spacemacs major mode for startup screen."
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  (setq cursor-type nil))

(defun spacemacs-load-dotfile ()
  "Load ~/.spacemacs. If it is not found then copy .spacemacs.template to
~/.spacemacs"
  (let ((dotfile (concat user-home-directory ".spacemacs")))
    (unless (file-exists-p dotfile)
      (copy-file (concat user-emacs-directory ".spacemacs.template") dotfile))
    (load dotfile)))

(defvar spacemacs-title-length 70)
(defvar loading-counter 0)
(defvar loading-text "Loading")
(defvar loading-done-text "Ready!")
(defvar loading-dots-chunk-count 3)
(defvar loading-dots-count
  (- spacemacs-title-length (length loading-text) (length loading-done-text)))
(defvar loading-dots-chunk-size (/ loading-dots-count loading-dots-chunk-count))
(defvar loading-dots-chunk-threshold 0)

(defun spacemacs-buffer ()
  "Create and initialize the spacemacs startup buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*spacemacs*"))
  (spacemacs-mode)
  (let ((buffer-read-only nil))
    (insert-file-contents (concat spacemacs-core-directory "banner.txt"))))

(defun append-to-spacemacs-buf (msg &optional messagebuf)
  "Append MSG to spacemacs buffer. If MESSAGEBUF is not nil then MSG is
 also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))))

(defun replace-last-line-of-spacemacs-buf (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG. If MESSAGEBUF is
 not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))))

(defun loading-animation ()
  "Display LOADING-TITLE with trailing dots of max length
SPACEMACS-TITLE-LENGTH. New loading title is displayed by chunk
of size LOADING-DOTS-CHUNK-THRESHOLD."
  (setq loading-counter (1+ loading-counter))
  (if (>= loading-counter loading-dots-chunk-threshold)
      (progn 
        (setq loading-counter 0)
        (let ((i 0))
          (while (< i loading-dots-chunk-size)
            (setq loading-text (concat loading-text "."))
            (setq i (1+ i))))
        (replace-last-line-of-spacemacs-buf loading-text)
        (redisplay))))

;; Ready message
(add-hook 'after-init-hook
          (lambda ()
            (append-to-spacemacs-buf (format "%s\n" loading-done-text))
            ;; from jwiegley
            ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
            (let ((elapsed (float-time
                            (time-subtract (current-time) emacs-start-time))))
              (append-to-spacemacs-buf
               (format "[%.3fs]\n" elapsed)))))

