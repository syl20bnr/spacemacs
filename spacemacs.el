(defun spacemacs-load-dotfile ()
  "Load ~/.spacemacs. If it is not found then copy .spacemacs.template to
~/.spacemacs"
  (let ((dotfile (concat user-home-directory ".spacemacs")))
    (unless (file-exists-p dotfile)
      (copy-file (concat user-emacs-directory ".spacemacs.template") dotfile))
    (load dotfile)))

(defvar spacemacs-title-length 74)
(defvar loading-counter 0)
(defvar loading-text "Loading")
(defvar loading-done-text "Ready!")
(defvar loading-dots-chunk-count 3)
(defvar loading-dots-count-max
  (- spacemacs-title-length (length loading-text) (length loading-done-text)))
(defvar loading-dots-chunk-size
  (/ loading-dots-count-max loading-dots-chunk-count))
(defvar loading-dots-chunk-threshold 0)

(defun create-spacemacs-buf ()
  "Create and initialize the spacemacs startup buffer."
  (switch-to-buffer (get-buffer-create "*spacemacs*"))
  (insert-file-contents (concat user-emacs-directory "banner.txt"))
  (redisplay))

(defun append-to-spacemacs-buf (msg)
  "Append MSG to spacemacs buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (insert (format "%s" msg))))

(defun replace-last-line-of-spacemacs-buf (msg)
  "Replace the last line of the spacemacs buffer with MSG."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (delete-region (line-beginning-position) (point-max))
    (insert msg)))

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
(add-hook 'after-init-hook (lambda ()
                             (append-to-spacemacs-buf loading-done-text)))

