;; from jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

(defvar spacemacs-title-length 116)
(defvar loading-text "Loading")
(defvar loading-done-text "Ready!")
(defvar loading-dots-chunk-count 3)
(defvar loading-dots-chunk-size 0)
(defvar loading-dots-chunk-size-max 0)
(defvar loading-dots-count-max
  (- spacemacs-title-length (length loading-text) (length loading-done-text)))
(defun create-spacemacs-buf ()
  "Create and initialize the spacemacs startup buffer."
  (switch-to-buffer (get-buffer-create "*spacemacs*"))
  (insert-file-contents (concat user-emacs-directory "banner.txt"))
  (goto-char (point-max))
  (insert loading-text)
  (redisplay))
(defun append-to-spacemacs-buf (msg)
  "Append MSG to spacemacs buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (insert (format "%s\n" msg))))
(defun replace-last-line-of-spacemacs-buf (msg)
  "Replace the last line of the spacemacs buffer with MSG."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (delete-region (line-beginning-position) (point-max))
    (insert msg)))
(defun loading-animation ()
  "Display LOADING-TITLE with trailing dots of max length
SPACEMACS-TITLE-LENGTH. New loading title is displayed by chunk
of size LOADING-DOTS-CHUNK-SIZE-MAX."
  (setq loading-dots-chunk-size (1+ loading-dots-chunk-size))
  (setq loading-text (concat loading-text "."))
  (if (>= loading-dots-chunk-size loading-dots-chunk-size-max)
      (progn 
        (setq loading-dots-chunk-size 0)
        (replace-last-line-of-spacemacs-buf loading-text)
        (redisplay))))

(defconst user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "User home directory (~/).")
(defconst contrib-config-directory
  (expand-file-name (concat user-emacs-directory "contrib/"))
  "Contribution layers base directory.")
(defconst user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")
;; if you have a dropbox, then ~/Dropbox/emacs is added to load path
(add-to-list 'load-path (concat user-dropbox-directory "emacs/"))

;; User configuration file for Spacemacs: ~/.spacemacs 
(load (concat user-home-directory ".spacemacs"))
(dotspacemacs/init)

(load (concat user-emacs-directory "contribsys.el"))
(contribsys/declare-layer 'spacemacs)
(contribsys/declare-configuration-layers)
(contribsys/load-layers)

;; Last configuration decisions are given to the user who can defined them 
;; in ~/.spacemacs
(dotspacemacs/config)

(append-to-spacemacs-buf loading-done-text)

; from jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
;; Display load times after init.el and after all buffers has been loaded
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))
(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time
                             (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
