;;; emoji-cheat-sheet-plus.el --- emoji-cheat-sheet for emacs

;; Copyright (C) 2013 by Shingo Fukuyama
;; Copyright (C) 2015 by Sylvain Benner

;; Version: 1.0
;; Author: Sylvain Benner (based on the work of Shingo Fukuyama)
;; URL: https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus
;; Created: May 24 2015
;; Keywords: emacs emoji
;; Package-Requires: ((emacs "24") (popwin "1.0.0") (helm "1.5"))


;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.


;;; Commentary:

;; This is an improved and somewhat rewritten version of
;; `emacs-emoji-cheat-sheet' from Shingo Fukuyama

;; Features available only in this version are:
;; - emoji buffer has its own major-mode
;; - automatic display of emoji code in the minibuffer while browsing the
;;   emoji buffer
;; - new minor mode `emoji-cheat-sheet-plus-display-mode' which replaces
;;   emoji codes in buffer by the corresponding image
;; - new function `emoji-cheat-sheet-plus-insert' to insert an emoji at point
;;   using an helm front-end. It is possible to insert several emoji with helm
;;   persistent action mechanism or multiple selection.

;; This version is stand-alone and does not require the original package
;; `emacs-emoji-cheat-sheet'.

;; Configuration
;; (add-to-list 'load-path "/path/to/emacs-emoji-cheat-sheet-plus")
;; (require 'emoji-cheat-sheet-plus)


;; Notices

;; Images are from arvida/emoji-cheat-sheet.com
;; https://github.com/arvida/emoji-cheat-sheet.com

;; octocat, squirrel, shipit
;; Copyright (c) 2012 GitHub Inc. All rights reserved.
;; bowtie
;; Copyright (c) 2012 37signals, LLC. All rights reserved.
;; neckbeard
;; Copyright (c) 2012 Jamie Dihiansan. Creative Commons Attribution 3.0 Unported
;; feelsgood, finnadie, goberserk, godmode, hurtrealbad, rage 1-4, suspect
;; Copyright (c) 2012 id Software. All rights reserved.
;; trollface
;; Copyright (c) 2012 whynne@deviantart. All rights reserved.
;; All other emoji images
;; Copyright (c) 2012 Apple Inc. All rights reserved.


;;; Code:

(require 'popwin)
(require 'helm)


;; Internal

(defconst emoji-cheat-sheet-plus--buffer-name "*emoji*"
  "Name of the buffer where to display the list of emojis.")

(defvar emoji-cheat-sheet-plus--dir
  (concat (file-name-directory (or load-file-name (buffer-file-name)))
   "emoji-cheat-sheet/"))

(defvar emoji-cheat-sheet-plus-image--cache nil
  "Cache for all the images.")

(defun emoji-cheat-sheet-plus--create-cache ()
  "Create the image cache."
  (unless emoji-cheat-sheet-plus-image--cache
    (let ((files (directory-files emoji-cheat-sheet-plus--dir 'full "png$")))
      (dolist (file files)
        (let ((code (intern (format ":%s:" (file-name-base file)))))
          (push (cons code (create-image file 'png nil :ascent 'center))
                emoji-cheat-sheet-plus-image--cache)))
      (setq emoji-cheat-sheet-plus-image--cache
            (nreverse emoji-cheat-sheet-plus-image--cache)))))

(defun emoji-cheat-sheet-plus--image-file (code)
  "Return the absolute path to the image file for CODE."
  (concat emoji-cheat-sheet-plus--dir
          (substring (symbol-name code) 1 -1) ".png"))

(defun emoji-cheat-sheet-plus--insert-image (code)
  "Return a propertized string for the given CODE."
  (let ((entry (assq code emoji-cheat-sheet-plus-image--cache)))
    (when entry
      (insert-image (cdr entry) (symbol-name (car entry))))))

(defun emoji-cheat-sheet-plus--create-buffer ()
  (let ((width (/ (window-width) 5))
        (i 0))
    (dolist (entry emoji-cheat-sheet-plus-image--cache)
      (if (display-graphic-p)
          (insert-image (append (cdr entry) (list :margin 4))
                        (symbol-name (car entry)))
        (insert " " (symbol-name (car entry)) " "))
      (setq i (1+ i))
      (when (or (not (display-graphic-p))
                (eq 0 (% i width))) (insert "\n")))))

(defun emoji-cheat-sheet-plus--code-under-point ()
  "Return the code under point."
  (ignore-errors
    (save-excursion
      (re-search-forward "\:.+?\:")
      (match-string-no-properties 0))))


;; Emoji cheat sheet buffer to explore emojis

(eval-after-load 'popwin
  `(push '(,emoji-cheat-sheet-plus--buffer-name :dedicated t
                                           :position bottom
                                           :stick t)
         popwin:special-display-config))

(defun emoji-cheat-sheet-plus-echo (&optional copy)
  "Echo the emoji code and optionaly copy it in the kill ring."
  (interactive)
  (let ((code (emoji-cheat-sheet-plus--code-under-point)))
    (when code
      (when copy
        (kill-new (match-string-no-properties 0)))
      (message (format "%s%s" code (if copy " (copied to kill ring)" ""))))))

(defun emoji-cheat-sheet-plus-echo-and-copy ()
  "Echo the current code and copy it to kill ring."
  (interactive)
  (emoji-cheat-sheet-plus-echo 'and-copy))

;; we need to delay the display of the current code otherwise the
;; current point position is not correct thus the found emoji code
;; is not correct.
(defun emoji-cheat-sheet-plus-delayed-echo ()
  "Delay the echo of the emoji code under point."
  (run-at-time 0.2 nil
               (lambda ()
                 (unless (eq 'emoji-cheat-sheet-plus-echo-and-copy last-command)
                   (emoji-cheat-sheet-plus-echo)))))

(defvar emoji-cheat-sheet-plus-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-this-buffer)
    (define-key map (kbd "RET") 'emoji-cheat-sheet-plus-echo-and-copy)
    map)
  "Keymap for emoji-cheat-sheet buffer.")

;;;###autoload
(defun emoji-cheat-sheet-plus-buffer ()
  "Open a new buffer with all the emojis."
  (interactive)
  (emoji-cheat-sheet-plus--create-cache)
  (with-current-buffer (get-buffer-create emoji-cheat-sheet-plus--buffer-name)
    (erase-buffer)
    (pop-to-buffer (current-buffer))
    (emoji-cheat-sheet-plus--create-buffer)
    (emoji-cheat-sheet-plus-buffer-mode)))

(define-derived-mode emoji-cheat-sheet-plus-buffer-mode
  fundamental-mode "Emoji-Cheat-Sheet"
  "Open a buffer to display all the emojis from emoji-cheat-sheet.com

\\<emoji-cheat-sheet-plus-mode-map>
"
  :group 'emoji
  (unless (bound-and-true-p emoji-cheat-sheet-plus-buffer-mode)
    (let ((header "Emoji Cheat Sheet (Copy any emoji code with `RET')"))
      (goto-char (point-min))
      (if (boundp 'header-line-format)
          (setq-local header-line-format header)
        (insert header
                "\n--------------------------------------------------\n")))
    (read-only-mode)
    (add-hook 'post-command-hook 'emoji-cheat-sheet-plus-delayed-echo
              nil 'local)))


;; Insert Emojis with Helm

(defun emoji-cheat-sheet-plus--helm-source ()
  "Return a helm source with all emojis."
  `((name . "Emoji Cheat Sheet")
    (init . (lambda () (with-current-buffer (helm-candidate-buffer 'local)
                         (emoji-cheat-sheet-plus-display-mode -1)
                         (mapc (lambda (x)
                                 (insert-image
                                  (cdr x)
                                  (symbol-name (car x)))
                                 (insert (concat " "
                                                 (symbol-name (car x))
                                                 "\n")))
                               emoji-cheat-sheet-plus-image--cache))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (("Insert into buffer" .
                emoji-cheat-sheet-plus--insert-selection)))))

(defun emoji-cheat-sheet-plus--insert-selection (_)
  "Insert the selected emojis into the buffer."
  (dolist (c (helm-marked-candidates))
    (save-match-data
      (message "candidate %s" c)
      (string-match "\:.+?\:" c)
      (insert (match-string 0 c)))))

;;;###autoload
(defun emoji-cheat-sheet-plus-insert ()
  "Insert selected emojis from helm source."
  (interactive)
  (emoji-cheat-sheet-plus--create-cache)
  (helm :sources (emoji-cheat-sheet-plus--helm-source)
        :candidate-number-limit 1000))


;; Replace emoji codes in buffer with images

;;;###autoload
(define-minor-mode emoji-cheat-sheet-plus-display-mode
  "Minor mode to display emoji cheat sheet images in buffer."
  :group 'emoji
  :lighter " emoji"
  :init-value nil
  (cond
   (emoji-cheat-sheet-plus-display-mode
    (emoji-cheat-sheet-plus--create-cache)
    (save-restriction
      (widen)
      (emoji-cheat-sheet-plus--display-region (point-min) (point-max)))
    (add-hook 'after-change-functions 'emoji-cheat-sheet-plus--changed-hook
              nil t)
    (add-hook 'find-file-hook 'emoji-cheat-sheet-plus--visit-hook nil t))
   (t
    (remove-hook 'after-change-functions 'emoji-cheat-sheet-plus--changed-hook
                 t)
    (remove-hook 'find-file-hook 'emoji-cheat-sheet-plus--visit-hook t)
    (save-restriction
      (widen)
      (emoji-cheat-sheet-plus--undisplay-region (point-min) (point-max))))))

(defun emoji-cheat-sheet-plus--visit-hook ()
  "Hook function for `find-file-hook' to display emoji image."
  (emoji-cheat-sheet-plus--display-region (point-min) (point-max)))

(defun emoji-cheat-sheet-plus--changed-hook (start end length)
  "Hook function for `after-change-functions' to display emoji image."
  (emoji-cheat-sheet-plus--display-region (line-beginning-position) end))

(defun emoji-cheat-sheet-plus--display-region (start end)
  "Add emoji display properties to passed region."
  (save-excursion
    (goto-char start)
    (let ((inhibit-read-only t)
          (modified (buffer-modified-p)))
      (while (re-search-forward "\:[a-z0-9\\+_-]+?\:" end t)
        (let* ((code (intern (match-string 0)))
               (image (cdr (assq code emoji-cheat-sheet-plus-image--cache))))
          (when image
            ;; propertize only the inner code of the emoji
            ;; the `:' are made invisible
            ;; this allows to correctly render several contiguous
            ;; occurrences of the same emoji
            (let ((inhibit-modification-hooks t))
              (add-text-properties
               (match-beginning 0) (1+ (match-beginning 0))
               '(invisible t emoji-cheat-sheet-plus-display t))
              (add-text-properties
               (1+ (match-beginning 0)) (match-end 0)
               `(display ,image emoji-cheat-sheet-plus-display t))))))
      (set-buffer-modified-p modified))))

(defun emoji-cheat-sheet-plus--undisplay-region (start end)
  "Remove emoji display properties from passed region."
  (save-excursion
    (goto-char start)
    (let ((point start)
          (inhibit-read-only t)
          (modified (buffer-modified-p)))
      (while (null (eq point end))
        (goto-char (next-single-property-change
                    point 'emoji-cheat-sheet-plus-display nil end))
        (when (get-text-property point 'emoji-cheat-sheet-plus-display)
          (remove-list-of-text-properties
           point (point) '(emoji-cheat-sheet-plus-display display)))
        (add-text-properties point (point) `(invisible nil))
        (setq point (point)))
      (set-buffer-modified-p modified))))

(provide 'emoji-cheat-sheet-plus)
;;; emoji-cheat-sheet-plus.el ends here
