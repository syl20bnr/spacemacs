;;; powerline-themes.el --- Themes for Powerline

;; Copyright (C) 2012-2022 Donald Ephraim Curtis
;; Copyright (C) 2013 Jason Milkins
;; Copyright (C) 2012 Nicolas Rougier

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline/
;; Version: 2.0
;; Keywords: mode-line

;;; Commentary:
;;
;; Themes for Powerline.
;; Included themes: default, center, center-evil, vim, and nano.
;;

;;; Code:

(require 'cl-lib)

(defface powerline-active3 '((t (:background "#afd700"
                                             :foreground "#008700"
                                             :inherit mode-line)))
  "Powerline face 3."
  :group 'powerline)

(defface powerline-inactive3 '((t (:foreground "grey90" :background "grey30" :inherit mode-line)))
  "Powerline face 3."
  :group 'powerline)

(defun check-in-list (list elems)
  (catch 'tag
	(dolist (elem elems)
	  (if (member elem list)
		  (throw 'tag elem)))))

;; Get a face for the current input mode and
;; desired feature. Defaults to "powerline-FEATURE-normal"
(defun pl/get-vim-face (face)
  "Find whether or not FACE is a valid face,
and if not, try to get the corresponding
'-normal' face "
  (let* ((face (replace-regexp-in-string "iedit-insert" "iedit" face))
         (split-face-name nil) (concat-face-name nil) ; some variables we'll use later on
         (report-wrong-prefix ; Our error reporter. Because we don't want to return nil.
          (lambda () (let ((prefix (cl-subseq (or split-face-name (split-string face "-")) 0 2)))
                       (error "There's no vim face with the prefix: %s"
                              (mapconcat 'identity prefix "-"))))))

    (cond ((facep face)                 ; If our face is a FACE (even if it's not a powerline face)
           (intern face))               ; just intern the string and send it back

          ;; Otherwise.
          ;; Are we a list of only three items?
          ;; Is the first item a string that is 'powerline'?
          ;; Is the last element not 'normal'? (If it was valid, it would have already passed)
          ((and (setf split-face-name (split-string face "-"))
                (string-equal (cl-first split-face-name) "powerline")
                (not (string-equal (cl-third split-face-name) "normal")))

           ;; If we passed all the tests above, then we try to create a valid
           ;; powerline-face symbol
           (progn
             (setf (cl-third split-face-name) "normal")

             ;; Re-build our string from the split pieces
             (setf concat-face-name (mapconcat 'identity split-face-name "-"))

             ;; And do one final check to make sure it's a face
             ;; before sending it off
             ;; (message "Concatenated name: %s" concat-face-name)
             (if (facep concat-face-name)
                 (intern concat-face-name)
               (report-wrong-prefix))))

          ;; Always fallthrough to the error
          (t (report-wrong-prefix)))))


(defmacro pl/vim-face (name state)
  `(pl/get-vim-face (format "powerline-%s-%s" ,name ,state)))

(require 'vim-colors)
(defun powerline-vimish-theme ()
  "Setup the default mode-line."
  ;; Populate our faces
  (mapcar 'eval  (powerline--generate-facedefs powerline-vim-colors-alist))
  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background (pl/vim-face "SPLIT" "inactive")) :underline nil
                      :overline nil :box nil)
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (harddiv-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (harddiv-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (softdiv-left (cl-case powerline-default-separator
                                          ((utf-8 arrow) "")
                                          ((bar nil) "|")
                                          (brace "}")
                                          (t ">")))
                          (softdiv-right (cl-case powerline-default-separator
                                          ((utf-8 arrow) "")
                                          ((bar nil) "|")
                                          (brace "{")
                                          (t "<")))
                          (editor-state (cond ((and active (boundp 'evil-mode) evil-mode)
                                               (symbol-name evil-state))
                                              (active "active")
                                              (t "inactive")))
                          (state-indicator-face (pl/vim-face "state_indicator" editor-state))
                          (vc-face              (pl/vim-face "branch" editor-state))
                          (fileinfo-face        (pl/vim-face "fileinfo" editor-state))
                          (split-face           (pl/vim-face "SPLIT" editor-state))

                          (fileformat-face      (pl/vim-face "fileformat" editor-state))
                          (fileencoding-face    (pl/vim-face "fileencoding" editor-state))
                          (filetype-face        (pl/vim-face "filetype" editor-state))

                          (scrollpercent-face   (pl/vim-face "scrollpercent" editor-state))
                          (lineinfo-face        (pl/vim-face "lineinfo" editor-state))

                          (input (split-string (symbol-name buffer-file-coding-system) "-"))
                          (platform (check-in-list input '("mac" "unix" "dos")))
                          (encoding (mapconcat 'identity (delete platform input) "-"))

                          ;; Left hand side
                          (lhs (list
                                (powerline-raw (format " %s " (upcase editor-state)) state-indicator-face)
                                (funcall harddiv-left state-indicator-face vc-face)
                                (when (and (buffer-file-name (current-buffer)) vc-mode)
                                  (concat
                                   (powerline-raw (downcase (format-mode-line '(vc-mode vc-mode))) vc-face 'r)
                                   (powerline-raw softdiv-left vc-face)))
                                (powerline-buffer-id fileinfo-face 'l)
                                (powerline-raw "%*" fileinfo-face 'lr)
                                (powerline-narrow fileinfo-face 'l)
                                (funcall harddiv-left fileinfo-face split-face)))

                          ;; Right Hand Side
                          (rhs (list
                                (powerline-raw global-mode-string split-face 'r)
                                (funcall harddiv-right split-face fileformat-face)
                                (concat
                                 (when (not (null platform))
                                   (concat (powerline-raw platform fileformat-face 'r)
                                           (powerline-raw softdiv-right fileformat-face)))
                                 (powerline-raw encoding fileencoding-face 'lr)
                                 (powerline-raw softdiv-right fileencoding-face))
                                (powerline-major-mode filetype-face 'lr)
                                (funcall harddiv-right filetype-face scrollpercent-face)
                                (powerline-raw "%p" scrollpercent-face 'lr)
                                (funcall harddiv-right scrollpercent-face lineinfo-face)
                                (powerline-raw "%l" lineinfo-face 'l)
                                (powerline-raw ":" lineinfo-face 'lr)
                                (powerline-raw "%c" lineinfo-face 'r))))

                     (when active
                       (set-face-attribute 'mode-line nil :underline nil :overline nil :box nil))
                     (if (and (null powerline-default-separator)
                              (null  (face-attribute 'powerline-SPLIT-normal :overline)))
                         nil
                       nil)
                     (concat (powerline-render lhs)
                             (powerline-fill split-face (powerline-width rhs))
                             (powerline-render rhs)))))))

(provide 'vim-powerline-theme)

;;; powerline-themes.el ends here
