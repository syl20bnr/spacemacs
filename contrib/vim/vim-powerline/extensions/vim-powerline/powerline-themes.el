;;; powerline-themes.el --- Themes for Powerline

;; Copyright (C) 2012-2013 Donald Ephraim Curtis
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

(defcustom powerline-display-buffer-size t
  "When non-nil, display the buffer size."
  :type 'boolean)

(defcustom powerline-display-mule-info t
  "When non-nil, display the mule info."
  :type 'boolean)

(defcustom powerline-display-hud t
  "When non-nil, display the hud."
  :type 'boolean)

;;;###autoload
(defun powerline-default-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size nil 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info nil 'l))
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) face1 'l))
				     (powerline-raw "%4l" face1 'l)
				     (powerline-raw ":" face1 'l)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face2 face1)))))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

;;;###autoload
(defun powerline-center-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-raw "%*" nil 'l)
				     (powerline-buffer-size nil 'l)
				     (powerline-buffer-id nil 'l)
				     (powerline-raw " ")
				     (funcall separator-left mode-line face1)
				     (powerline-narrow face1 'l)
				     (powerline-vc face1)))
			  (rhs (list (powerline-raw global-mode-string face1 'r)
				     (powerline-raw "%4l" face1 'r)
				     (powerline-raw ":" face1)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)
				     (powerline-hud face2 face1)))
			  (center (list (powerline-raw " " face1)
					(funcall separator-left face1 face2)
					(when (boundp 'erc-modified-channels-object)
					  (powerline-raw erc-modified-channels-object face2 'l))
					(powerline-major-mode face2 'l)
					(powerline-process face2)
					(powerline-raw " :" face2)
					(powerline-minor-modes face2 'l)
					(powerline-raw " " face2)
					(funcall separator-right face2 face1))))
		     (concat (powerline-render lhs)
			     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill face1 (powerline-width rhs))
			     (powerline-render rhs)))))))

(defun powerline-center-evil-theme ()
  "Setup a mode-line with major, evil, and minor modes centered."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-raw "%*" nil 'l)
				     (powerline-buffer-size nil 'l)
				     (powerline-buffer-id nil 'l)
				     (powerline-raw " ")
				     (funcall separator-left mode-line face1)
				     (powerline-narrow face1 'l)
				     (powerline-vc face1)))
			  (rhs (list (powerline-raw global-mode-string face1 'r)
				     (powerline-raw "%4l" face1 'r)
				     (powerline-raw ":" face1)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)
				     (powerline-hud face2 face1)))
			  (center (append (list (powerline-raw " " face1)
						(funcall separator-left face1 face2)
						(when (boundp 'erc-modified-channels-object)
						  (powerline-raw erc-modified-channels-object face2 'l))
						(powerline-major-mode face2 'l)
						(powerline-process face2)
						(powerline-raw " " face2))
					  (if (split-string (format-mode-line minor-mode-alist))
					      (append (if evil-mode
							  (list (funcall separator-right face2 face1)
								(powerline-raw evil-mode-line-tag face1 'l)
								(powerline-raw " " face1)
								(funcall separator-left face1 face2)))
						      (list (powerline-minor-modes face2 'l)
							    (powerline-raw " " face2)
							    (funcall separator-right face2 face1)))
					    (list (powerline-raw evil-mode-line-tag face2)
						  (funcall separator-right face2 face1))))))
		     (concat (powerline-render lhs)
			     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill face1 (powerline-width rhs))
			     (powerline-render rhs)))))))

;;;###autoload
(defun powerline-vim-theme ()
  "Setup a Vim-like mode-line."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-buffer-id `(mode-line-buffer-id ,mode-line) 'l)
				     (powerline-raw "[" mode-line 'l)
				     (powerline-major-mode mode-line)
				     (powerline-process mode-line)
				     (powerline-raw "]" mode-line)
				     (when (buffer-modified-p)
				       (powerline-raw "[+]" mode-line))
				     (when buffer-read-only
				       (powerline-raw "[RO]" mode-line))
				     (powerline-raw "[%z]" mode-line)
				     ;; (powerline-raw (concat "[" (mode-line-eol-desc) "]") mode-line)
				     (when (and (boundp 'which-func-mode) which-func-mode)
				       (powerline-raw which-func-format nil 'l))
				     (when (boundp 'erc-modified-channels-object)
				       (powerline-raw erc-modified-channels-object face1 'l))
				     (powerline-raw "[" mode-line 'l)
				     (powerline-minor-modes mode-line)
				     (powerline-raw "%n" mode-line)
				     (powerline-raw "]" mode-line)
				     (when (and vc-mode buffer-file-name)
				       (let ((backend (vc-backend buffer-file-name)))
					 (when backend
					   (concat (powerline-raw "[" mode-line 'l)
						   (powerline-raw (format "%s / %s" backend (vc-working-revision buffer-file-name backend)))
						   (powerline-raw "]" mode-line)))))))
			  (rhs (list (powerline-raw '(10 "%i"))
				     (powerline-raw global-mode-string mode-line 'r)
				     (powerline-raw "%l," mode-line 'l)
				     (powerline-raw (format-mode-line '(10 "%c")))
				     (powerline-raw (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p"))) mode-line 'r))))
		     (concat (powerline-render lhs)
			     (powerline-fill mode-line (powerline-width rhs))
			     (powerline-render rhs)))))))

;;;###autoload
(defun powerline-nano-theme ()
  "Setup a nano-like mode-line."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (lhs (list (powerline-raw (concat "GNU Emacs "
							    (number-to-string
							     emacs-major-version)
							    "."
							    (number-to-string
							     emacs-minor-version))
						    nil 'l)))
			  (rhs (list (if (buffer-modified-p) (powerline-raw "Modified" nil 'r))))
			  (center (list (powerline-raw "%b" nil))))
		     (concat (powerline-render lhs)
			     (powerline-fill-center nil (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill nil (powerline-width rhs))
			     (powerline-render rhs)))))))

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
          (lambda () (let ((prefix (subseq (or split-face-name (split-string face "-")) 0 2)))
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

(provide 'powerline-themes)

;;; powerline-themes.el ends here
