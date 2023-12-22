;;; -*-lexical-binding:t-*-
;;; (require 'sly)
(require 'eldoc)
(require 'cl-lib)
(require 'sly-parse "lib/sly-parse")

(define-sly-contrib sly-autodoc
  "Show fancy arglist in echo area."
  (:license "GPL")
  (:authors "Luke Gorrie  <luke@bluetail.com>"
            "Lawrence Mitchell  <wence@gmx.li>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler  <tcr@freebits.de>")
  (:slynk-dependencies slynk/arglists)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-autodoc-mode)
            (add-hook 'sly-mrepl-mode-hook 'sly-autodoc-mode)
            (add-hook 'sly-minibuffer-setup-hook 'sly-autodoc-mode))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-autodoc-mode)
              (remove-hook 'sly-mrepl-mode-hook 'sly-autodoc-mode)
              (remove-hook 'sly-minibuffer-setup-hook 'sly-autodoc-mode)))

(defcustom sly-autodoc-accuracy-depth 10
  "Number of paren levels that autodoc takes into account for
  context-sensitive arglist display (local functions. etc)"
  :type 'integer
  :group 'sly-ui)



(defun sly-arglist (name)
  "Show the argument list for NAME."
  (interactive (list (sly-read-symbol-name "Arglist of: " t)))
  (let ((arglist (sly-autodoc--retrieve-arglist name)))
    (if (eq arglist :not-available)
        (error "Arglist not available")
        (message "%s" (sly-autodoc--fontify arglist)))))

(defun sly-autodoc--retrieve-arglist (name)
  (let ((name (cl-etypecase name
		(string name)
		(symbol (symbol-name name)))))
    (car (sly-eval `(slynk:autodoc '(,name ,sly-cursor-marker))))))

(defun sly-autodoc-manually ()
  "Like autodoc information forcing multiline display."
  (interactive)
  (let ((doc (sly-autodoc t)))
    (cond (doc (eldoc-message (format "%s" doc)))
	  (t (eldoc-message nil)))))

;; Must call eldoc-add-command otherwise (eldoc-display-message-p)
;; returns nil and eldoc clears the echo area instead.
(eldoc-add-command 'sly-autodoc-manually)

(defun sly-autodoc-space (n)
  "Like `sly-space' but nicer."
  (interactive "p")
  (self-insert-command n)
  (let ((doc (sly-autodoc)))
    (when doc
      (eldoc-message (format "%s" doc)))))

(eldoc-add-command 'sly-autodoc-space)


;;;; Autodoc cache

(defvar sly-autodoc--cache-last-context nil)
(defvar sly-autodoc--cache-last-autodoc nil)


;;;; Formatting autodoc

(defsubst sly-autodoc--canonicalize-whitespace (string)
  (replace-regexp-in-string "[ \n\t]+" " "  string))

(defvar sly-autodoc-preamble nil)

(defun sly-autodoc--format (doc multilinep)
  (let* ((strings (delete nil
                          (list sly-autodoc-preamble
                                (and doc
                                     (sly-autodoc--fontify doc)))))
         (message (and strings (mapconcat #'identity strings "\n"))))
    (when message
      (cond (multilinep message)
            (t (sly-oneliner (sly-autodoc--canonicalize-whitespace message)))))))

(defun sly-autodoc--fontify (string)
  "Fontify STRING as `font-lock-mode' does in Lisp mode."
  (with-current-buffer (get-buffer-create (sly-buffer-name :fontify :hidden t))
    (erase-buffer)
    (unless (eq major-mode 'lisp-mode)
      ;; Just calling (lisp-mode) will turn sly-mode on in that buffer,
      ;; which may interfere with this function
      (setq major-mode 'lisp-mode)
      (lisp-mode-variables t))
    (insert string)
    (let ((font-lock-verbose nil))
      (font-lock-fontify-buffer))
    (goto-char (point-min))
    (when (re-search-forward "===> \\(\\(.\\|\n\\)*\\) <===" nil t)
      (let ((highlight (match-string 1)))
        ;; Can't use (replace-match highlight) here -- broken in Emacs 21
        (delete-region (match-beginning 0) (match-end 0))
        (sly-insert-propertized '(face eldoc-highlight-function-argument)
                                highlight)))
    (buffer-substring (point-min) (point-max))))


;;;; Autodocs (automatic context-sensitive help)

(defun sly-autodoc (&optional force-multiline)
  "Returns the cached arglist information as string, or nil.
If it's not in the cache, the cache will be updated asynchronously."
  (interactive "P")
  (save-excursion
    (save-match-data
      ;; See github#385 and
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45117
      (let* ((inhibit-quit t)
             (context
              (cons
               (sly-current-connection)
               (sly-autodoc--parse-context))))
	(when (car context)
	  (let* ((cached (and (equal context sly-autodoc--cache-last-context)
                              sly-autodoc--cache-last-autodoc))
		 (multilinep (or force-multiline
				 eldoc-echo-area-use-multiline-p)))
	    (cond (cached (sly-autodoc--format cached multilinep))
		  (t
		   (when (sly-background-activities-enabled-p)
		     (sly-autodoc--async context multilinep))
		   nil))))))))

;; Return the context around point that can be passed to
;; slynk:autodoc.  nil is returned if nothing reasonable could be
;; found.
(defun sly-autodoc--parse-context ()
  (and (not (sly-inside-string-or-comment-p))
       (sly-parse-form-upto-point sly-autodoc-accuracy-depth)))

(defun sly-autodoc--async (context multilinep)
  (sly-eval-async
      `(slynk:autodoc ',(cdr context) ;; FIXME: misuse of quote
		      :print-right-margin ,(window-width (minibuffer-window)))
    (sly-curry #'sly-autodoc--async% context multilinep)))

(defun sly-autodoc--async% (context multilinep doc)
  (cl-destructuring-bind (doc &optional cache-p) doc
    (unless (eq doc :not-available)
      (when cache-p
        (setq sly-autodoc--cache-last-context context)
        (setq sly-autodoc--cache-last-autodoc doc))
      ;; Now that we've got our information,
      ;; get it to the user ASAP.
      (when (eldoc-display-message-p)
	(eldoc-message (format "%s" (sly-autodoc--format doc multilinep)))))))


;;; Minor mode definition
(defvar sly-autodoc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d A") 'sly-autodoc)
    map))

(define-minor-mode sly-autodoc-mode
  "Toggle echo area display of Lisp objects at point."
  nil nil nil
  (cond (sly-autodoc-mode
         (set (make-local-variable 'eldoc-documentation-function) 'sly-autodoc)
         (set (make-local-variable 'eldoc-minor-mode-string) "")
         (eldoc-mode sly-autodoc-mode))
        (t
         (eldoc-mode -1)
         (set (make-local-variable 'eldoc-documentation-function) nil)
         (set (make-local-variable 'eldoc-minor-mode-string) " ElDoc"))))

(provide 'sly-autodoc)
