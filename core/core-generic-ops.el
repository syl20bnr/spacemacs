(require 'evil)

;; Evaling is for modes who can send text to a repl and receive a response

(defvar spacemacs--generic-eval-region-fallback-to-send t
  "When truthy, fallback to using spacemacs--generic-send-region-alist if no eval func exists for mode")

(defvar spacemacs--generic-eval-region-alist '()
  "Alist mapping mode to a 2-arity function which evals from beg to end for the mode")

(defun spacemacs//get-eval-region-for-current-mode ()
  (interactive)
  (catch 'break
    (dolist (test spacemacs--generic-eval-region-alist)
      (let ((mode (car test))
            (val (cdr test)))
        (when (and (symbolp mode)
                   (derived-mode-p mode)
                   (symbolp val))
          (throw 'break val))))))


(defun spacemacs//generic-eval-region (beg end)
  (interactive)
  (let* ((f (or (spacemacs//get-eval-region-for-current-mode)
                (when spacemacs--generic-eval-region-fallback-to-send
                  (spacemacs//get-send-region-for-current-mode)))))
    (when (functionp f)
      (funcall f beg end))))

(defvar spacemacs--generic-eval-region-want-flash t)
(when (and spacemacs--generic-eval-want-flash
           (require 'eval-sexp-fu nil 'noerror))
  (define-eval-sexp-fu-flash-command spacemacs//generic-eval-region
    (eval-sexp-fu-flash (cons beg end))))

(evil-define-operator spacemacs//generic-evil-eval-operator (beg end)
  (spacemacs//generic-eval-region beg end))

(defun spacemacs//generic-eval-line ()
  "Evals the current line as though you did (kbd \"ma^ SPC meo $`a\")"
  (interactive)
  ;; we don't use execute-kbd-macro here, because popping the mark kills the overlay
  (save-excursion
    (let* ((beg (progn (beginning-of-line) (point)))
           (end (progn (end-of-line) (point))))
      (spacemacs//generic-eval-region beg end))))

(defun spacemacs//generic-eval-paragraph ()
  "Evals the current paragraph as though you did (kbd \"ma SPC meo ap`a\")"
  (interactive)
  ;; we don't use execute-kbd-macro here, because popping the mark kills the overlay
  (save-excursion
    (let* ((graph (evil-inner-paragraph))
           (beg (first graph))
           (end (second graph)))
      (spacemacs//generic-eval-region beg end))))

(defun spacemacs//generic-eval-active-region-or-paragraph ()
  "Evals the current active region or paragraph"
  (interactive)
  (if (use-region-p)
      (spacemacs//generic-eval-region (region-beginning)
                                   (region-end))
    (spacemacs//generic-eval-paragraph)))

(defun spacemacs//generic-eval-buffer ()
  "Evals the current buffer as though you did (kbd \"ma gg SPC meo VG`a\")"
  (interactive)
  ;; we don't use execute-kbd-macro here, because popping the mark kills the overlay
  (save-excursion
    (let* ((beg (progn (beginning-of-buffer)
                       (point)))
           (end (progn (end-of-buffer)
                       (point))))
      (spacemacs//generic-eval-region beg end))))

;; Sending is for modes who are content to schlep some text into a repl buffer

(defvar spacemacs--generic-send-region-fallback-to-eval t
  "When truthy, fallback to using spacemacs--generic-eval-region-alist if no send func exists for mode")

(defvar spacemacs--generic-send-region-alist '()
  "Alist mapping mode to a 2-arity function which sends text to a repl buffer from beg to end for the mode")

(defun spacemacs//get-send-region-for-current-mode ()
  (interactive)
  (catch 'break
    (dolist (test spacemacs--generic-send-region-alist)
      (let ((mode (car test))
            (val (cdr test)))
        (when (and (symbolp mode)
                   (derived-mode-p mode)
                   (symbolp val))
          (throw 'break val))))))

(defun spacemacs//generic-send-region (beg end)
  (interactive)
  (let ((f (or (spacemacs//get-send-region-for-current-mode)
               (when spacemacs--generic-send-region-fallback-to-eval
                 (spacemacs//get-eval-region-for-current-mode)))))
    (when (functionp f)
      (funcall f beg end))))

(defun spacemacs//generic-send-line ()
  "Sends the current line as though you did (kbd \"ma^ SPC mso $`a\")"
  (interactive)
  ;; we don't use execute-kbd-macro here, because popping the mark kills the overlay
  (save-excursion
    (let* ((beg (progn (beginning-of-line) (point)))
           (end (progn (end-of-line) (point))))
      (spacemacs//generic-send-region beg end))))

(defun spacemacs//generic-send-paragraph ()
  "Sends the current paragraph as though you did (kbd \"ma SPC mso ap`a\")"
  (interactive)
  ;; we don't use execute-kbd-macro here, because popping the mark kills the overlay
  (save-excursion
    (let* ((graph (evil-inner-paragraph))
           (beg (first graph))
           (end (second graph)))
      (spacemacs//generic-send-region beg end))))

(defun spacemacs//generic-send-active-region-or-paragraph ()
  "Sends the current active region or paragraph"
  (interactive)
  (if (use-region-p)
      (spacemacs//generic-send-region (region-beginning)
                                   (region-end))
    (spacemacs//generic-send-paragraph)))

(defun spacemacs//generic-send-buffer ()
  "Sends the current buffer as though you did (kbd \"ma gg SPC mso VG`a\")"
  (interactive)
  ;; we don't use execute-kbd-macro here, because popping the mark kills the overlay
  (save-excursion
    (let* ((beg (progn (beginning-of-buffer)
                       (point)))
           (end (progn (end-of-buffer)
                       (point))))
      (spacemacs//generic-send-region beg end))))

(evil-define-operator spacemacs//generic-evil-send-operator (beg end)
  (spacemacs//generic-eval-region beg end))
