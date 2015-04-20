(defun ibuffer/get-major-modes-ibuff-rules-list (mm-list result-list)
  (if mm-list
      (let* ((cur-mm (car mm-list))
             (next-res-list-el `(,(symbol-name cur-mm) (mode . ,cur-mm))))
        (ibuffer/get-major-modes-ibuff-rules-list (cdr mm-list) (cons next-res-list-el result-list)))
    result-list))

(defun ibuffer/get-major-modes-list ()
  (mapcar
   (function (lambda (buffer) (buffer-local-value 'major-mode (get-buffer buffer))))
   (buffer-list (selected-frame))))

(defun ibuffer/create-buffs-group ()
  (interactive)
  (let* ((ignore-modes '(Buffer-menu-mode
			 compilation-mode
			 minibuffer-inactive-mode
			 ibuffer-mode
			 magit-process-mode
			 messages-buffer-mode
			 fundamental-mode
			 completion-list-mode
			 help-mode
			 Info-mode))
	 (cur-bufs (list (cons "Home"
			       (ibuffer/get-major-modes-ibuff-rules-list
				(cl-set-difference 
				 (remove-duplicates (ibuffer/get-major-modes-list)) ignore-modes) '())))))
    (setq ibuffer-saved-filter-groups cur-bufs)
    (ibuffer-switch-to-saved-filter-groups "Home")))

