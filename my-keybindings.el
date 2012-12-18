;;;; zane-keys.el --- Keyboard shortcuts.
;;;;
;;;; http://www.masteringemacs.org/articles/2011/02/08/mastering-key-bindings-emacs/
;;;; http://www.gnu.org/software/emacs/elisp/html_node/Key-Binding-Conventions.html
;;;; http://code.google.com/p/ergoemacs/wiki/adoption
;
;(setq mac-command-modifier 'meta)
;(setq mac-option-modifier 'alt)
;
;;; hash table to store the old key bindings
;(setq old-key-bindings (make-hash-table :test 'equal))
;
;;; shows a hint about the change of the key binding
;(defun show-hint-old-kbind (key)
;  (let ((function-symbol (gethash key old-key-bindings)))
;    (beep)
;    (message "You typed: %s. For %s, use %s."
;             key
;             function-symbol
;             (mapcar 'key-description (where-is-internal function-symbol)))))
;
;;; turns off a key binding, leaving a hint for the unbound command
;(defmacro global-unset-key-leave-hint (key)
;  `(let ((function-symbol (global-key-binding ,key)))
;     (when function-symbol
;       (puthash ,key function-symbol old-key-bindings)
;       (global-set-key (kbd ,key) (lambda() (interactive) (show-hint-old-kbind ,key))))))
;
;(global-unset-key (kbd "C-x C-k"))
;
;(global-set-key (kbd "C-s") 'save-buffer)
;(global-set-key (kbd "C-w") 'close-current-buffer)
;
;(global-set-key (kbd "C-c C-a") 'align-regexp)
;;(global-set-key (kbd "C-c C-o") 'sort-lines)
;(global-set-key (kbd "C-x y") 'bury-buffer)
;(if (z:mac-p) (global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)) ; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
;;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
;
;;; Tab key
;;; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
;(keyboard-translate ?\C-i ?\H-i)
;
;;; Unset M-SPC because it's used by Alfred.app
;(global-unset-key (kbd "M-SPC"))
;(global-unset-key (kbd "M-TAB"))
;
;(global-unset-key (kbd "C-x RET"))
;(add-hook 'term-exec-hook
;          (lambda ()
;            (set-buffer-process-coding-system 'utf-8-unix
;                                              'utf-8-unix)))
;
;;; Occur
;;; http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
;(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(provide 'my-keybindings)
