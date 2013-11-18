;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode '(progn ,@body)))

;; Works only with selections from top to bottom
(fset 'mac-mc-edit-beginnings-of-lines-tb
   (lambda (&optional arg)
    "Edit beginnings of lines using mulitple cursors with Evil."
     (interactive "p")
     (kmacro-exec-ring-item (quote ("k\215bi" 0 "%d")) arg)))
;; Works only with selections from top to bottom
(fset 'mac-mc-edit-ends-of-lines-tb
   (lambda (&optional arg)
     "Edit ends of lines using mulitple cursors with Evil."
     (interactive "p")
     (kmacro-exec-ring-item (quote ("k\215ei" 0 "%d")) arg)))
;; Works only with selections from bottom to top
(fset 'mac-mc-edit-beginnings-of-lines-bt
   (lambda (&optional arg)
    "Edit beginnings of lines using mulitple cursors with Evil."
     (interactive "p")
     (kmacro-exec-ring-item (quote ("\215bi" 0 "%d")) arg)))
;; Works only with selections from bottom to top
(fset 'mac-mc-edit-ends-of-lines-bt
   (lambda (&optional arg)
     "Edit ends of lines using mulitple cursors with Evil."
     (interactive "p")
     (kmacro-exec-ring-item (quote ("\215ei" 0 "%d")) arg)))
;; Acquire cursors at the beginning of all selected regions
(fset 'mac-mc-acquire-cursors-at-beginning
   (lambda (&optional arg)
     "Transform a multiple selection into cursors at the beginning of each selection."
     (interactive "p")
     (kmacro-exec-ring-item (quote ([73 105 escape 104 120] 0 "%d")) arg)))


(provide 'my-macros)
