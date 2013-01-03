;; Works only with selections from top to bottom
(fset 'macro-mc-edit-beginnings-of-lines
  (lambda (&optional arg)
    "Edit beginnings of lines using mulitple cursors with Evil."
    (interactive "p") (kmacro-exec-ring-item (quote ("k\215\215i" 0 "%d")) arg)))

;; Works only with selections from top to bottom
(fset 'macro-mc-edit-ends-of-lines
   (lambda (&optional arg)
     "Edit ends of lines using mulitple cursors with Evil."
     (interactive "p") (kmacro-exec-ring-item
                        (quote ([107 134217741 167772173 105] 0 "%d")) arg)))

(provide 'macros)
