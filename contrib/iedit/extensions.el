(defvar iedit-post-extensions '(evil-iedit-state))

(defun iedit/init-evil-iedit-state ()
  (spacemacs/defface-state-color 'iedit "IndianRed1")
  (spacemacs/defface-state-color 'iedit-insert "IndianRed1")
  (defun iedit//lazy-load ()
    (require 'evil-iedit-state)
    (setq evil-iedit-state-cursor `(,(spacemacs/state-color 'iedit) box))   
    (setq evil-iedit-insert-state-cursor `((spacemacs/state-color 'iedit-insert) (bar . 2)))
    (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
    ;; override the basic edit mode from ahs if required
    (eval-after-load 'auto-highlight-symbol
      '(defalias 'ahs-edit-mode 'evil-iedit-state/iedit-mode)))
    ;; add 'e' action to expand-region
  (add-to-hooks 'iedit//lazy-load '(prog-mode-hook markdown-mode-hook)))


  
