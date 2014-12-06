(defvar iedit-post-extensions '(evil-iedit-state))

(defun iedit/init-evil-iedit-state ()
  (spacemacs/defface-state-color 'iedit "IndianRed1")
  (spacemacs/defface-state-color 'iedit-insert "IndianRed1")
  (defun iedit//lazy-load ()
    (require 'evil-iedit-state)
    (setq evil-iedit-state-cursor `(,(spacemacs/state-color 'iedit) box))   
    (setq evil-iedit-insert-state-cursor `((spacemacs/state-color 'iedit-insert) (bar . 2)))
    (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
    (when (and (boundp 'evil-escape-mode)
               (symbol-value evil-escape-mode))
      (key-chord-define evil-iedit-state-map
                        evil-escape-key-sequence
                        'evil-iedit-state/quit-iedit-mode)
      (key-chord-define evil-iedit-insert-state-map
                        evil-escape-key-sequence
                        'evil-iedit-state/quit-iedit-mode)))
  ;; override the basic edit mode from ahs everywhere
  (eval-after-load 'auto-highlight-symbol
    '(progn
       (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
       (defalias 'ahs-edit-mode 'evil-iedit-state/iedit-mode)))
  (add-to-hooks 'iedit//lazy-load '(prog-mode-hook markdown-mode-hook)))


  
