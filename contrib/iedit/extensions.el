(defvar iedit-post-extensions '(evil-iedit-state))

(defun iedit/init-evil-iedit-state ()
  (spacemacs/defface-state-color 'iedit "firebrick1")
  (spacemacs/defface-state-color 'iedit-insert "firebrick1")

  (defun iedit//lazy-load ()
    (require 'evil-iedit-state)
    (setq evil-iedit-state-cursor `(,(spacemacs/state-color 'iedit) box))   
    (setq evil-iedit-insert-state-cursor `((spacemacs/state-color 'iedit-insert) (bar . 2)))
    (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map
      (kbd evil-leader/leader) evil-leader--default-map)
    (define-key evil-iedit-insert-state-map
      (kbd evil-leader/leader) evil-leader--default-map)
    ;; evil-escape support
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


  
