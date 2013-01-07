(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'completion-styles 'initials t)
(add-to-list 'ac-sources 'ac-source-semantic)
(semantic-mode t)

;; customization
(setq ac-auto-start 2
      ac-delay 0.
      ac-quick-help-delay 1.
      ac-use-fuzzy t
      ac-fuzzy-enable t
      tab-always-indent 'complete ; use 'complete when auto-complete is disabled
      ac-dwim t)

;;; list of modes where ac should be available
(dolist (mode '(emacs-lisp-mode
                java-mode
                org-mode
                python-mode))
  (add-to-list 'ac-modes mode))

;; solarized
(custom-set-faces
 '(ac-candidate-face ((t (:background "#586e75" :foreground "#000000"))))
 '(ac-candidate-mouse-face ((t (:background "#073642" :foreground "#dc322f" :box (:line-width -1 :style pressed-button)))))
 '(ac-selection-face ((t (:background "#073642" :foreground "#b58900" :box (:line-width -1 :style pressed-button) :overline "black" :weight bold)))))
