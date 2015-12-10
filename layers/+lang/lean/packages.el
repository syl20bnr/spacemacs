(setq dotspacemacs-additional-packages
      (append '(dash dash-functional f s
                     fill-column-indicator
                     mmm-mode)))

(setq lean-packages '((lean-mode :location (recipe
                                            :fetcher github
                                            :repo leanprover/lean
                                            :files ("src/emacs/*.el")))))

(defun lean/init-lean-mode ()
  (use-package dash)
  (use-package dash-functional)
  (use-package f)
  (use-package s)
  (use-package fill-column-indicator)
  (use-package mmm-mode
    :defer t)
  (use-package lean-mode
    :defer t
    :config
    (evil-leader/set-key-for-mode 'lean-mode
      "L" 'lean-std-exe
      "R" 'lean-server-restart-process
      "d" 'lean-eldoc-documentation-function
      "f" 'lean-fill-placeholder
      "gu" 'lean-generate-tags
      "gg" 'lean-find-tag
      "o" 'lean-set-option
      "c" 'lean-eval-cmd
      "," 'lean-show-goal-at-pos
      "k" 'quail-show-key
      "i" 'lean-show-id-keyword-info
      "t" 'lean-show-type)))
