(use-package git-gutter-fringe
  :init
  (global-git-gutter-mode t)
  :config
  (progn
    (setq git-gutter:hide-gutter t)
    ;; Don't need log/message.
    (setq git-gutter:verbosity 0)
    (setq git-gutter-fr:side 'right-fringe)
    ;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
    ;; custom graphics that works nice with half-width fringes
    (fringe-helper-define 'git-gutter-fr:added nil
      "..X...."
      "..X...."
      "XXXXX.."
      "..X...."
      "..X...."
      )
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "......."
      "......."
      "XXXXX.."
      "......."
      "......."
      )
    (fringe-helper-define 'git-gutter-fr:modified nil
      "..X...."
      ".XXX..."
      "XXXXX.."
      ".XXX..."
      "..X...."
      )))

