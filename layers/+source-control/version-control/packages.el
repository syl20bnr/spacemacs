;;; packages.el --- Source Control Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq version-control-packages
      '(
        diff-mode
        diff-hl
        git-gutter
        git-gutter+
        git-gutter-fringe
        git-gutter-fringe+
        ))

(defun version-control/init-diff-mode ()
  (use-package diff-mode
    :defer t
    :config
    (evilified-state-evilify diff-mode diff-mode-map
      "j" 'diff-hunk-next
      "k" 'diff-hunk-prev)))

(defun version-control/init-diff-hl ()
  (use-package diff-hl
    :init
    (progn
      (setq diff-hl-side 'right)
      (when (eq version-control-diff-tool 'diff-hl)
        (when version-control-global-margin
          (global-diff-hl-mode))
        (unless (display-graphic-p)
          (setq diff-hl-side 'left)
          (diff-hl-margin-mode))))))

(defun version-control/init-git-gutter ()
  (use-package git-gutter
    :commands global-git-gutter-mode
    :init
    (progn
      ;; If you enable global minor mode
      (when (and (eq version-control-diff-tool 'git-gutter)
                 version-control-global-margin)
        (global-git-gutter-mode t))
      ;; If you would like to use git-gutter.el and linum-mode
      (if dotspacemacs-line-numbers
          (git-gutter:linum-setup))
      (setq git-gutter:update-interval 2
            git-gutter:modified-sign " "
            git-gutter:added-sign "+"
            git-gutter:deleted-sign "-"
            git-gutter:diff-option "-w"
            git-gutter:hide-gutter t
            git-gutter:ask-p nil
            git-gutter:verbosity 0
            git-gutter:handled-backends '(git hg bzr svn)
            git-gutter:hide-gutter t))
    :config
    (spacemacs|hide-lighter git-gutter-mode)))

(defun version-control/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :commands git-gutter-mode
    :init
    (progn
      (when (display-graphic-p)
        (with-eval-after-load 'git-gutter
          (require 'git-gutter-fringe)))
      (setq git-gutter-fr:side 'right-fringe))
    :config
    (progn
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
        "XX.XX.."
        ".XXX..."
        "..X...."
        ))))

(defun version-control/init-git-gutter+ ()
  (use-package git-gutter+
    :commands global-git-gutter+-mode
    :init
    (progn
      ;; If you enable global minor mode
      (when (and (eq version-control-diff-tool 'git-gutter+)
                 version-control-global-margin)
        (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh)
        (global-git-gutter+-mode t))
      (setq
       git-gutter+-modified-sign " "
       git-gutter+-added-sign "+"
       git-gutter+-deleted-sign "-"
       git-gutter+-diff-option "-w"
       git-gutter+-hide-gutter t))
    ;; identify magit changes
    :config
    (spacemacs|hide-lighter git-gutter+-mode)
    ;; (set-face-foreground 'git-gutter+-modified "black")
    ;; (set-face-foreground 'git-gutter+-added    "black")
    ;; (set-face-foreground 'git-gutter+-deleted  "black")
    ;; (set-face-background 'git-gutter+-modified "orange1")
    ;; (set-face-background 'git-gutter+-added    "green4")
    ;; (set-face-background 'git-gutter+-deleted  "red3")
    ))

(defun version-control/init-git-gutter-fringe+ ()
  (use-package git-gutter-fringe+
    :commands git-gutter+-mode
    :init
    (progn
      (when (display-graphic-p)
        (with-eval-after-load 'git-gutter+
          (require 'git-gutter-fringe+)))
      (setq git-gutter-fr+-side 'right-fringe))
    :config
    (progn
      ;; custom graphics that works nice with half-width fringes
      (fringe-helper-define 'git-gutter-fr+-added nil
        "..X...."
        "..X...."
        "XXXXX.."
        "..X...."
        "..X...."
        )
      (fringe-helper-define 'git-gutter-fr+-deleted nil
        "......."
        "......."
        "XXXXX.."
        "......."
        "......."
        )
      (fringe-helper-define 'git-gutter-fr+-modified nil
        "..X...."
        ".XXX..."
        "XX.XX.."
        ".XXX..."
        "..X...."
        ))))
