;;; packages.el --- Source Control Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq version-control-packages
      '(
        browse-at-remote
        (diff-hl            :toggle (eq 'diff-hl version-control-diff-tool))
        diff-mode
        evil-unimpaired
        (git-gutter         :toggle (eq 'git-gutter version-control-diff-tool))
        (git-gutter-fringe  :toggle (eq 'git-gutter version-control-diff-tool))
        (git-gutter+        :toggle (eq 'git-gutter+ version-control-diff-tool))
        (git-gutter-fringe+ :toggle (eq 'git-gutter+ version-control-diff-tool))
        (smerge-mode :location built-in)
        (vc :location built-in)
        ))

(defun version-control/init-vc ()
  (use-package vc
    :defer t
    :commands (vc-ignore)
    :init
    (progn
      (spacemacs/declare-prefix "gv" "version-control")
      (spacemacs/set-leader-keys
        "gvv" 'vc-next-action
        "gvg" 'vc-annotate
        "gvD" 'vc-root-diff
        "gve" 'vc-ediff
        "gvd" 'vc-dir
        "gv+" 'vc-update
        "gvi" 'vc-register
        "gvI" 'vc-ignore
        "gvu" 'vc-revert
        "gvl" 'vc-print-log
        "gvL" 'vc-print-root-log
        "gvr" 'vc-resolve-conflicts))
    :config
    (progn
      (evilified-state-evilify vc-dir-mode vc-dir-mode-map
        "j" 'vc-dir-next-line
        (kbd "M-n") 'vc-dir-next-line
        "k" 'vc-dir-previous-line
        (kbd "M-p") 'vc-dir-previous-line
        "gj" 'vc-dir-next-directory
        (kbd "<tab>") 'vc-dir-next-directory
        "gk" 'vc-dir-previous-directory
        (kbd "<backtab>") 'vc-dir-previous-directory
        "l" 'vc-print-log
        "c" 'vc-next-action
        "a" 'vc-annotate
        "r" 'vc-dir-refresh
        "E" 'vc-dir-ignore)

      (evilified-state-evilify log-view-mode log-view-mode-map
        (kbd "M-n") 'log-view-msg-next
        (kbd "M-p") 'log-view-msg-prev
        (kbd "C-j") 'log-view-msg-next
        (kbd "C-k") 'log-view-msg-prev
        "J" 'log-view-file-next
        (kbd "<tab>") 'log-view-file-next
        "gj" 'log-view-file-next
        "K" 'log-view-file-prev
        "gk" 'log-view-file-prev
        (kbd "<backtab>") 'log-view-file-prev
        (kbd "<return>") 'log-view-find-revision
        "H" 'log-view-toggle-entry-display
        "o" 'ace-link-woman)
      (evilified-state-evilify vc-svn-log-view-mode vc-svn-log-view-mode-map)
      (evilified-state-evilify vc-git-log-view-mode vc-git-log-view-mode-map)
      (evilified-state-evilify vc-hg-log-view-mode vc-git-log-view-mode-map)
      (evilified-state-evilify vc-annotate-mode vc-annotate-mode-map
        "J" 'vc-annotate-next-revision
        "K" 'vc-annotate-prev-revision
        "L" 'vc-annotate-show-log-revision-at-line
        "H" 'vc-annotate-toggle-annotation-visibility
        "a" 'vc-annotate-revision-at-line
        "p" 'vc-annotate-revision-previous-to-line))))


(defun version-control/init-diff-mode ()
  (use-package diff-mode
    :defer t
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'diff-mode "mf" "format")
      (spacemacs/set-leader-keys-for-major-mode 'diff-mode
        "a" 'diff-apply-hunk
        "d" 'diff-hunk-kill
        "D" 'diff-file-kill
        "e" 'diff-ediff-patch
        "fc" 'diff-unified->context
        "fr" 'diff-reverse-direction
        "fu" 'diff-context->unified
        "g" 'diff-goto-source
        "j" 'diff-hunk-next
        "J" 'diff-file-next
        "k" 'diff-hunk-prev
        "K" 'diff-file-prev
        "r" 'spacemacs/diff-mode-revert-hunk
        "s" 'diff-split-hunk
        "u" 'diff-undo
        "q" 'quit-window)
      (spacemacs|define-transient-state diff-mode
        :title "Diff-mode Transient State"
        :evil-leader-for-mode (diff-mode . ".")
        :bindings
        ("j" diff-hunk-next "next hunk")
        ("J" diff-file-next "next file")
        ("k" diff-hunk-prev "previous hunk")
        ("K" diff-file-prev "previous file")
        ("q" nil "quit" :exit t)
        ("<escape>" nil nil :exit t)))))

(defun version-control/init-diff-hl ()
  (use-package diff-hl
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "gv=" 'diff-hl-diff-goto-hunk)
      (if version-control-global-margin
          (progn
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
            (run-with-idle-timer 1 nil 'global-diff-hl-mode))
        (run-with-idle-timer 1 nil 'diff-hl-margin-mode)))
    :config
    (progn
      (spacemacs|do-after-display-system-init
       (setq diff-hl-side (if (eq version-control-diff-side 'left)
                              'left 'right))))))

(defun version-control/post-init-evil-unimpaired ()
  (define-key evil-normal-state-map (kbd "[ h") 'spacemacs/vcs-previous-hunk)
  (define-key evil-normal-state-map (kbd "] h") 'spacemacs/vcs-next-hunk))

(defun version-control/init-git-gutter ()
  (use-package git-gutter
    :defer t
    :init
    (progn
      ;; If you enable global minor mode
      (when version-control-global-margin
        (run-with-idle-timer 1 nil 'global-git-gutter-mode))
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
    :defer t
    :init
    (progn
      (spacemacs|do-after-display-system-init
       (with-eval-after-load 'git-gutter
         (require 'git-gutter-fringe)))
      (setq git-gutter-fr:side (if (eq version-control-diff-side 'left)
                                   'left-fringe 'right-fringe)))
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
    :if (eq version-control-diff-tool 'git-gutter+)
    :defer t
    :init
    (progn
      ;; If you enable global minor mode
      (when version-control-global-margin
        (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh)
        (run-with-idle-timer 1 nil 'global-git-gutter+-mode))
      (setq
       git-gutter+-modified-sign " "
       git-gutter+-added-sign "+"
       git-gutter+-deleted-sign "-"
       git-gutter+-diff-option "-w"
       git-gutter+-hide-gutter t))
    ;; identify magit changes
    :config
    (spacemacs|hide-lighter git-gutter+-mode)
    ))

(defun version-control/init-git-gutter-fringe+ ()
  (use-package git-gutter-fringe+
    :defer t
    :init
    (progn
      (spacemacs|do-after-display-system-init
       (with-eval-after-load 'git-gutter+
         (require 'git-gutter-fringe+)))
      (setq git-gutter-fr+-side (if (eq version-control-diff-side 'left)
                                    'left-fringe 'right-fringe)))
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


(defun version-control/init-smerge-mode ()
  (use-package smerge-mode
    :defer t
    :diminish smerge-mode
    :commands spacemacs/smerge-transient-state/body
    :init
    (progn
      (spacemacs/set-leader-keys
        "gr" 'spacemacs/smerge-transient-state/body)
      (spacemacs|transient-state-format-hint smerge
        spacemacs--smerge-ts-full-hint
        "\n
 Movement^^^^         Merge Action^^      Diff^^            Other
 ---------------^^^^  ----------------^^  --------------^^  ---------------------------^^
 [_n_]^^   next hunk  [_b_] keep base     [_<_] base/mine   [_C_] combine curr/next hunks
 [_N_/_p_] prev hunk  [_m_] keep mine     [_=_] mine/other  [_u_] undo
 [_j_]^^   next line  [_a_] keep all      [_>_] base/other  [_q_] quit
 [_k_]^^   prev line  [_o_] keep other    [_r_] refine
 ^^^^                 [_c_] keep current  [_e_] ediff       [_?_]^^ toggle help
 ^^^^                 [_K_] kill current")
      (spacemacs|define-transient-state smerge
        :title "Smerge Transient State"
        :hint-is-doc t
        :dynamic-hint (spacemacs//smerge-ts-hint)
        :bindings
        ;; move
        ("n" smerge-next)
        ("N" smerge-prev)
        ("p" smerge-prev)
        ("j" evil-next-line)
        ("k" evil-previous-line)
        ;; merge action
        ("b" smerge-keep-base)
        ("m" smerge-keep-mine)
        ("a" smerge-keep-all)
        ("o" smerge-keep-other)
        ("c" smerge-keep-current)
        ;; diff
        ("<" smerge-diff-base-mine)
        ("=" smerge-diff-mine-other)
        (">" smerge-diff-base-other)
        ("r" smerge-refine)
        ("e" smerge-ediff :exit t)
        ;; other
        ("C" smerge-combine-with-next)
        ("K" smerge-kill-current)
        ("u" undo-tree-undo)
        ("q" nil :exit t)
        ("?" spacemacs//smerge-ts-toggle-hint)))))

(defun version-control/init-browse-at-remote ()
  (use-package browse-at-remote
    :defer t
    :init (spacemacs/set-leader-keys "go" 'browse-at-remote)))
