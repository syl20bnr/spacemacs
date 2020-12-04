;;; packages.el --- Space-macs Editing Visual Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-editing-visual-packages
      '(
        ;; default
        (hide-comnt :location local)
        ;; see https://github.com/syl20bnr/space-macs/issues/2529
        ;; waiting for an overlay bug to be fixed
        (hl-anything :excluded t)
        column-enforce-mode
        highlight-indentation
        highlight-numbers
        highlight-parentheses
        indent-guide
        rainbow-delimiters
        volatile-highlights
        writeroom-mode
        ))

;; Initialization of packages

(defun space-macs-editing-visual/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t
    :init
    (space-macs|define-transient-state centered-buffer-mode
      :title "Centered Buffer Transient State"
      :bindings
      ("[" writeroom-decrease-width "shrink")
      ("]" writeroom-increase-width "enlarge")
      ("=" writeroom-adjust-width "adjust width"))))

(defun space-macs-editing-visual/init-column-enforce-mode ()
  (use-package column-enforce-mode
    :commands (column-enforce-mode global-column-enforce-mode)
    :init
    (progn
      (space-macs|add-toggle highlight-long-lines
        :status column-enforce-mode
        :prefix columns
        :on (column-enforce-n (or columns column-enforce-column))
        :on-message (format "long-lines enabled for %s columns."
                            (or columns column-enforce-column))
        :off (column-enforce-mode -1)
        :documentation "Highlight the characters past the 80th column."
        :evil-leader "t8")
      (space-macs|add-toggle highlight-long-lines-globally
        :mode global-column-enforce-mode
        :documentation "Globally Highlight the characters past the 80th column."
        :evil-leader "t C-8"))
    :config (space-macs|diminish column-enforce-mode "â‘§" "8")))

(defun space-macs-editing-visual/init-hide-comnt ()
  (use-package hide-comnt
    :commands hide/show-comments-toggle
    :init (space-macs/set-leader-keys "ch" 'hide/show-comments-toggle)))

(defun space-macs-editing-visual/init-highlight-indentation ()
  (use-package highlight-indentation
    :defer t
    :init
    (progn
      (space-macs|add-toggle highlight-indentation
        :mode highlight-indentation-mode
        :documentation "Highlight indentation levels."
        :evil-leader "thi")
      (space-macs|add-toggle highlight-indentation-current-column
        :mode highlight-indentation-current-column-mode
        :documentation "Highlight indentation level at point."
        :evil-leader "thc"))
    :config
    (progn
      (space-macs|diminish highlight-indentation-mode " â“—i" " hi")
      (space-macs|diminish
       highlight-indentation-current-column-mode " â“—c" " hc"))))

(defun space-macs-editing-visual/init-highlight-numbers ()
  (use-package highlight-numbers
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-numbers-mode)
      (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1))))))

(defun space-macs-editing-visual/init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :init
    (progn
      (when (member dotspace-macs-highlight-delimiters '(all current))
        (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
      (setq hl-paren-delay 0.2)
      (space-macs/set-leader-keys "thp" 'highlight-parentheses-mode)
      (setq hl-paren-colors '("Springgreen3"
                              "IndianRed1"
                              "IndianRed3"
                              "IndianRed4")))
    :config
    (space-macs|hide-lighter highlight-parentheses-mode)
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)))

(defun space-macs-editing-visual/init-hl-anything ()
  (use-package hl-anything
    :init
    (progn
      (hl-highlight-mode)
      (setq-default hl-highlight-save-file
                    (concat space-macs-cache-directory ".hl-save"))
      (space-macs/set-leader-keys
        "hc"  'hl-unhighlight-all-local
        "hC"  'hl-unhighlight-all-global
        "hh"  'hl-highlight-thingatpt-local
        "hH"  'hl-highlight-thingatpt-global
        "hn"  'hl-find-next-thing
        "hN"  'hl-find-prev-thing
        "hr"  'hl-restore-highlights
        "hs"  'hl-save-highlights))
    :config (space-macs|hide-lighter hl-highlight-mode)))

(defun space-macs-editing-visual/init-indent-guide ()
  (use-package indent-guide
    :defer t
    :init
    (progn
      (setq indent-guide-delay 0.3)
      (space-macs|add-toggle indent-guide
        :mode indent-guide-mode
        ;; :documentation (concat "Highlight indentation level at point."
        ;;                        " (alternative to highlight-indentation).")
        :documentation "Highlight indentation level at point. (alternative to highlight-indentation)."
        :evil-leader "ti")
      (space-macs|add-toggle indent-guide-globally
        :mode indent-guide-global-mode
        ;; :documentation (concat "Highlight indentation level at point globally."
        ;;                        " (alternative to highlight-indentation).")
        :documentation "Highlight indentation level at point globally. (alternative to highlight-indentation)."
        :evil-leader "t TAB"))
    :config
    (space-macs|diminish indent-guide-mode " â“˜" " i")))

(defun space-macs-editing-visual/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (space-macs/set-leader-keys "tCd" 'rainbow-delimiters-mode)
      (when (member dotspace-macs-highlight-delimiters '(any all))
        (space-macs/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook))))))

(defun space-macs-editing-visual/init-volatile-highlights ()
  (use-package volatile-highlights
    :defer (space-macs/defer 2)
    :config
    (progn
      (require 'volatile-highlights)
      ;; additional extensions
      ;; evil
      (vhl/define-extension 'evil
                            'evil-move
                            'evil-paste-after
                            'evil-paste-before
                            'evil-paste-pop)
      (with-eval-after-load 'evil
        (vhl/install-extension 'evil)
        (vhl/load-extension 'evil))
      ;; undo-tree
      (vhl/define-extension 'undo-tree
                            'undo-tree-move
                            'undo-tree-yank)
      (with-eval-after-load 'undo-tree
        (vhl/install-extension 'undo-tree)
        (vhl/load-extension 'undo-tree))
      (volatile-highlights-mode)
      (space-macs|hide-lighter volatile-highlights-mode))))


