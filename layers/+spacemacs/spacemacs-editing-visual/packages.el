;;; packages.el --- Spacemacs Editing Visual Layer packages File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(setq spacemacs-editing-visual-packages
      '(
        ;; default
        (hide-comnt :location local)
        ;; see https://github.com/syl20bnr/spacemacs/issues/2529
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

(defun spacemacs-editing-visual/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t
    :init
    (spacemacs|define-transient-state centered-buffer-mode
      :title "Centered Buffer Transient State"
      :bindings
      ("[" writeroom-decrease-width "shrink")
      ("]" writeroom-increase-width "enlarge")
      ("=" writeroom-adjust-width "adjust width"))))

(defun spacemacs-editing-visual/init-column-enforce-mode ()
  (use-package column-enforce-mode
    :commands (column-enforce-mode global-column-enforce-mode)
    :init
    (progn
      (spacemacs|add-toggle highlight-long-lines
        :status column-enforce-mode
        :prefix columns
        :on (column-enforce-n (or columns column-enforce-column))
        :on-message (format "long-lines enabled for %s columns."
                            (or columns column-enforce-column))
        :off (column-enforce-mode -1)
        :documentation "Highlight the characters past the 80th column."
        :evil-leader "t8")
      (spacemacs|add-toggle highlight-long-lines-globally
        :mode global-column-enforce-mode
        :documentation "Globally Highlight the characters past the 80th column."
        :evil-leader "t C-8"))
    :config (spacemacs|diminish column-enforce-mode "⑧" "8")))

(defun spacemacs-editing-visual/init-hide-comnt ()
  (use-package hide-comnt
    :commands hide/show-comments-toggle
    :init (spacemacs/set-leader-keys "ch" 'hide/show-comments-toggle)))

(defun spacemacs-editing-visual/init-highlight-indentation ()
  (use-package highlight-indentation
    :defer t
    :init
    (progn
      (spacemacs|add-toggle highlight-indentation
        :mode highlight-indentation-mode
        :documentation "Highlight indentation levels."
        :evil-leader "thi")
      (spacemacs|add-toggle highlight-indentation-current-column
        :mode highlight-indentation-current-column-mode
        :documentation "Highlight indentation level at point."
        :evil-leader "thc"))
    :config
    (progn
      (spacemacs|diminish highlight-indentation-mode " ⓗi" " hi")
      (spacemacs|diminish
       highlight-indentation-current-column-mode " ⓗc" " hc"))))

(defun spacemacs-editing-visual/init-highlight-numbers ()
  (use-package highlight-numbers
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-numbers-mode)
      (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1))))))

(defun spacemacs-editing-visual/init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :init
    (progn
      (when (member dotspacemacs-highlight-delimiters '(all current))
        (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
      (setq hl-paren-delay 0.2)
      (spacemacs/set-leader-keys "thp" 'highlight-parentheses-mode)
      (setq hl-paren-colors '("Springgreen3"
                              "IndianRed1"
                              "IndianRed3"
                              "IndianRed4")))
    :config
    (spacemacs|hide-lighter highlight-parentheses-mode)
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)))

(defun spacemacs-editing-visual/init-hl-anything ()
  (use-package hl-anything
    :init
    (progn
      (hl-highlight-mode)
      (setq-default hl-highlight-save-file
                    (concat spacemacs-cache-directory ".hl-save"))
      (spacemacs/set-leader-keys
        "hc"  'hl-unhighlight-all-local
        "hC"  'hl-unhighlight-all-global
        "hh"  'hl-highlight-thingatpt-local
        "hH"  'hl-highlight-thingatpt-global
        "hn"  'hl-find-next-thing
        "hN"  'hl-find-prev-thing
        "hr"  'hl-restore-highlights
        "hs"  'hl-save-highlights))
    :config (spacemacs|hide-lighter hl-highlight-mode)))

(defun spacemacs-editing-visual/init-indent-guide ()
  (use-package indent-guide
    :defer t
    :init
    (progn
      (setq indent-guide-delay 0.3)
      (spacemacs|add-toggle indent-guide
        :mode indent-guide-mode
        ;; :documentation (concat "Highlight indentation level at point."
        ;;                        " (alternative to highlight-indentation).")
        :documentation "Highlight indentation level at point. (alternative to highlight-indentation)."
        :evil-leader "ti")
      (spacemacs|add-toggle indent-guide-globally
        :mode indent-guide-global-mode
        ;; :documentation (concat "Highlight indentation level at point globally."
        ;;                        " (alternative to highlight-indentation).")
        :documentation "Highlight indentation level at point globally. (alternative to highlight-indentation)."
        :evil-leader "t TAB"))
    :config
    (spacemacs|diminish indent-guide-mode " ⓘ" " i")))

(defun spacemacs-editing-visual/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "tCd" 'rainbow-delimiters-mode)
      (when (member dotspacemacs-highlight-delimiters '(any all))
        (spacemacs/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook))))))

(defun spacemacs-editing-visual/init-volatile-highlights ()
  (use-package volatile-highlights
    :defer (spacemacs/defer 2)
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
      (spacemacs|hide-lighter volatile-highlights-mode))))
