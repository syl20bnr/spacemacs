;;; packages.el --- Spacemacs Editing Visual Layer packages File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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

;;; Commentary:

;;; Code:

(defconst spacemacs-editing-visual-packages
  '(
    column-enforce-mode
    (hide-comnt :location (recipe :fetcher github :repo "emacsmirror/hide-comnt"))
    highlight-indentation
    highlight-numbers
    highlight-parentheses
    indent-guide
    rainbow-delimiters
    (term-cursor :location (recipe :fetcher github :repo "h0d/term-cursor.el"))
    volatile-highlights
    writeroom-mode))


(defun spacemacs-editing-visual/init-column-enforce-mode ()
  (use-package column-enforce-mode
    :init
    (spacemacs|add-toggle highlight-long-lines
      :status column-enforce-mode
      :prefix columns
      :on (column-enforce-n (or columns column-enforce-column))
      :on-message (format "long-lines enabled for %s columns."
                          (or columns column-enforce-column))
      :off (column-enforce-mode -1)
      :off-message (format "long-lines disabled for %s columns."
                           (or columns column-enforce-column))
      :documentation "Highlight the characters past the 80th column."
      :evil-leader "t8")
    (spacemacs|add-toggle highlight-long-lines-globally
      :mode global-column-enforce-mode
      :documentation "Globally highlight the characters past the 80th column."
      :evil-leader "t C-8")
    :spacediminish ("⑧" "8")))

(defun spacemacs-editing-visual/init-hide-comnt ()
  (use-package hide-comnt
    :commands hide/show-comments-toggle
    :init
    (advice-add 'hide/show-comments
                :after (lambda (&optional hide/show start end)
                         (pcase hide/show
                           ('hide (message "Hide comments enabled."))
                           ('show (message "Hide comments disabled.")))))
    (spacemacs/set-leader-keys "ch" 'hide/show-comments-toggle)))

(defun spacemacs-editing-visual/init-highlight-indentation ()
  (use-package highlight-indentation
    :defer t
    :init
    (spacemacs|add-toggle highlight-indentation
      :mode highlight-indentation-mode
      :documentation "Highlight indentation levels."
      :evil-leader "thi")
    (spacemacs|add-toggle highlight-indentation-current-column
      :mode highlight-indentation-current-column-mode
      :documentation "Highlight indentation level at point."
      :evil-leader "thc")
    :spacediminish ((" ⓗi" " hi")
                    (highlight-indentation-current-column-mode " ⓗc" " hc"))))

(defun spacemacs-editing-visual/init-highlight-numbers ()
  (use-package highlight-numbers
    :defer t
    :init
    (spacemacs|add-toggle highlight-numbers
      :mode highlight-numbers-mode
      :documentation "Highlight numeric literals."
      :evil-leader "thn")
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)
    (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1)))))

(defun spacemacs-editing-visual/init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :custom
    (highlight-parentheses-delay 0.2)
    (highlight-parentheses-colors '("Springgreen3"
                                    "IndianRed1"
                                    "IndianRed3"
                                    "IndianRed4"))
    :custom-face (highlight-parentheses-highlight ((nil (:weight ultra-bold))))
    :commands highlight-parentheses-minibuffer-setup
    :init
    (spacemacs|add-toggle highlight-parentheses
      :mode highlight-parentheses-mode
      :documentation "Highlight surrounding parentheses."
      :evil-leader "thp")
    (spacemacs|add-toggle highlight-parentheses-globally
      :mode global-highlight-parentheses-mode
      :documentation "Globally highlight surrounding parentheses."
      :evil-leader "thP")
    (when (memq dotspacemacs-highlight-delimiters '(all current))
      (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
    (when (eq dotspacemacs-highlight-delimiters 'all)
      (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup))
    :config (spacemacs|hide-lighter highlight-parentheses-mode)))

(defun spacemacs-editing-visual/init-indent-guide ()
  (use-package indent-guide
    :defer t
    :custom
    (indent-guide-delay 0.3)
    :init
    (spacemacs|add-toggle indent-guide
      :mode indent-guide-mode
      :documentation "Highlight indentation level at point. (alternative to highlight-indentation)."
      :evil-leader "ti")
    (spacemacs|add-toggle indent-guide-globally
      :mode indent-guide-global-mode
      :documentation "Highlight indentation level at point globally. (alternative to highlight-indentation)."
      :evil-leader "t TAB")
    :spacediminish (" ⓘ" " i")))

(defun spacemacs-editing-visual/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (spacemacs|add-toggle rainbow-delimiters
      :mode rainbow-delimiters-mode
      :documentation "Highlight nested parentheses, brackets, and braces according to their depth."
      :evil-leader "tCd")
    (when (memq dotspacemacs-highlight-delimiters '(any all))
      (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))))

(defun spacemacs-editing-visual/init-term-cursor ()
  (use-package term-cursor
    :defer t
    :custom (term-cursor-triggers '(blink-cursor-mode-hook))
    :init
    (unless (or (daemonp) (display-graphic-p))
      (global-term-cursor-mode))
    (when (or (daemonp) dotspacemacs-enable-server)
      (add-hook 'server-after-make-frame-hook 'spacemacs//maybe-enable-term-cursor))))

(defun spacemacs-editing-visual/init-volatile-highlights ()
  (use-package volatile-highlights
    :defer t
    :init
    (spacemacs|add-toggle volatile-highlights
      :mode volatile-highlights-mode
      :documentation "Display visual feedback for some operations."
      :evil-leader "thv")

    ;; volatile-highlights is redundant with built-in highlighting in occur.  In
    ;; Emacs 29, it starts to cause errors.  See
    ;; https://github.com/k-talo/volatile-highlights.el/issues/26
    (setq vhl/use-occur-extension-p (< emacs-major-version 28))

    (volatile-highlights-mode t)
    :config
    ;; additional extensions
    ;; evil
    (with-eval-after-load 'evil
      (vhl/define-extension 'evil
                            'evil-move
                            'evil-paste-after
                            'evil-paste-before
                            'evil-paste-pop)
      (vhl/install-extension 'evil)
      (vhl/load-extension 'evil))
    ;; undo-tree
    (with-eval-after-load 'undo-tree
      (vhl/define-extension 'undo-tree
                            'undo-tree-move
                            'undo-tree-yank)
      (vhl/install-extension 'undo-tree)
      (vhl/load-extension 'undo-tree))
    (spacemacs|hide-lighter volatile-highlights-mode)))

(defun spacemacs-editing-visual/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t
    :custom (writeroom-mode-line-toggle-position 'mode-line-format)
    :init
    (spacemacs|add-toggle centered-buffer
      :status writeroom-mode
      :on (let ((writeroom-maximize-window nil)
                (writeroom-mode-line t))
            (writeroom-mode 1))
      :on-message "Centered-buffer is enabled."
      :off (writeroom-mode -1)
      :off-message "Centered-buffer is disabled."
      :documentation "Centerize current buffer."
      :evil-leader "wcc")
    (spacemacs|add-toggle distraction-free
      :status writeroom-mode
      :on (let ((writeroom-maximize-window t)
                (writeroom-mode-line nil))
            (writeroom-mode 1))
      :on-message "Distraction-free is enabled."
      :off (writeroom-mode -1)
      :off-message "Distraction-free is disabled."
      :documentation "Centerize and maximize current buffer."
      :evil-leader "wcC")
    (spacemacs/set-leader-keys "wc." #'spacemacs/centered-buffer-transient-state)
    (spacemacs|define-transient-state centered-buffer
      :title "Centered Buffer Transient State"
      :bindings
      ("m" writeroom-toggle-mode-line "modeline")
      ("[" writeroom-decrease-width   "shrink")
      ("]" writeroom-increase-width   "enlarge")
      ("=" writeroom-adjust-width     "adjust width"))))

;;; packages.el ends here
