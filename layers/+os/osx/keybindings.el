;;; keybindings.el --- OSX Layer keybindings File for Spacemacs
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


(when (spacemacs/system-is-mac)
  (spacemacs/set-leader-keys "bf" 'reveal-in-osx-finder)

  ;; `Command' key is by default bound to HYPER (H-*),
  ;; `Option' key is by default bound to META (M-*).
  ;; `Function' key is by default not rebound.
  ;; `Control' key is by default not rebound.
  ;; The right variations of the above keys can
  ;; also be modified but are not rebound by
  ;; default.

  ;; `Alist' linking the layer config variables to
  ;; the internal Emacs variables for the modifier keys.
  (setq modifier-keys '((osx-command-as       . mac-command-modifier)
                        (osx-option-as        . mac-option-modifier)
                        (osx-function-as      . mac-function-modifier)
                        (osx-control-as       . mac-control-modifier)
                        (osx-right-command-as . mac-right-command-modifier)
                        (osx-right-option-as  . mac-right-option-modifier)
                        (osx-right-control-as . mac-right-control-modifier)))

  ;; The allowed non-nil values for the config variables.
  (setq allowed-values '(super meta hyper control alt none left))

  ;; Set internal variables according to the given config variables
  (cl-loop for (key-var . internal-var) in modifier-keys do
           (let ((key-value (symbol-value key-var)))
             (when (member key-value allowed-values)
               (setf (symbol-value internal-var) key-value))))

  (when osx-swap-option-and-command
    (cl-rotatef mac-command-modifier mac-option-modifier))

  (defun kbd-mac-command (keys)
    "Call `kbd' with a macOS-compatible Command-key (⌘) prefixed.
KEYS should be a string suitable as input to `kbd'.
`mac-commmand-modifier' determines which prefix will be added; it
should be set to one of `hyper', `super', or `alt'.  For example,
if KEYS is the string `f', it will be prefixed as `H-f', `s-f',
or `A-f' accordingly.  If KEYS is of the form `C-f', it likewise
will be prefixed as `H-C-f', `s-C-f', or `A-C-f'.

If `mac-command-modifier' is set to `none' or something other
than the three values listed above, `H-' will be used as the
default."
    (let ((found (assoc mac-command-modifier
                        '((hyper . "H-")
                          (super . "s-")
                          (alt   . "A-")))))
      (if found
          (kbd (concat (cdr found) keys))
        (kbd (concat "H-" keys)))))

  ;; Keybindings
  (global-set-key (kbd-mac-command "=") 'spacemacs/scale-up-font)
  (global-set-key (kbd-mac-command "-") 'spacemacs/scale-down-font)
  (global-set-key (kbd-mac-command "0") 'spacemacs/reset-font-size)
  (global-set-key (kbd-mac-command "q") 'save-buffers-kill-terminal)
  (global-set-key (kbd-mac-command "v") 'yank)
  (global-set-key (kbd-mac-command "c") 'evil-yank)
  (global-set-key (kbd-mac-command "a") 'mark-whole-buffer)
  (global-set-key (kbd-mac-command "x") 'kill-region)
  (global-set-key (kbd-mac-command "w") 'delete-window)
  (global-set-key (kbd-mac-command "W") 'delete-frame)
  (global-set-key (kbd-mac-command "n") 'make-frame)
  (global-set-key (kbd-mac-command "`") 'other-frame)
  (global-set-key (kbd-mac-command "z") 'undo-tree-undo)
  (global-set-key (kbd-mac-command "s") 'save-buffer)

  ;; window manipulation with command key
  (global-set-key (kbd-mac-command "1") 'spacemacs/winum-select-window-1)
  (global-set-key (kbd-mac-command "2") 'spacemacs/winum-select-window-2)
  (global-set-key (kbd-mac-command "3") 'spacemacs/winum-select-window-3)
  (global-set-key (kbd-mac-command "4") 'spacemacs/winum-select-window-4)
  (global-set-key (kbd-mac-command "5") 'spacemacs/winum-select-window-5)
  (global-set-key (kbd-mac-command "6") 'spacemacs/winum-select-window-6)
  (global-set-key (kbd-mac-command "7") 'spacemacs/winum-select-window-7)
  (global-set-key (kbd-mac-command "8") 'spacemacs/winum-select-window-8)
  (global-set-key (kbd-mac-command "9") 'spacemacs/winum-select-window-9)

  (global-set-key (kbd-mac-command "Z") 'undo-tree-redo)
  (global-set-key (kbd-mac-command "C-f") 'spacemacs/toggle-frame-fullscreen)
  (global-set-key (kbd "M-s-h") 'ns-do-hide-others)

  ;; Emacs sometimes registers C-s-f as this weird keycode
  ;; (global-set-key (kbd "<C-s-268632070>") 'spacemacs/toggle-frame-fullscreen)
  ;; (global-set-key [142607065] 'ns-do-hide-others)
  )
