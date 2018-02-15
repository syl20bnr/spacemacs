;;; config.el --- OSX Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (spacemacs/system-is-mac)
  (spacemacs/set-leader-keys "bf" 'reveal-in-osx-finder)

  ;; this is only applicable to GUI mode
  (when (display-graphic-p)

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

    ;; Backwards compatibility
    (case osx-use-option-as-meta
      ('nil (setf osx-option-as 'none))
      (deprecated nil)
      (t (setf osx-option-as 'meta)))

    ;; Set internal variables according to the given config variables
    (cl-loop for (key-var . internal-var) in modifier-keys do
             (let ((key-value (symbol-value key-var)))
               (when (member key-value allowed-values)
                 (setf (symbol-value internal-var) key-value))))

    (defun kbd-mac-command (keys)
      "Wraps `kbd' function with Mac OSX compatible Command-key (⌘).
KEYS should be a string such as \"f\" which will be turned into values
such as \"H-f\", \"s-f\", or \"A-f\" depending on the value of
`mac-commmand-modifier' which could be `hyper', `super', or `alt'.
KEYS with a string of \"C-f\" are also valid and will be turned into
values such as \"H-C-f\".
Returns nil if `mac-command-modifier' is set to `none' or something
other than the three sane values listed above."
      (let ((found (assoc mac-command-modifier
                          '((hyper . "H-")
                            (super . "s-")
                            (alt   . "A-")))))
        (when found (kbd (concat (cdr found) keys)))))

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
    (global-set-key (kbd-mac-command "s")
                    (lambda ()
                      (interactive)
                      (call-interactively (key-binding "\C-x\C-s"))))

    ;; window manipulation with command key
    (global-set-key (kbd-mac-command "1") 'winum-select-window-1)
    (global-set-key (kbd-mac-command "2") 'winum-select-window-2)
    (global-set-key (kbd-mac-command "3") 'winum-select-window-3)
    (global-set-key (kbd-mac-command "4") 'winum-select-window-4)
    (global-set-key (kbd-mac-command "5") 'winum-select-window-5)
    (global-set-key (kbd-mac-command "6") 'winum-select-window-6)
    (global-set-key (kbd-mac-command "7") 'winum-select-window-7)
    (global-set-key (kbd-mac-command "8") 'winum-select-window-8)
    (global-set-key (kbd-mac-command "9") 'winum-select-window-9)

    (global-set-key (kbd-mac-command "Z") 'undo-tree-redo)
    (global-set-key (kbd-mac-command "C-f") 'spacemacs/toggle-frame-fullscreen)
    (global-set-key (kbd "M-s-h") 'ns-do-hide-others)

    ;; Emacs sometimes registers C-s-f as this weird keycode
    ;; (global-set-key (kbd "<C-s-268632070>") 'spacemacs/toggle-frame-fullscreen)
    ;; (global-set-key [142607065] 'ns-do-hide-others)
    ))
