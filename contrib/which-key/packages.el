(setq which-key-packages '(which-key))

(setq which-key-excluded-packages '(guide-key
                                    guide-key-tip))

(defun which-key/init-which-key ()
  (use-package which-key
    :init
    (progn
      (setq which-key-max-description-length 32)
      (which-key-mode)
      (spacemacs|add-toggle which-key
                            :status which-key-mode
                            :on (which-key-mode)
                            :off (which-key-mode -1)
                            :documentation
                            "Display a buffer with available key bindings."
                            :evil-leader "tK")
      (defadvice which-key--update
          (around spacemacs/inhibit-which-key-buffer activate)
        "Prevent the popup of the which-key buffer in some case."
        ;; a micro-state is running
        ;; or
        ;; bzg-big-fringe-mode is on
        (if (or overriding-terminal-local-map
                bzg-big-fringe-mode)
            (let ((which-key-inhibit t)) ad-do-it)
          ad-do-it))
      (add-to-list 'which-key-description-replacement-alist '("select-window-\\([0-9]\\)" . "Window \\1"))
      (which-key-add-key-based-replacements
       "SPC TAB"  "last buffer"
       "SPC SPC"  "ace word"
       "SPC !"    "shell cmd"
       "SPC '"    "open shell"
       "SPC /"    "smart search"
       "SPC ?"    "show keybindings"
       "SPC J"    "split sexp"
       "SPC l"    "ace line"
       "SPC u"    "universal arg"
       "SPC v"    "expand region"
       "SPC <f1>" "apropos"
       "SPC m"    "maj mode cmds"
       (concat "SPC " dotspacemacs-command-key) "M-x")
      (spacemacs|diminish which-key-mode " Ⓚ" " K"))))
