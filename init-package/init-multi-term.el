(require 'multi-term)

(setq multi-term-program "/bin/zsh")

(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

(add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
