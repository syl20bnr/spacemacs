;; Setup ipython integration with python-mode"
(setq
python-shell-interpreter "ipython"
python-shell-interpreter-args ""
python-shell-prompt-regexp "In \[[0-9]+\]: "
python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
python-shell-completion-setup-code ""
python-shell-completion-string-code "';'.join(get_ipython().complete('''%s''')[1])\n")

