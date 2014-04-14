(use-package python
  :defer t
  :init
  (progn
    (add-hook 'python-mode-hook '(lambda()
                                   (setq indent-tabs-mode nil)
                                   (setq indent-level 4)
                                   (setq python-indent 4)
                                   (setq tab-width 4)))
    ;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
    (defun annotate-pdb ()
      "Highlight break point lines."
      (interactive)
      (highlight-lines-matching-regexp "import pdb")
      (highlight-lines-matching-regexp "pdb.set_trace()"))
    (add-hook 'python-mode-hook 'annotate-pdb)
    (use-package jedi
      :defer t
      :init
      (progn
        (setq jedi:setup-keys t)
        (add-hook 'python-mode-hook 'jedi:setup))
      :config
      (progn
        (setq jedi:complete-on-dot t))))
  :config
  (progn
    ;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
    (defun python-add-breakpoint ()
      "Add a break point, highlight it and save the buffer."
      (interactive)
      (evil-end-of-line)
      (newline-and-indent)
      (insert "import pdb; pdb.set_trace()")
      (save-buffer))))
