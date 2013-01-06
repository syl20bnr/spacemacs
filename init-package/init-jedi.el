(setq jedi:setup-keys t)
(require 'jedi)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)
