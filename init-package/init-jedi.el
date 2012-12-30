(setq jedi:setup-keys t)
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
