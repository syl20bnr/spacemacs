(require 'ruby-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" .
               ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" .
               ruby-mode))
