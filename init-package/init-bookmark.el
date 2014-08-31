(use-package bookmark
  :commands (bookmark-delete
             bookmark-jump
             bookmark-rename
             bookmark-set)
  :config
  (setq 
  bookmark-default-file "~/.emacs.d/bookmarks"  ; keep my ~/ clean
  bookmark-save-flag 1)                         ; autosave each change
)
