(use-package grep
  :init
  (progn 
    (defadvice grep-compute-defaults
      (after grep-default-fish-compatibility)
      (grep-apply-setting 'grep-find-command "find . ! -name \"*~\" ! -name \"#*#\" -type f -print0 | xargs -0 -e grep -nH -e ")
      (grep-apply-setting 'grep-find-template (replace-regexp-in-string "\s{\}\s" " \"{}\" " grep-find-template)))
    (ad-activate 'grep-compute-defaults)))

