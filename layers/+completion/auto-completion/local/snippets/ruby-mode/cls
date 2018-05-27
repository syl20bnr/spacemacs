#name : class ... end
#contributor : hitesh <hitesh.jasani@gmail.com>
#group : definitions
# --
class ${1:`(let ((fn (capitalize (file-name-nondirectory
                                 (file-name-sans-extension
				 (or (buffer-file-name)
				     (buffer-name (current-buffer))))))))
             (replace-regexp-in-string "_" "" fn t t))`}
  $0
end
