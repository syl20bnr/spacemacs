(require 'cc-mode)
(add-hook 'c-mode-hook '(lambda () (c-toggle-auto-state t)))
(add-hook 'c++-mode-hook '(lambda () (c-toggle-auto-state t)))

;; From http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
(setq compilation-finish-function
   (lambda (buf str)

     (if (string-match "exited abnormally" str)

         ;;there were errors
         (message "compilation errors, press C-x ` to visit")

       ;;no errors, make the compilation window go away in 0.5 seconds
       (run-at-time 0.5 nil 'delete-windows-on buf)
       (message "No compilation errors."))))
