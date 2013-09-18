(require 'cc-mode)
(add-hook 'c-mode-hook '(lambda () (c-toggle-auto-state t)))
(add-hook 'c++-mode-hook '(lambda () (c-toggle-auto-state t)))

;; From http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
(setq compilation-finish-function
   (lambda (buf str)

     (if (or (string-match "exited abnormally" str)
            (string-match "FAILED" (buffer-string)))

         ;;there were errors
         (message "There were errors. SPC-e-n to visit.")

       ;;no errors, make the compilation window go away in 0.5 seconds
       (run-at-time 1.0 nil 'delete-windows-on buf)
       (message "Ok."))))
