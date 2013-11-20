(use-package erc
  :defer t
  :config
  (progn
    ;; joining && autojoing
    (erc-autojoin-mode t)
    ;; check channels
    (erc-track-mode t)
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))
    ;; don't show any of this
    (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
    ;; buffer width
    (setq erc-fill-column 140)))

;; from http://emacs-fu.blogspot.ca/2009/06/erc-emacs-irc-client.html
(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "10.140.40.21:6667") ;; ERC already active?
      
      (erc-track-switch-buffer 1)    ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ")   ;; no: maybe start ERC
      (erc :server "10.140.40.21" :port 6667 :nick "sylnux" :full-name "Sylvain Benner"))))
