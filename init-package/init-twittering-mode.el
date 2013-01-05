(setq twittering-use-master-password t)
(setq twittering-icon-mode t)                ; Show icons
(setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
(setq twittering-url-show-status nil)        ; Keeps the echo area from showing all the http processes

;; spell check
(add-hook 'twittering-edit-mode-hook (lambda () (ispell-minor-mode) (flyspell-mode)))

;; timelines opened at startup
(setq twittering-initial-timeline-spec-string '(
        ":direct_messages"
        ":replies"
        ":home"
;;        ":search/emacs/"
;;        "user_name/list_name"
        ))

;; add follow URL by pressing 'o'
(add-hook 'twittering-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key twittering-mode-map
                        (read-kbd-macro key) func)))
                  '(("o" . twittering-click)))))
