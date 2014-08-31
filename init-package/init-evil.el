;; relative line number for operator state
;; inspired by https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-evil.el
(defvar cofi/current-line 0
  "Stores the current line before linum numbers the lines.")
(defadvice linum-update (before set-current-line activate)
  (setq cofi/current-line (line-number-at-pos)))
(defun cofi/relative-line (line-number)
  (let ((relative (abs (- line-number cofi/current-line))))
    (propertize (format "%2d" relative) 'face (if (= relative 0)
                                                  'linum-current-line
                                                'linum))))
(defun cofi/evil-toggle-relative-lines ()
  (interactive)
  (if (eq linum-format #'cofi/relative-line)
      (progn
        (linum-mode -1)
        (setq linum-format #'cofi/linum-dynamic-lines))
    (progn
      (linum-mode t)
      (setq linum-format #'cofi/relative-line)))
  (linum-update-current))
(defun cofi/linum-dynamic-lines (line-number)
  (let ((width (ceiling (log (count-lines (point-min) (point-max)) 10))))
    (propertize (format (format "%%%dd" width) line-number)
                'face (if (= cofi/current-line line-number)
                          'linum-current-line
                        'linum))))
(setq linum-format #'cofi/linum-dynamic-lines)

;; evil mode init ------------------------------------------------------------

(use-package evil
  :init
  (progn
    (setq evil-mode-line-format 'before)
    (setq evil-emacs-state-cursor  '("red" box))
    (setq evil-normal-state-cursor '("orange" box))
    (setq evil-visual-state-cursor '("black" box))
    (setq evil-insert-state-cursor '("green3" box))
    (setq evil-motion-state-cursor '("purple" box))
    (add-to-hooks #'cofi/evil-toggle-relative-lines
                  '(evil-operator-state-entry-hook
                    evil-operator-state-exit-hook))
    ;; I prefer to stay on the original character when leaving insert mode
    ;; (initiated with 'i').
    (setq evil-move-cursor-back nil)
    (evil-mode 1)
    ;; replace motion state by normal state and add some other modes
    (setq evil-normal-state-modes (append '(rcirc-mode) evil-motion-state-modes)))
  :config
  (progn
    ;; inspired from:
    ;; https://github.com/roman/emacs.d/blob/master/zoo/zoo-evil.el
    (evil-define-command fd-trigger (callback)
      "Allows to execute the passed function using 'fd'."
      :repeat change
      (let ((modified (buffer-modified-p)))
        (insert "f")
        (let ((evt (read-event
                    (format "Insert %c to exit insert state" ?d)
                    nil 0.1)))
          (cond
           ((null evt)
              (message ""))
           ((and (integerp evt)
                 (char-equal evt ?d))
              ;; remove the f character
              (delete-char -1)
              (set-buffer-modified-p modified)
              (funcall callback))
           (t ; otherwise
              (setq unread-command-events (append unread-command-events
                                                  (list evt))))))))
    ;; load evil-leader
    (use-package evil-leader
      :init
      (progn
        (setq evil-leader/in-all-states t
              evil-leader/leader "SPC"
              evil-leader/non-normal-prefix "s-")
        (global-evil-leader-mode))
      :config
      (progn
      ;; Unset shortcuts which shadow evil leader
      (eval-after-load "compile"
        '(define-key compilation-mode-map (kbd "SPC") nil))
      (eval-after-load "dired"
        '(define-key dired-mode-map (kbd "SPC") nil))
      ;; make leader available in visual mode
      (define-key evil-visual-state-map (kbd "SPC") evil-leader--default-map)
      (define-key evil-motion-state-map (kbd "SPC") evil-leader--default-map)
      (define-key evil-emacs-state-map  (kbd "SPC") evil-leader--default-map)))
    ;; load surround
    (use-package surround
      :init (global-surround-mode 1)) 
    ;; load evil-exchange
    (use-package evil-exchange
      :init (evil-exchange-install))
    ;; initiate a search of the selected text
    (use-package evil-visualstar)
    ))
