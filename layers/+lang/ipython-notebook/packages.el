;;; packages.el --- ipython Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ipython-notebook-packages
      '(
        company
        ein
        ob-ipython
        websocket
        request
        request-deferred
        dash
        s
        skewer-mode
        ))

(defun ipython-notebook/init-websocket ()
  (use-package websocket
    :defer t
    :init))

(defun ipython-notebook/init-request-deferred ()
  (use-package request-deferred
    :defer t
    :init))

(defun ipython-notebook/init-dash ()
  (use-package dash
    :defer t
    :init))

(defun ipython-notebook/init-s ()
  (use-package s
    :defer t
    :init))

(defun ipython-notebook/post-init-skewer-mode ())

(defun ipython-notebook/post-init-request ())

(defun ipython-notebook/post-init-company ())

(defun ipython-notebook/init-ein-notebook ()
  (use-package ein-notebook
    :defer t
    :init))

(defun ipython-notebook/init-ein-subpackages ()
  (use-package ein-subpackages
    :defer t
    :init))

(defun ipython-notebook/init-ein ()
  (use-package ein
    :defer t
    :commands (ein:notebooklist-open ein:notebooklist-login ein:run ein:stop)
    :init
    (progn
      (spacemacs/set-leader-keys
        "ayl" 'ein:notebooklist-login
        "ayo" 'ein:notebooklist-open
        "ayr" 'ein:run
        "ays" 'ein:stop)
      (spacemacs/declare-prefix "ay" "ipython notebook")
      (with-eval-after-load 'ein-notebooklist
        (evilified-state-evilify-map ein:notebooklist-mode-map
          :mode ein:notebooklist-mode
          :bindings
          (kbd "o") 'spacemacs/ace-buffer-links)
        (define-key ein:notebooklist-mode-map "o" 'spacemacs/ace-buffer-links)))
    :config
    (progn
      (defun spacemacs/ein:worksheet-merge-cell-next ()
        (interactive)
        (ein:worksheet-merge-cell (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

      (defun spacemacs//concat-leader (key)
        (if dotspacemacs-major-mode-leader-key
            (concat dotspacemacs-major-mode-leader-key key)
          (concat "," key)))

      (spacemacs/set-leader-keys-for-minor-mode 'ein:notebook-mode
        "y" 'ein:worksheet-copy-cell-km
        "p" 'ein:worksheet-yank-cell-km
        "d" 'ein:worksheet-kill-cell-km
        "h" 'ein:notebook-worksheet-open-prev-or-last-km
        "i" 'ein:worksheet-insert-cell-below-km
        "I" 'ein:worksheet-insert-cell-above-km
        "j" 'ein:worksheet-goto-next-input-km
        "k" 'ein:worksheet-goto-prev-input-km
        "l" 'ein:notebook-worksheet-open-next-or-first-km
        "H" 'ein:notebook-worksheet-move-prev-km
        "J" 'ein:worksheet-move-cell-down-km
        "K" 'ein:worksheet-move-cell-up-km
        "L" 'ein:notebook-worksheet-move-next-km
        "t" 'ein:worksheet-toggle-output-km
        "R" 'ein:worksheet-rename-sheet-km
        "RET" 'ein:worksheet-execute-cell-and-goto-next-km
        ;; Output
        "C-l" 'ein:worksheet-clear-output-km
        "C-S-l" 'ein:worksheet-clear-all-output-km
        ;;Console
        "C-o" 'ein:console-open
        ;; Merge cells
        "C-k" 'ein:worksheet-merge-cell-km
        "C-j" 'spacemacs/ein:worksheet-merge-cell-next
        "s" 'ein:worksheet-split-cell-at-point-km
        ;; Notebook
        "C-s" 'ein:notebook-save-notebook-command-km
        "C-r" 'ein:notebook-rename-command-km
        "1" 'ein:notebook-worksheet-open-1th-km
        "2" 'ein:notebook-worksheet-open-2th-km
        "3" 'ein:notebook-worksheet-open-3th-km
        "4" 'ein:notebook-worksheet-open-4th-km
        "5" 'ein:notebook-worksheet-open-5th-km
        "6" 'ein:notebook-worksheet-open-6th-km
        "7" 'ein:notebook-worksheet-open-7th-km
        "8" 'ein:notebook-worksheet-open-8th-km
        "9" 'ein:notebook-worksheet-open-last-km
        "+" 'ein:notebook-worksheet-insert-next-km
        "-" 'ein:notebook-worksheet-delete-km
        "x" 'ein:notebook-close-km
        "u" 'ein:worksheet-change-cell-type-km
        "fs" 'ein:notebook-save-notebook-command-km)

      ;; keybindings for ipython notebook traceback mode
      (spacemacs/set-leader-keys-for-major-mode 'ein:traceback-mode
        "RET" 'ein:tb-jump-to-source-at-point-command
        "n" 'ein:tb-next-item
        "p" 'ein:tb-prev-item
        "q" 'bury-buffer)

      ;; keybindings mirror ipython web interface behavior
      (evil-define-minor-mode-key 'insert 'ein:notebook-mode
        (kbd "<C-return>") 'ein:worksheet-execute-cell-km
        (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km)

      ;; keybindings mirror ipython web interface behavior
      (evil-define-minor-mode-key 'hybrid 'ein:notebook-mode
        (kbd "<C-return>") 'ein:worksheet-execute-cell-km
        (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km)

      (evil-define-minor-mode-key 'normal 'ein:notebook-mode
        ;; keybindings mirror ipython web interface behavior
        (kbd "<C-return>") 'ein:worksheet-execute-cell-km
        (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km
        "gj" 'ein:worksheet-goto-next-input-km
        "gk" 'ein:worksheet-goto-prev-input-km)

      ;; if this is not required then the following keygindings fail
      (require 'ein-notebook)
      (define-key ein:notebook-mode-map (kbd "M-j") 'ein:worksheet-move-cell-down-km)
      (define-key ein:notebook-mode-map (kbd "M-k") 'ein:worksheet-move-cell-up-km)

      (spacemacs|define-transient-state ipython-notebook
        :title "iPython Notebook Transient State"
        :doc "
 Operations on Cells^^^^^^            On Worksheets^^^^              Other
 ----------------------------^^^^^^   ------------------------^^^^   ----------------------------------^^^^
 [_k_/_j_]^^     select prev/next     [_h_/_l_]   select prev/next   [_t_]^^         toggle output
 [_K_/_J_]^^     move up/down         [_H_/_L_]   move left/right    [_C-l_/_C-S-l_] clear/clear all output
 [_C-k_/_C-j_]^^ merge above/below    [_1_.._9_]  open [1st..last]   [_C-o_]^^       open console
 [_O_/_o_]^^     insert above/below   [_+_/_-_]   create/delete      [_C-s_/_C-r_]   save/rename notebook
 [_y_/_p_/_d_]   copy/paste           ^^^^                           [_x_]^^         close notebook
 [_u_]^^^^       change type          ^^^^                           [_q_]^^         quit transient-state
 [_RET_]^^^^     execute"
        :evil-leader-for-mode (ein:notebook-mode . ".")
        :bindings
        ("q" nil :exit t)
        ("?" spacemacs//ipython-notebook-ms-toggle-doc)
        ("h" ein:notebook-worksheet-open-prev-or-last-km)
        ("j" ein:worksheet-goto-next-input-km)
        ("k" ein:worksheet-goto-prev-input-km)
        ("l" ein:notebook-worksheet-open-next-or-first-km-km)
        ("H" ein:notebook-worksheet-move-prev-km-km)
        ("J" ein:worksheet-move-cell-down-km)
        ("K" ein:worksheet-move-cell-up-km)
        ("L" ein:notebook-worksheet-move-next-km-km)
        ("t" ein:worksheet-toggle-output-km)
        ("d" ein:worksheet-kill-cell-km)
        ("R" ein:worksheet-rename-sheet-km)
        ("y" ein:worksheet-copy-cell-km)
        ("p" ein:worksheet-yank-cell-km)
        ("o" ein:worksheet-insert-cell-below-km)
        ("O" ein:worksheet-insert-cell-above-km)
        ("u" ein:worksheet-change-cell-type-km)
        ("RET" ein:worksheet-execute-cell-and-goto-next-km)
        ;; Output
        ("C-l" ein:worksheet-clear-output-km)
        ("C-S-l" ein:worksheet-clear-all-output-km)
        ;;Console
        ("C-o" ein:console-open-km)
        ;; Merge and split cells
        ("C-k" ein:worksheet-merge-cell-km)
        ("C-j" spacemacs/ein:worksheet-merge-cell-next)
        ("s" ein:worksheet-split-cell-at-point-km)
        ;; Notebook
        ("C-s" ein:notebook-save-notebook-command-km)
        ("C-r" ein:notebook-rename-command-km)
        ("1" ein:notebook-worksheet-open-1th-km)
        ("2" ein:notebook-worksheet-open-2th-km)
        ("3" ein:notebook-worksheet-open-3th-km)
        ("4" ein:notebook-worksheet-open-4th-km)
        ("5" ein:notebook-worksheet-open-5th-km)
        ("6" ein:notebook-worksheet-open-6th-km)
        ("7" ein:notebook-worksheet-open-7th-km)
        ("8" ein:notebook-worksheet-open-8th-km)
        ("9" ein:notebook-worksheet-open-last-km)
        ("+" ein:notebook-worksheet-insert-next-km)
        ("-" ein:notebook-worksheet-delete-km)
        ("x" ein:notebook-close-km)))))

(defun ipython-notebook/pre-init-ob-ipython ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-ipython
      :init (add-to-list 'org-babel-load-languages '(ipython . t)))))

(defun ipython-notebook/init-ob-ipython ())
