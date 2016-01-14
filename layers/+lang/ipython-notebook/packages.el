;;; packages.el --- ipython Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ipython-notebook-packages '(ein))

(defun ipython-notebook/init-ein ()
  (use-package ein
    :defer t
    :commands ein:notebooklist-open
    :init
    (spacemacs/set-leader-keys "ain" 'ein:notebooklist-open)
    :config
    (progn
      (defun spacemacs/ein:worksheet-merge-cell-next ()
        (interactive)
        (ein:worksheet-merge-cell (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

      (defvar spacemacs--ipython-notebook-ms-doc-toggle 0
        "Display a short doc when nil, full doc otherwise.")

      (defun spacemacs//ipython-notebook-ms-doc ()
        (if (equal 0 spacemacs--ipython-notebook-ms-doc-toggle)
            "[_?_] for help"
          "
        [_?_] toggle this help

        [_k_,_j_] prev/next cell               [_K_,_J_] move cell up/down
        [_h_,_l_] prev/next worksheet          [_H_,_L_] move worksheet left/right
        [_C-k_,_C-j_] merge cell above/bellow  [_O_,_o_] insert cell above/bellow
        [_d_] kill cell                      [_y_,_p_] copy or past cell
        [_RET_] execute cell                 [_u_] change cell type

        [_C-o_]: open console                [_t_] toggle output
        [_C-l_,_C-S-L_] clear/clear all output

        [_C-s_,_C-r_] save/rename notebook
        [_1_.._9_] open [_1st..last_] worksheet
        [_+_,_-_] create/delete worksheet

        [_x_] close notebook                 [_q_] quit micro-state"))

      (defun spacemacs//ipython-notebook-ms-toggle-doc ()
        (interactive)
        (setq spacemacs--ipython-notebook-ms-doc-toggle
              (logxor spacemacs--ipython-notebook-ms-doc-toggle 1)))

      (defun spacemacs//concat-leader (key)
        (if dotspacemacs-major-mode-leader-key
            (concat dotspacemacs-major-mode-leader-key key)
            (concat "," key)
        ))

      (spacemacs/set-leader-keys-for-major-mode 'ein:notebook-multilang-mode
        "y" 'ein:worksheet-copy-cell
        "p" 'ein:worksheet-yank-cell
        "d" 'ein:worksheet-kill-cell
        "h" 'ein:notebook-worksheet-open-prev-or-last
        "i" 'ein:worksheet-insert-cell-below
        "I" 'ein:worksheet-insert-cell-above
        "j" 'ein:worksheet-goto-next-input
        "k" 'ein:worksheet-goto-prev-input
        "l" 'ein:notebook-worksheet-open-next-or-first
        "H" 'ein:notebook-worksheet-move-prev
        "J" 'ein:worksheet-move-cell-down
        "K" 'ein:worksheet-move-cell-up
        "L" 'ein:notebook-worksheet-move-next
        "t" 'ein:worksheet-toggle-output
        "R" 'ein:worksheet-rename-sheet
        "RET" 'ein:worksheet-execute-cell-and-goto-next
        ;; Output
        " C-l" 'ein:worksheet-clear-output
        " C-S-l" 'ein:worksheet-clear-all-output
        ;;Console
        " C-o" 'ein:console-open
        ;; Merge cells
        " C-k" 'ein:worksheet-merge-cell
        " C-j" 'spacemacs/ein:worksheet-merge-cell-next
        ;; Notebook
        " C-s" 'ein:notebook-save-notebook-command
        " C-r" 'ein:notebook-rename-command
        "1" 'ein:notebook-worksheet-open-1th
        "2" 'ein:notebook-worksheet-open-2th
        "3" 'ein:notebook-worksheet-open-3th
        "4" 'ein:notebook-worksheet-open-4th
        "5" 'ein:notebook-worksheet-open-5th
        "6" 'ein:notebook-worksheet-open-6th
        "7" 'ein:notebook-worksheet-open-7th
        "8" 'ein:notebook-worksheet-open-8th
        "9" 'ein:notebook-worksheet-open-last
        "+" 'ein:notebook-worksheet-insert-next
        "-" 'ein:notebook-worksheet-delete
        "x" 'ein:notebook-close
        "u" 'ein:worksheet-change-cell-type
        "fs" 'ein:notebook-save-notebook-command)

      ;; keybindings for ipython notebook traceback mode
      (spacemacs/set-leader-keys-for-major-mode 'ein:traceback-mode
        "RET" 'ein:tb-jump-to-source-at-point-command
        "n" 'ein:tb-next-item
        "p" 'ein:tb-prev-item
        "q" 'bury-buffer)

      ;; keybindings mirror ipython web interface behavior
      (evil-define-key 'insert ein:notebook-multilang-mode-map
        (kbd "<C-return>") 'ein:worksheet-execute-cell
        (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next)

      (evil-define-key 'normal ein:notebook-multilang-mode-map
        ;; keybindings mirror ipython web interface behavior
        (kbd "<C-return>") 'ein:worksheet-execute-cell
        (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next
        "gj" 'ein:worksheet-goto-next-input
        "gk" 'ein:worksheet-goto-prev-input)

      ;; if this is not required then the following keygindings fail
      (require 'ein-multilang)
      (define-key ein:notebook-multilang-mode-map (kbd "M-j") 'ein:worksheet-move-cell-down)
      (define-key ein:notebook-multilang-mode-map (kbd "M-k") 'ein:worksheet-move-cell-up)

      (spacemacs|define-micro-state-2 ipython-notebook
        :doc (concat (spacemacs//ipython-notebook-ms-doc))
        :bindings
        ("q" nil :exit t)
        ("?" spacemacs//ipython-notebook-ms-toggle-doc)
        ("h" ein:notebook-worksheet-open-prev-or-last)
        ("j" ein:worksheet-goto-next-input)
        ("k" ein:worksheet-goto-prev-input)
        ("l" ein:notebook-worksheet-open-next-or-first)
        ("H" ein:notebook-worksheet-move-prev)
        ("J" ein:worksheet-move-cell-down)
        ("K" ein:worksheet-move-cell-up)
        ("L" ein:notebook-worksheet-move-next)
        ("t" ein:worksheet-toggle-output)
        ("d" ein:worksheet-kill-cell)
        ("R" ein:worksheet-rename-sheet)
        ("y" ein:worksheet-copy-cell)
        ("p" ein:worksheet-yank-cell)
        ("o" ein:worksheet-insert-cell-below)
        ("O" ein:worksheet-insert-cell-above)
        ("u" ein:worksheet-change-cell-type)
        ("RET" ein:worksheet-execute-cell-and-goto-next)
        ;; Output
        ("C-l" ein:worksheet-clear-output)
        ("C-S-l" ein:worksheet-clear-all-output)
        ;;Console
        ("C-o" ein:console-open)
        ;; Merge cells
        ("C-k" ein:worksheet-merge-cell)
        ("C-j" spacemacs/ein:worksheet-merge-cell-next)
        ;; Notebook
        ("C-s" ein:notebook-save-notebook-command)
        ("C-r" ein:notebook-rename-command)
        ("1" ein:notebook-worksheet-open-1th)
        ("2" ein:notebook-worksheet-open-2th)
        ("3" ein:notebook-worksheet-open-3th)
        ("4" ein:notebook-worksheet-open-4th)
        ("5" ein:notebook-worksheet-open-5th)
        ("6" ein:notebook-worksheet-open-6th)
        ("7" ein:notebook-worksheet-open-7th)
        ("8" ein:notebook-worksheet-open-8th)
        ("9" ein:notebook-worksheet-open-last)
        ("+" ein:notebook-worksheet-insert-next)
        ("-" ein:notebook-worksheet-delete)
        ("x" ein:notebook-close))
      (spacemacs/set-leader-keys "ein" ))))
