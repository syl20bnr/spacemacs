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

(setq ipython-notebook-packages '(ein))

(defun ipython-notebook/init-ein ()
  (use-package ein
    :init
    (spacemacs/set-leader-keys "ayl" 'ein:login
      "ayr" 'ein:run
      "ays" 'ein:stop)
    (spacemacs/declare-prefix "ay" "ipython notebook")
    :config
    (mapc
     (lambda (mode)
       (evil-define-minor-mode-key
         mode 'ein:notebook
         (kbd "<C-return>") 'ein:worksheet-execute-cell-km
         (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km))
     '(insert hybrid normal))
    (evil-define-minor-mode-key
      'normal 'ein:notebook
      "gj" 'ein:worksheet-goto-next-input-km
      "gk" 'ein:worksheet-goto-prev-input-km)
    (with-eval-after-load 'ein-notebook
      (let ((bindings '(("y" ein:worksheet-copy-cell)
                        ("p" ein:worksheet-yank-cell)
                        ("d" ein:worksheet-kill-cell)
                        ("h" ein:notebook-worksheet-open-prev-or-last)
                        ("i" ein:worksheet-insert-cell-below)
                        ("I" ein:worksheet-insert-cell-above)
                        ("j" ein:worksheet-goto-next-input)
                        ("k" ein:worksheet-goto-prev-input)
                        ("l" ein:notebook-worksheet-open-next-or-first)
                        ("H" ein:notebook-worksheet-move-prev)
                        ("J" ein:worksheet-move-cell-down)
                        ("K" ein:worksheet-move-cell-up)
                        ("L" ein:notebook-worksheet-move-next)
                        ("t" ein:worksheet-toggle-output)
                        ("R" ein:worksheet-rename-sheet)
                        ("RET" ein:worksheet-execute-cell-and-goto-next)
                        ;; Output
                        ("C-l" ein:worksheet-clear-output)
                        ("C-S-l" ein:worksheet-clear-all-output)
                        ;;Console
                        ;; ("C-o" ein:console-open)
                        ;; Merge cells
                        ("C-k" ein:worksheet-merge-cell)
                        ;;("C-j" spacemacs/ein:worksheet-merge-cell-next)
                        ("s" ein:worksheet-split-cell-at-point)
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
                        ("x" ein:notebook-close)
                        ("u" ein:worksheet-change-cell-type)
                        ("fs" ein:notebook-save-notebook-command))))
        (apply #'spacemacs/set-leader-keys-for-minor-mode
               (quote ein:notebook)
               (cl-mapcan
                (lambda (bind)
                  (if (fboundp (cl-second bind))
                      bind
                    (prog1 nil
                      (display-warning
                       'warn (format "ipython-notebook/init-ein: undefined %s"
                                     (cl-second bind))))))
                (copy-tree bindings)))
        (eval (append '(spacemacs|define-transient-state
			                   ipython-notebook
			                   :title "iPython Notebook Transient State"
                         :doc "
      Operations on Cells^^^^^^            On Worksheets^^^^              Other
      ----------------------------^^^^^^   ------------------------^^^^   ----------------------------------^^^^
      [_k_/_j_]^^     select prev/next     [_h_/_l_]   select prev/next   [_t_]^^         toggle output
      [_K_/_J_]^^     move up/down         [_H_/_L_]   move left/right    [_C-l_/_C-S-l_] clear/clear all output
      [_C-k_]^^       merge above          [_1_.._9_]  open [1st..last]   ^^^^
      [_y_/_p_/_d_]   copy/paste           [_+_/_-_]   create/delete      [_C-s_/_C-r_]   save/rename notebook
      [_u_]^^^^       change type          ^^^^                           [_x_]^^         close notebook
      [_RET_]^^^^     execute              ^^^^                           [_q_]^^         quit transient-state
           "
			                   :evil-leader-for-mode (ein:notebook . ".")
			                   :bindings
			                   ("q" nil :exit t))
		                  bindings))))))
