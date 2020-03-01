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

(setq ipython-notebook-packages '(ein ob-ipython))

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
        mode 'ein:notebook-mode
        (kbd "<C-return>") 'ein:worksheet-execute-cell-km
        (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km))
     '(insert hybrid normal))
    (with-eval-after-load 'ein-notebook
      (evil-define-key nil ein:notebooklist-mode-map "o" 'spacemacs/ace-buffer-links)
      (let ((bindings '(("j" ein:worksheet-goto-next-input-km)
                        ("k" ein:worksheet-goto-prev-input-km)
                        ("J" ein:worksheet-move-cell-down-km)
                        ("K" ein:worksheet-move-cell-up-km)
                        ("e" ein:worksheet-toggle-output-km)
                        ("d" ein:worksheet-kill-cell-km)
                        ("y" ein:worksheet-copy-cell-km)
                        ("p" ein:worksheet-yank-cell-km)
                        ("m" ein:worksheet-merge-cell-km)
                        ("s" ein:worksheet-split-cell-at-point-km)
                        ("o" ein:worksheet-insert-cell-below-km)
                        ("O" ein:worksheet-insert-cell-above-km)
                        ("t" ein:worksheet-toggle-cell-type-km)
                        ("C-m" ein:worksheet-execute-cell-km)
                        ("l" ein:worksheet-clear-output-km)
                        ("L" ein:worksheet-clear-all-output-km)
                        ("C-s" ein:notebook-save-notebook-command-km)
                        ("C-r" ein:notebook-rename-command-km)
                        ("x" ein:notebook-close-km)
                        ("z" ein:notebook-kernel-interrupt-command-km))))
        (apply #'spacemacs/set-leader-keys-for-minor-mode
               (quote ein:notebook-mode)
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
                        :bindings
                        ("q" nil :exit t))
                      bindings))))))

(defun ipython-notebook/pre-init-ob-ipython ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-ipython
      :init (add-to-list 'org-babel-load-languages '(ipython . t)))))

(defun ipython-notebook/init-ob-ipython ())
