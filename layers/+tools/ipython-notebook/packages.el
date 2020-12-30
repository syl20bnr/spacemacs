;;; packages.el --- ipython Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ipython-notebook-packages '(ein company))

(defun ipython-notebook/init-ein ()
  (use-package ein
    :init
    (spacemacs/set-leader-keys "atil" 'ein:login
      "atir" 'ein:run
      "atis" 'ein:stop)
    (spacemacs/declare-prefix "ati" "ipython notebook")
    :config
    (with-eval-after-load 'ein-notebook
      (add-hook 'ein:notebook-mode-hook
                (lambda ()
                  (add-to-list 'minor-mode-overriding-map-alist
                               `(ein:notebook-mode . ,ein:notebook-mode-map))))
      (mapc
       (lambda (mode)
         (evil-define-minor-mode-key mode 'ein:notebook-mode
           (kbd "<C-return>") 'ein:worksheet-execute-cell-km
           (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km))
       '(insert hybrid normal))
      (evil-define-minor-mode-key 'normal 'ein:notebook-mode
        "gj" 'ein:worksheet-goto-next-input-km
        "gk" 'ein:worksheet-goto-prev-input-km)
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
                        ("RET" ein:worksheet-execute-cell-and-goto-next-km)
                        ("l" ein:worksheet-clear-output-km)
                        ("L" ein:worksheet-clear-all-output-km)
                        ("C-s" ein:notebook-save-notebook-command-km)
                        ("C-r" ein:notebook-rename-command-km)
                        ("x" ein:notebook-close-km)
                        ("z" ein:notebook-kernel-interrupt-command-km))))
        (apply #'spacemacs/set-leader-keys-for-minor-mode 'ein:notebook-mode
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
                         :title "EIN Transient State"
                         :evil-leader-for-mode (ein:notebook-mode . ".")
                         :bindings
                         ("q" nil :exit t))
                      bindings
                      `(:doc ,(ipython-notebook/transient-doc bindings))))))))

(defun ipython-notebook/post-init-company ()
  (add-hook 'ein:notebook-mode-hook #'spacemacs//ein-setup-company))

(defun ipython-notebook/max-by-prefix (alist)
  (seq-reduce (lambda (lst1 lst2) (if (> (cl-second lst1)
                                         (cl-second lst2))
                                      lst1 lst2))
              (cdr alist) (car alist)))

(defun ipython-notebook/count-by-prefix (alist)
  (mapcar (lambda (lst)
            (cons (car lst) (list (length (cdr lst)))))
          alist))

(defun ipython-notebook/commands-by-prefix-alist (commands)
  "Return ((P1 P1-CMD1 P1-CMD2) (P2 P2-CMD1 P2-CMD2) ... )"
  (let* ((commands (if (symbolp (car commands))
                       (mapcar #'symbol-name commands)
                     commands))
         (upto (ipython-notebook/prefix commands))
         result)
    (cl-flet ((get-prefix
               (command)
               (intern (mapconcat #'identity
                                  (subseq (split-string command (regexp-quote "-"))
                                          0 (1+ upto))
                                  "-"))))
      (mapc (lambda (command)
              (let ((lst (alist-get (get-prefix command) result)))
                (setf (alist-get (get-prefix command) result)
                      (cons (intern command) lst))))
            commands)
      result)))

(cl-defun ipython-notebook/prefix (commands &key (separator "-") (suffix-p nil))
  "Return index of first different prefix among COMMANDS if each were split on SEPARATOR.
For example, return 2 if COMMANDS are '(ein:notebook-foo ein:notebook-foo-bar)."
  (let* ((commands (if (symbolp (car commands))
                       (mapcar #'symbol-name commands)
                     commands))
         (split-up (mapcar (lambda (command)
                             (funcall (if suffix-p #'reverse #'identity)
                                      (split-string command (regexp-quote separator))))
                           commands)))
    (cl-loop for result from 0
             for bogey = (nth result (car split-up))
             if (or (null bogey)
                    (null (cdr split-up))
                    (cl-some (lambda (lst) (not (string= bogey (nth result lst))))
                             (cdr split-up)))
             return (funcall (if suffix-p #'- #'identity) result)
             end)))

(defun ipython-notebook/transient-doc (bindings)
  (let* ((commands-by (ipython-notebook/commands-by-prefix-alist (mapcar #'cl-second bindings)))
         (counts-by (ipython-notebook/count-by-prefix commands-by))
         (max-by (ipython-notebook/max-by-prefix counts-by))
         (main (cl-first max-by))
         (n-main (cl-second max-by))
         (n-other (apply #'+ (mapcar (lambda (lst) (if (eq (cl-first lst) main)
                                                       0 (cl-second lst)))
                                     counts-by)))
         (max-col 3)
         (spread (min max-col (ceiling (/ n-main (* 1.3 n-other)))))
         (main-commands (cdr (assq main commands-by)))
         (other-commands (cl-mapcan #'cdr (remove-if (lambda (lst)
                                                       (eq (car lst) main))
                                                     commands-by)))
         (other-from (ipython-notebook/prefix other-commands))
         (other-to (ipython-notebook/prefix other-commands :suffix-p t))
         (main-from (ipython-notebook/prefix main-commands))
         (main-to (ipython-notebook/prefix main-commands :suffix-p t)))
    (cl-flet ((get-key (command) (car (rassoc (list command) bindings)))
              (massage (command from to)
                       (let ((toks (split-string (symbol-name command) (regexp-quote "-"))))
                         (mapconcat #'identity
                                    (subseq toks from (+ (length toks) to))
                                    "-"))))
      (cl-macrolet ((rescol
                     (result commands from to)
                     `(let* ((key-width 10)
                             (col-width 20)
                             (format-str (format "%%-%ds%%-%ds" key-width col-width)))
                        (if-let ((command (pop ,commands)))
                            (let ((massaged (massage command ,from ,to)))
                              (setq ,result
                                    (concat ,result
                                            (format format-str
                                                    (format "[_%s_]^^" (get-key command))
                                                    (subseq massaged 0
                                                            (min (length massaged) col-width))))))
                          (setq ,result (concat ,result (format format-str "" "")))))))
        (cl-loop
         with result = "\n"
         with betw = (make-string 1 ? )
         with col = 1
         if (= col spread)
         do (rescol result other-commands other-from other-to)
         and do (setq result (concat result "\n"))
         and do (setq col 1)
         else
         do (rescol result main-commands main-from main-to)
         and do (setq result (concat result betw))
         and do (cl-incf col)
         end
         until (and (null other-commands) (null main-commands))
         finally return result)))))
