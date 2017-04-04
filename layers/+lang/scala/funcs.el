;;; funcs.el --- Scala Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//scala-setup-ensime ()
  "Setup ENSIME for Scala."
  (spacemacs//java-setup-ensime))

(defun spacemacs//scala-disable-flycheck-scala ()
  (push 'scala flycheck-disabled-checkers))

(defun spacemacs/scala-join-line ()
  "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
  (interactive)
  (let (join-pos)
    (save-excursion
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-line)
        (call-interactively 'scala-indent:join-line)
        (setq join-pos (point))))

    (when join-pos
      (goto-char join-pos))))
<<<<<<< HEAD
=======

(defun scala/completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (company-abort)
  (insert ".")
  (company-complete))

;;; Flyspell

(defun scala/flyspell-verify ()
  "Prevent common flyspell false positives in scala-mode."
  (and (flyspell-generic-progmode-verify)
       (not (s-matches? (rx bol (* space) "package") (current-line)))))

(defun scala/configure-flyspell ()
  (setq-local flyspell-generic-check-word-predicate 'scala/flyspell-verify))

(defun scala/yank-type-at-point ()
  "Yank to kill ring and print short type name at point to the minibuffer."
  (interactive)
  (ensime-type-at-point t nil))

(defun scala/yank-type-at-point-full-name ()
  "Yank to kill ring and print full type name at point to the minibuffer."
  (interactive)
  (ensime-type-at-point t t))
>>>>>>> bd7ef98e4c35fd87538dd2a81356cc83f5fd02f3
