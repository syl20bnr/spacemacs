;;; keybindings.el --- Emacs Lisp Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-transient-state edebug
  :title "Edebug Transient State"
  :doc
  "
Modes^^^^                               Jumping^^^^               Eval^^                   Other^^
-----^^^^----------------------------- --------^^^^------------- -----^^---------------- -------^^-------------------
[_c_/_C_] continue/continue fast mode   [_f_] forward sexp      [_e_] eval expression    [_?_]^^   help
[_g_/_G_] go/go non-stop mode           [_h_] goto here         [_E_] eval last sexp     [_q_/_Q_] quit
[_n_]^^   next mode                     [_i_] step in           [_F_] edebug defun       [_d_]^^   backtrace
[_s_]^^   step mode                     [_o_] step out          [_r_] previous result    ^^^^
[_t_/_T_] trace/trace-fast mode
"
  :evil-leader-for-mode (emacs-lisp-mode . "dd")
  :foreign-keys warn
  :bindings
  ("?" edebug-help)
  ("c" edebug-continue-mode)
  ("C" edebug-Continue-fast-mode)
  ("d" edebug-backtrace)
  ("e" eval-expression)
  ("E" edebug-eval-last-sexp)
  ("f" edebug-forward-sexp)
  ("F" edebug-defun :exit t)
  ("g" edebug-go-mode)
  ("G" edebug-Go-nonstop-mode)
  ("h" edebug-goto-here)
  ("i" edebug-step-in)
  ("n" edebug-next-mode)
  ("o" edebug-step-out)
  ("q" top-level :exit t)
  ("Q" edebug-top-level-nonstop :exit t)
  ("r" edebug-previous-result)
  ("s" edebug-step-mode)
  ("S" edebug-stop)
  ("t" edebug-trace-mode)
  ("T" edebug-Trace-fast-mode))

(add-hook 'edebug-mode-hook 'spacemacs/edebug-transient-state/body)
