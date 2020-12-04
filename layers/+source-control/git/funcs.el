;;; funcs.el --- Colors Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; magit

(defun space-macs/magit-toggle-whitespace ()
  "Toggle whitespace in `magit-diff-mode'."
  (interactive)
  (if (member "-w" (if (derived-mode-p 'magit-diff-mode)
                       magit-refresh-args
                     magit-diff-section-arguments))
      (space-macs//magit-dont-ignore-whitespace)
    (space-macs//magit-ignore-whitespace)))

(defun space-macs//magit-ignore-whitespace ()
  "Ignore whitespace in `magit-diff-mode'"
  (add-to-list (if (derived-mode-p 'magit-diff-mode)
                   'magit-refresh-args 'magit-diff-section-arguments) "-w")
  (magit-refresh))

(defun space-macs//magit-dont-ignore-whitespace ()
  "Don't ignore whitespace in `magit-diff-mode'"
  (setq magit-diff-options
        (remove "-w"
                (if (derived-mode-p 'magit-diff-mode)
                    magit-refresh-args
                  magit-diff-section-arguments)))
  (magit-refresh))

(defun space-macs/git-permalink ()
  "Allow the user to get a permalink via git-link in a git-timemachine buffer."
  (interactive)
  (let ((git-link-use-commit t))
    (call-interactively 'git-link-commit)))

(defun space-macs/git-permalink-copy-url-only ()
  "Allow the user to get a permalink via git-link in a git-timemachine buffer."
  (interactive)
  (let (git-link-open-in-browser
        (git-link-use-commit t))
    (call-interactively 'git-link-commit)))

(defun space-macs/git-link-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link)))

(defun space-macs/git-link-commit-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link-commit)))


(defun space-macs//support-evilified-buffer-p (style)
  "Return non-nil if evil navigation should be enabled for STYLE."
  (or (eq style 'vim)
      (and (eq style 'hybrid)
           hybrid-style-enable-evilified-state)))

(defun space-macs//magit-evil-magit-bindings (style)
  "Set `evil-magit' bindings for the given editing STYLE."
  (cond
   ((space-macs//support-evilified-buffer-p style)
    (evil-magit-init))
   (t
    (when (featurep 'evil-magit)
      (evil-magit-revert)))))


;; git blame transient state

(defun space-macs//git-blame-ts-toggle-hint ()
  "Toggle the full hint docstring for the git blame transient state."
  (interactive)
  (setq space-macs--git-blame-ts-full-hint-toggle
        (not space-macs--git-blame-ts-full-hint-toggle)))

(defun space-macs//git-blame-ts-hint ()
  "Return a condensed/full hint for the git-blame transient state"
  (concat
   " "
   (if space-macs--git-blame-ts-full-hint-toggle
       space-macs--git-blame-ts-full-hint
     (concat "[" (propertize "?" 'face 'hydra-face-red) "] help"
             space-macs--git-blame-ts-minified-hint))))

(space-macs|transient-state-format-hint git-blame
  space-macs--git-blame-ts-minified-hint "\n
Chunks: _n_ _N_ _p_ _P_ _RET_ Commits: _b_ _r_ _f_ _e_ _q_")

(space-macs|transient-state-format-hint git-blame
  space-macs--git-blame-ts-full-hint
  (format "\n[_?_] toggle help
Chunks^^^^                   Commits^^                     Other
[_p_/_P_] prev /same commit  [_b_] adding lines            [_c_] cycle style
[_n_/_N_] next /same commit  [_r_] removing lines          [_Y_] copy hash
[_RET_]^^ show commit        [_f_] last commit with lines  [_B_] magit-blame
^^^^                         [_e_] echo                    [_Q_] quit TS
^^^^                         [_q_] quit blaming"))


;; Forge

(defun space-macs/forge-get-info-from-fetched-notification-error (err)
  "Return info for given s-exp error return by `forge-pull-notifications'.

Call this function interactively and paste the s-exp from the error returned by
the `forge-pull-notifications' function.

Example of error:

error in process filter: ghub--signal-error: peculiar error:
((path \"query\" \"_Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5MDM3NDE6NTI0NzY1\")
 (extensions (code . \"undefinedField\")
 (typeName . \"Query\")
 (fieldName . \"nil\"))
 (locations ((line . 2) (column . 1)))
 (message . \"Field 'nil' doesn't exist on type 'Query'\"))

Function adapted from issue:
https://github.com/magit/forge/issues/80#issuecomment-456103195
"
  (interactive "xs-exp: ")
  (message "%s" err)
  (let* ((query_value (third (car err)))
         (result (car (forge-sql
                       [:select [owner name]
                                :from repository
                                :where (= id $s1)]
                       (base64-encode-string
                        (mapconcat
                         #'identity
                         (butlast
                          (split-string
                           (base64-decode-string (substring query_value 1))
                           ":"))
                         ":")
                        t)))))
    (message "repository: %s/%s" (car result) (cadr result))))


