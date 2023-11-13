;;; evil-matchit.el --- Vim matchit ported to Evil

;; Copyright (C) 2014-2020 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/evil-matchit
;; Version: 3.0.2
;; Keywords: matchit vim evil
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This program emulates matchit.vim by Benji Fisher.
;;
;; If EVIL is installed,
;;   - Add `(global-evil-matchit-mode 1)' into Emacs setup.
;;   Then press % or `evilmi-jump-items' to jump between then matched pair.
;;   Text object "%" is also provided.
;;
;;   - The shortcut "%" is defined in `evilmi-shortcut'.  It's both the name of
;;   text object and shortcut of `evilmi-jump-items'.  Some people prefer set it
;;   to "m".  Here is sample setup:
;;
;;   (setq evilmi-shortcut "m")
;;   (global-evil-matchit-mode 1)
;;
;;
;; If EVIL is NOT installed,
;;  - Use `evilmi-jump-items-native' to replace `evilmi-jump-items'
;;
;;  - `evilmi-shortcut' and `global-evil-matchit-mode' are not used
;;
;; Tips:
;;   It's reported some mode is not compatible with this package.
;;   You can use `evilmi-jump-hook' to turn off the mode before
;;   jumping to the matched tag.
;;   Then turn on it after the jump using the same hook.
;;
;;   An example to toggle `global-tree-sitter-mode',
;;
;;   (add-hook 'evilmi-jump-hook
;;             (lambda (before-jump-p)
;;               (global-tree-sitter-mode (not before-jump-p))))
;;
;; See https://github.com/redguardtoo/evil-matchit/ for more information
;;
;; This program requires EVIL (https://github.com/emacs-evil/evil)
;;
;; The other commands like `evilmi-select-items' and `evil-delete-items'
;; always work with or without EVIL.

;;; Code:

(defgroup evil-matchit nil
  "Matchit.vim for Emacs."
  :group 'evil
  :prefix "evil-matchit")

(require 'evil-matchit-sdk)

(defcustom evilmi-jump-hook nil
  "Hook run before&after jump to the matched tag.
If the parameter of hook is t, the hook runs before jump.
Or else, the hook runs after jump.
Some modes can be toggle on/off in the hook"
  :group 'evil-matchit
  :type 'hook)

(defcustom evilmi-plugins
  '(emacs-lisp-mode ((evilmi-simple-get-tag evilmi-simple-jump)))
  "The Matrix of algorithms."
  :group 'evil-matchit
  :type '(repeat sexp))

;;;###autoload
(defun evilmi-jump-items-internal (num &optional func)
  "Jump between items NUM times and apply function FUNC."
  (when evilmi-debug
    (message "evilmi-jump-items-internal called => %s (point)=%d func=%s" num (point) func))
  (let* ((jump-rules (plist-get evilmi-plugins major-mode))
         rlt
         jumped
         ideal-dest)

    (unless num (setq num 1))


    (run-hook-with-args 'evilmi-jump-hook t)

    (when (derived-mode-p 'prog-mode)
      (setq jump-rules
            (append (plist-get evilmi-plugins 'prog-mode) jump-rules)))

    (when jump-rules
      (dolist (rule jump-rules)
        ;; execute evilmi-xxxx-get-tag
        ;; every rule should be executed.
        ;; the simple rule might just forward a word
        (setq rlt (funcall (nth 0 rule)))
        (when (and rlt (not jumped))
          ;; before jump, we may need some operation
          (if func (funcall func rlt))
          ;; jump now, execute evilmi-xxxx-jump
          (setq ideal-dest (funcall (nth 1 rule) rlt num))
          ;; jump only once if the jump is successful
          (setq jumped t))
        (when (and evilmi-debug rlt)
          (message "rlt=%s rule=%s p=%s jumped=%s idea-dest=%s"
                   rlt
                   rule
                   (point)
                   jumped
                   ideal-dest))))

    ;; give `evilmi-sdk-simple-jump' a second chance
    (unless jumped
      (if func (funcall func (list (point))))
      (evilmi-sdk-simple-jump)
      (setq ideal-dest (point)))

    (if evilmi-debug (message "evilmi-jump-items-internal returned: %s" ideal-dest))
    ideal-dest))

;;;###autoload
(defun evilmi-jump-items-native (&optional num)
  "Jump between items NUM times."
  (interactive "P")
  (evilmi-jump-items-internal num))

(defun evilmi--push-mark (position)
  "Pus POSITION as marker."
  (push-mark (nth 0 position) t t))

(defun evilmi--convert-rules (rules)
  "Convert RULES to function pairs list."
  (let* (rlt)
    (dolist (rule rules)
      (let* ((rule-filename (concat "evil-matchit-" (symbol-name rule)))
             (fn-prefix (concat "evilmi-" (symbol-name rule)))
             (get-tag-function (intern (concat fn-prefix "-get-tag")))
             (jump-function (intern (concat fn-prefix "-jump"))))

        ;; functions might be defined in another file
        (unless (and (fboundp get-tag-function) (fboundp jump-function))
          (autoload get-tag-function rule-filename nil)
          (autoload jump-function rule-filename nil))

        ;; load function
        (push (list get-tag-function jump-function) rlt)))

    (nreverse rlt)))

;;;###autoload
(defun evilmi-load-plugin-rules(modes rules)
  "Load MODES's plugin RULES."
  (dolist (mode modes)
    (setq evilmi-plugins (plist-put evilmi-plugins
                                    mode
                                    (evilmi--convert-rules rules)))))

;;;###autoload
(defun evilmi-init-plugins ()
  "Load plugins."
  (interactive)

  ;; rules for `prog-mode'
  (evilmi-load-plugin-rules '(prog-mode) '(prog))

  ;; simple matching for languages containing brackets
  (evilmi-load-plugin-rules '(java-mode perl-mode cperl-mode go-mode)
                            '(simple))

  ;; Javascript/Typescript
  (evilmi-load-plugin-rules '(js-mode
                              json-mode
                              js2-mode
                              js3-mode
                              javascript-mode
                              rjsx-mode
                              js2-jsx-mode
                              react-mode
                              typescript-mode
                              typescript-tsx-mode
                              tsx-ts-mode)
                            '(simple javascript html))

  ;; Html
  (evilmi-load-plugin-rules '(web-mode
                              html-mode
                              nxml-mode
                              nxhtml-mode
                              sgml-mode
                              php-mode
                              message-mode
                              mhtml-mode)
                            '(template simple html))

  ;; Emacs Org-mode
  (evilmi-load-plugin-rules '(org-mode) '(simple org))

  ;; Markdown
  (evilmi-load-plugin-rules '(markdown-mode) '(markdown))

  ;; Latex
  (evilmi-load-plugin-rules '(latex-mode) '(latex simple))

  ;; Ocaml
  (evilmi-load-plugin-rules '(tuareg-mode) '(simple ocaml))

  ;; Octave
  (evilmi-load-plugin-rules '(octave-mode) '(simple octave))
  (evilmi-load-plugin-rules '(matlab-mode) '(simple octave))

  ;; Python
  (evilmi-load-plugin-rules '(python-mode) '(simple python))

  ;; Yaml
  (evilmi-load-plugin-rules '(yaml-mode) '(simple yaml))

  ;; SQL
  (evilmi-load-plugin-rules '(sql-mode) '(simple sql))

  ;; C/C++
  (evilmi-load-plugin-rules '(c-mode c++-mode) '(c simple))

  ;; Diff/Patch
  (evilmi-load-plugin-rules '(diff-mode ffip-diff-mode magit-diff-mode)
                            '(simple diff))

  ;; Fortran
  (evilmi-load-plugin-rules '(f90-mode fortran-mode) '(fortran))

  ;; CMake (http://www.cmake.org)
  (evilmi-load-plugin-rules '(cmake-mode) '(cmake))

  ;; sh-mode
  (evilmi-load-plugin-rules '(sh-mode) '(sh))

  ;; verilog-mode
  (evilmi-load-plugin-rules '(verilog-mode) '(verilog))

  ;; Lua or script
  (evilmi-load-plugin-rules '(lua-mode vimrc-mode) '(simple script))

  ;; css/scss/less
  (evilmi-load-plugin-rules '(css-mode less-mode scss-mode) '(simple))

  ;; Ruby
  (evilmi-load-plugin-rules '(ruby-mode enh-ruby-mode) '(simple ruby))

  ;; terminal
  (evilmi-load-plugin-rules '(term-mode shell-mode) '(simple terminal))

  ;; Elixir
  (evilmi-load-plugin-rules '(elixir-mode elixir-ts-mode enh-elixir-mode) '(simple elixir)))


(defun evilmi--region-to-select-or-delete (num &optional is-inner)
  "Get NUM region(s) to select or delete.
If IS-INNER is t, the region is inner text object."
  (let* (ideal-dest b e)
    (save-excursion
      (setq ideal-dest (evilmi-jump-items-internal num #'evilmi--push-mark))
      (if ideal-dest (goto-char ideal-dest))
      (setq b (region-beginning))
      (setq e (region-end))
      (goto-char b)

      ;; for inner text object, forward a line at the beginning
      (cond
       (is-inner
        (forward-line 1)
        (setq b (line-beginning-position)))
       (t
        (if (string-match "[ \t]*" (buffer-substring-no-properties (line-beginning-position) b))
            (setq b (line-beginning-position)))))

      ;; for inner text object, backward a line at the end
      ;; but in python-mode, last line is also code line
      (when (and is-inner (not (eq major-mode 'python-mode)))
        (goto-char e)
        (forward-line -1)
        (setq e (line-end-position))))

    (if evilmi-debug (message "evilmi--region-to-select-or-delete called. Return: %s" (list b e)))
    (list b e)))

;;;###autoload
(defun evilmi-select-items (&optional num)
  "Select NUM items/tags and the region between them."
  (interactive "p")
  (let* ((selected-region (evilmi--region-to-select-or-delete num)))
    (when selected-region
      (evilmi--push-mark selected-region)
      (goto-char (cadr selected-region)))))

;;;###autoload
(defun evilmi-delete-items (&optional num)
  "Delete NUM items/tags and the region between them."
  (interactive "p")
  (let* ((selected-region (evilmi--region-to-select-or-delete num)))
    ;; 1+ because the line feed
    (kill-region (car selected-region) (1+ (cadr selected-region)))))

;;;###autoload
(defun evilmi-version()
  "Print version."
  (interactive)
  (message "3.0.2"))

;; initialize evilmi-plugins only once
(evilmi-init-plugins)

(with-eval-after-load 'evil
  (require 'evil-matchit-evil-setup))

(provide 'evil-matchit)
;;; evil-matchit.el ends here
