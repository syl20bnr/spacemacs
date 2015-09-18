;;; core-keybindings.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defvar spacemacs/prefix-titles nil
  "alist for mapping command prefixes to long names.")

(defun spacemacs/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `spacemacs/prefix-titles'."
  (let* ((command name)
         (full-prefix (concat dotspacemacs-leader-key " " prefix))
         (full-prefix-emacs (concat dotspacemacs-emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence
                                 (kbd full-prefix-emacs))))
    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (if (fboundp 'which-key-declare-prefixes)
        (which-key-declare-prefixes
          full-prefix-emacs (cons name long-name)
          full-prefix (cons name long-name))
      (unless (lookup-key evil-leader--default-map prefix)
        (define-prefix-command (intern command))
        (evil-leader/set-key prefix (intern command))
        (push (cons full-prefix-lst long-name) spacemacs/prefix-titles)
        (push (cons full-prefix-emacs-lst long-name) spacemacs/prefix-titles)))))

(defun spacemacs/declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let  ((command (intern (concat (symbol-name mode) name)))
         (full-prefix (concat dotspacemacs-leader-key " " prefix))
         (full-prefix-emacs (concat dotspacemacs-emacs-leader-key " " prefix))
         (is-major-mode-prefix (string-prefix-p "m" prefix))
         (major-mode-prefix (concat dotspacemacs-major-mode-leader-key " " (substring prefix 1)))
         (major-mode-prefix-emacs (concat dotspacemacs-major-mode-emacs-leader-key " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (if (fboundp 'which-key-declare-prefixes-for-mode)
          (progn
            (which-key-declare-prefixes-for-mode mode
              full-prefix-emacs prefix-name
              full-prefix prefix-name)
            (when is-major-mode-prefix
              (which-key-declare-prefixes-for-mode mode
                major-mode-prefix prefix-name
                major-mode-prefix-emacs prefix-name)))
        (define-prefix-command command)
        (evil-leader/set-key-for-mode mode prefix command)))))

(define-minor-mode spacemacs-additional-leader-mode ()
  "This mode follows the design of `evil-leader-mode' and
complements it by added additional leader keys."
  :init-value nil
  :keymap nil
  (let* ((mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
         (root-map (when spacemacs-additional-leader-mode
                     (or mode-map evil-leader--default-map)))
         (major-mode-map (when (and spacemacs-additional-leader-mode
                                    mode-map)
                           (lookup-key mode-map (kbd "m"))))
         (state-maps '(evil-normal-state-local-map
                       evil-motion-state-local-map
                       evil-visual-state-local-map))
         (emacs-state-maps '(evil-emacs-state-local-map
                             evil-insert-state-local-map
                             evil-normal-state-local-map
                             evil-motion-state-local-map
                             evil-visual-state-local-map
                             evil-hybrid-state-local-map)))
    (dolist (state-map state-maps)
      (when state-map
        (setq state-map (eval state-map))
        (when dotspacemacs-major-mode-leader-key
          (define-key state-map
            (kbd dotspacemacs-major-mode-leader-key) major-mode-map))))
    (dolist (state-map emacs-state-maps)
      (when state-map
        (setq state-map (eval state-map))
        (when dotspacemacs-major-mode-emacs-leader-key
          (define-key state-map
            (kbd dotspacemacs-major-mode-emacs-leader-key) major-mode-map))
        (define-key state-map (kbd dotspacemacs-emacs-leader-key) root-map)))))

(provide 'core-keybindings)
