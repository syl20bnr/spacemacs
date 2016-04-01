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

(defun spacemacs/activate-major-mode-leader ()
  "Bind major mode key map to `dotspacemacs-major-mode-leader-key'."
  (let* ((mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
         (major-mode-map (when mode-map (lookup-key mode-map (kbd "m")))))
    (when major-mode-map
      (mapc (lambda (s)
              (eval `(define-key
                       ,(intern (format "evil-%S-state-local-map" s))
                       ,(kbd dotspacemacs-major-mode-leader-key)
                       major-mode-map)))
            '(normal motion))
      (mapc (lambda (s)
              (eval `(define-key
                       ,(intern (format "evil-%S-state-local-map" s))
                       ,(kbd dotspacemacs-major-mode-emacs-leader-key)
                       major-mode-map)))
            '(emacs insert normal motion visual))
      ;; using `bound-and-true-p', because hybrid-mode may not be loaded when
      ;; using emacs or vim style
      (when (bound-and-true-p hybrid-mode)
        (define-key evil-hybrid-state-map
          (kbd dotspacemacs-major-mode-emacs-leader-key)
          major-mode-map)))))

(provide 'core-keybindings)
