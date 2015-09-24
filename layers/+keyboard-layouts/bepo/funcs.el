;;; funcs.el --- bepo Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Fabien Dubosson & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Map multiple states at once. Courtesy of Michael Markert;
;; http://permalink.gmane.org/gmane.emacs.vim-emulation/1674

(defun bepo//define-key (maps key def bindings)
  "Define a list of keys with their associated bindings in the
given state maps."
  (while key
    (dolist (map maps)
      (define-key map key def))
    (setq key (pop bindings)
          def (pop bindings))))

(defun bepo/set-in-state (map key def &rest bindings)
  "Define a list of keys with their associated bindings in a given state map." 
  (declare (indent 1))
  (bepo//define-key (list map) key def bindings))

(defun bepo/set-in-states (maps key def &rest bindings)
  "Define a list of keys with their associated bindings in given state maps."
  (declare (indent 1))
  (bepo//define-key maps key def bindings))

(defun bepo/set-in-all-evil-states (key def &rest bindings)
  "Define a list of keys with their associated bindings in all states." 
  (declare (indent 0))
  (bepo//define-key (list evil-normal-state-map
                          evil-visual-state-map
                          evil-insert-state-map
                          evil-emacs-state-map
                          evil-motion-state-map)
                    key def bindings))

(defun bepo/set-in-all-evil-states-but-insert (key def &rest bindings)
  "Define a list of keys with their associated bindings in all states except
insert."
  (declare (indent 0))
  (bepo//define-key (list evil-normal-state-map
                          evil-visual-state-map
                          evil-emacs-state-map
                          evil-motion-state-map)
                    key def bindings))

(defun bepo//replace-in-list-rec (lst elem repl)
  "Replace `elem' in the list `lst' recursively and replace it with `repl'."
  (if (typep lst 'list)
      (let* ((body-position (cl-position elem lst)))
        (if body-position
            ;; The element is in the list, replace it
            (progn
              (setf (nth body-position lst) repl)
              lst)
          ;; The element is not in the list, recurse
        (dolist (l lst)
            (bepo//replace-in-list-rec l elem repl))))))

(defmacro bepo|rebind (&optional name &rest props)
  "Macro used for structuring the keys rebinding."
  (declare (indent 1))
  (let* ((description (plist-get props :description))
         (functions (plist-get props :functions))
         (loader (plist-get props :loader))
         (remap (plist-get props :remap))
         (switch (plist-get props :switch))
         (additional (plist-get props :additional))
         (special (plist-get props :special))
         (mappings (list 'progn))
         body)
    ;; Prepare the mappings
    (if remap
        (nconc mappings (list remap)))
    (if switch
        (nconc mappings (list switch)))
    (if additional
        (nconc mappings (list additional)))
    (setq body mappings)
    ;; Use loader if defined
    (if loader
        (progn
          (bepo//replace-in-list-rec loader 'BODY mappings)
          (setq body loader)))
    ;; Generate the code
    `(progn
       ,description
       ,functions
       ,body
       ,special
       ,name
       )))

(defmacro bepo|config (&optional name &rest props)
  "Macro used for structuring the configuration changes."
  (declare (indent 1))
  (let* ((description (plist-get props :description))
         (functions (plist-get props :functions))
         (loader (plist-get props :loader))
         (config (plist-get props :config))
         (special (plist-get props :special))
         (body config))
    ;; Use loader if defined
    (if loader
        (progn
          (bepo//replace-in-list-rec loader 'BODY config)
          (setq body loader)))
    ;; Generate the code
    `(progn
       ,description
       ,functions
       ,body
       ,special
       ,name
       )))
