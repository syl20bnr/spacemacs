



(defun spacemacs/state-color-face (state)
  "Return the symbol of the face for the given STATE."
  (intern (format "spacemacs-%s-face" (symbol-name state))))

(defun spacemacs/state-color (state)
  "Return the color string associated to STATE."
  (face-background (spacemacs/state-color-face state)))

(defun spacemacs/current-state-color ()
  "Return the color string associated to the current state."
  (face-background (spacemacs/state-color-face evil-state)))

(defun spacemacs/state-face (state)
  "Return the face associated to the STATE."
  (spacemacs/state-color-face state))

(defun spacemacs/current-state-face ()
  "Return the face associated to the current state."
  (let ((state (if (eq evil-state 'operator)
                    evil-previous-state
                  evil-state)))
    (spacemacs/state-color-face state)))

(defun evil-insert-state-cursor-hide ()
  (setq evil-insert-state-cursor '((hbar . 0))))

(defun spacemacs/evil-smart-doc-lookup ()
  "Version of `evil-lookup' that attempts to use
        the mode specific goto-definition binding,
        i.e. `SPC m h h`, to lookup the source of the definition,
        while falling back to `evil-lookup'"
  (interactive)
  (let ((binding (key-binding (kbd (concat dotspacemacs-leader-key " mhh")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-lookup))))

(defun spacemacs/evil-smart-goto-definition ()
  "Version of `evil-goto-definition' that attempts to use
        the mode specific goto-definition binding,
        i.e. `SPC m g g`, to lookup the source of the definition,
        while falling back to `evil-goto-definition'"
  (interactive)
  (let ((binding (key-binding (kbd (concat dotspacemacs-leader-key " mgg")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-goto-definition))))

(defmacro spacemacs|define-text-object (key name start end)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name)))
        (start-regex (regexp-opt (list start)))
        (end-regex (regexp-opt (list end))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name))
       (with-eval-after-load 'evil-surround
         (push (cons (string-to-char ,key)
                     (if ,end
                         (cons ,start ,end)
                       ,start))
               evil-surround-pairs-alist)))))

(defmacro evil-map (state key seq)
  "Map for a given STATE a KEY to a sequence SEQ of keys.

Can handle recursive definition only if KEY is the first key of SEQ.
Example: (evil-map visual \"<\" \"<gv\")"
  (let ((map (intern (format "evil-%S-state-map" state))))
    `(define-key ,map ,key
       (lambda ()
         (interactive)
         ,(if (string-equal key (substring seq 0 1))
              `(progn
                 (call-interactively ',(lookup-key evil-normal-state-map key))
                 (execute-kbd-macro ,(substring seq 1)))
            (execute-kbd-macro ,seq))))))



(defun spacemacs//hydra-key-doc-function (key key-width doc doc-width)
  "Custom hint documentation format for keys."
  (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
          key doc))
