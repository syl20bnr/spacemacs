(defconst minuet-packages
  '(
    ;; Get the package from MELPA, ELPA, etc.
    (minuet :location (recipe
                       :fetcher github
                       :repo "golden0080/minuet-ai.el"
                       :branch "f1-multi-accept"))
    (plz :location elpa)

    (dash :location elpa)

    (cl-lib :location elpa)

    ;; (minuet :location elpa)
    ))

(defun minuet/init-minuet ()
  (use-package minuet
    :config
    (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64)
    (plist-put (symbol-value (intern (format "minuet-%s-options" minuet-provider))) :api-key (defun minuet--api-key () minuet-provider-api-key))

    ;; configure active mode keybindings
    (define-key minuet-active-mode-map (kbd "C-j") 'minuet-next-suggestion)
    (define-key minuet-active-mode-map (kbd "C-k") 'minuet-previous-suggestion)
    (define-key minuet-active-mode-map (kbd "C-l") 'minuet-accept-suggestion-line-continue)
    (define-key minuet-active-mode-map (kbd "TAB") 'minuet-accept-suggestion)
    (define-key minuet-active-mode-map (kbd "C-h") 'minuet-dismiss-suggestion)
    (define-key minuet-active-mode-map (kbd "C-g") 'minuet-dismiss-suggestion)
    ))
