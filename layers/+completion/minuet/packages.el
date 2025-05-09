(defconst minuet-packages
  '(
    ;; Get the package from MELPA, ELPA, etc.
    (minuet :location (recipe
                       :fetcher github
                       :repo "milanglacier/minuet-ai.el"))
    (plz :location elpa)

    (dash :location elpa)

    (cl-lib :location elpa)

    ;; (minuet :location elpa)
    ))

(defun minuet/init-minuet ()
  (use-package minuet
    :config
    (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64)
    (plist-put (intern (format "minuet-%s-options" minuet-provider)) :api-key #'(defun minuet--api-key () minuet-provider-api-key))
    ;; (message "DEBUG: minuet-openai-fim-compatible-options is %S" minuet-openai-fim-compatible-options)
    )
  )
