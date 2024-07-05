(defun spacemacs/whisper-select-language ()
  "Prompt the user to select a language for whisper, showing the current selection."
  (interactive)
  (let* ((languages '("en" "es" "fr" "de" "it" "pt" "ru" "zh" "ja" "ko" "auto"))
         (current (or whisper-language "Not set"))
         (prompt (format "Current language: %s\nSelect language: "
                         (propertize current 'face 'success))))
    (setq whisper-language (completing-read prompt languages nil t nil nil current))))

(defun spacemacs/whisper-select-model ()
  "Prompt the user to select a base model for whisper, showing the current selection."
  (interactive)
  (let* ((all-models '("tiny.en" "tiny" "base.en" "base" "small.en" "small"
                       "medium.en" "medium" "large-v1" "large-v2" "large-v3"))
         (filtered-models (if (string= whisper-language "en")
                              all-models
                            (seq-filter (lambda (m) (not (string-suffix-p ".en" m))) all-models)))
         (current (or whisper-model "Not set"))
         (prompt (format "Current model: %s\nSelect base model: "
                         (propertize current 'face 'success))))
    (setq whisper-model (completing-read prompt filtered-models nil t nil nil current))))
