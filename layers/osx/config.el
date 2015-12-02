(defvar osx-use-option-as-meta t
  "If non nil the option key is mapped to meta. Set to `nil` if you need the
  option key to type common characters.")

;; Use the OS X Emoji font for Emoticons
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend))
