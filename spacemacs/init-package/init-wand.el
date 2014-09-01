(use-package wand
  :disabled t
  :init
  (progn
    (require 'wand)
    (wand:add-rule (wand:create-rule :match "https?://"
                                     :capture :whole
                                     :action message))))
