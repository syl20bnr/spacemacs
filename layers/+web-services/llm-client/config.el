(defvar llm-client--gptel-send-called nil
  "Flag to track if gptel-send has been called at least once.")

(defcustom llm-client-enable-gptel t
  "If non-nil, enable the =gptel= package."
  :type 'boolean
  :group 'llm-client)

(defcustom llm-client-enable-ellama t
  "If non-nil, enable the =ellama= package."
  :type 'boolean
  :group 'llm-client)
