(configuration-layer/declare-layers '(auto-completion lua syntax-checking))

(spacemacs|defvar-company-backends lean-mode)
(push 'lean-company company-backends-lean-mode)
