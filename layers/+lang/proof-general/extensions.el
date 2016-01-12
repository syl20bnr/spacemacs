(defvar proof-general-post-extensions '(proof-general))

(defun proof-general/init-proof-general ()
  "Initialize Proof General"

  (unless (executable-find "proofgeneral")
    (spacemacs/buffer/warning
     (concat "ProofGeneral not detected, be sure that ProofGeneral binaries are "
             "available in your PATH or check the installation instructions in "
             "the README file.")))

  (use-package proof-site
    :if (executable-find "proofgeneral")
    :defer t
    :mode ("\\.v\\'" . coq-mode)
    :load-path "/usr/local/ProofGeneral/generic"
    :config (progn
              (evil-leader/set-key-for-mode 'coq-mode
                "ms" 'proof-toggle-active-scripting
                "m." 'proof-electric-terminator-toggle))))
