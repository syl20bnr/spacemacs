;; Layer local yasnippets
(spacemacs|use-package-add-hook yasnippet
  :post-config
    (add-to-list 'yas-snippet-dirs (concat configuration-layer-directory "+tools/cfengine/snippets/"))
    (yas-load-directory (concat configuration-layer-directory "+tools/cfengine/snippets/")))
