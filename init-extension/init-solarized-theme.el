(use-package solarized
  :init
  (progn
    (deftheme solarized-dark "The dark variant of the Solarized colour theme")
    (create-solarized-theme 'dark 'solarized-dark)
    (provide-theme 'solarized-dark)

    (deftheme solarized-light "The light variant of the Solarized colour theme")
    (create-solarized-theme 'light 'solarized-light)
    (provide-theme 'solarized-light))
  )

