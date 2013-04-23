(require 'solarized)

(deftheme solarized-dark "The dark variant of the Solarized colour theme")
(create-solarized-theme 'dark 'solarized-dark)
(provide-theme 'solarized-dark)

(deftheme solarized-light "The light variant of the Solarized colour theme")
(create-solarized-theme 'light 'solarized-light)
(provide-theme 'solarized-light)

(if (equal system-name "QUE-WKS-AA427-Linux")
    (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t))
