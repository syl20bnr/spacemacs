(require 'solarized)

(deftheme solarized-dark "The dark variant of the Solarized colour theme")
(create-solarized-theme 'dark 'solarized-dark)
(provide-theme 'solarized-dark)

(deftheme solarized-light "The light variant of the Solarized colour theme")
(create-solarized-theme 'light 'solarized-light)
(provide-theme 'solarized-light)

(if (or (equal system-name "QUE-WKS-AA427-Linux")
        (equal system-name "QUE-WKS-AA593"))
    (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t))
