;;; paradox.el --- A modern Packages Menu. Colored, with package ratings, and customizable. -*- lexical-binding:t -*-

;; Copyright (C) 2014-2015 Artur Malabarba <emacs@endlessparentheses.com>

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: https://github.com/Malabarba/paradox
;; Version: 2.5.5
;; Keywords: package packages
;; Package-Requires: ((emacs "24.4") (seq "1.7") (let-alist "1.0.3") (spinner "1.7.3") (hydra "0.13.2"))
;; Prefix: paradox
;; Separator: -

;;; Commentary:
;;
;; Paradox can be installed from Melpa with M-x `package-install' RET
;; paradox.
;; It can also be installed manually in the usual way, just be mindful of
;; the dependencies.
;;
;; To use it, simply call M-x `paradox-list-packages' (instead of the
;; regular `list-packages').
;; This will give you most features out of the box. If you want to be
;; able to star packages as well, just configure the
;; `paradox-github-token' variable then call `paradox-list-packages'
;; again.
;;
;; If you'd like to stop using Paradox, you may call `paradox-disable'
;; and go back to using the regular `list-packages'.
;;
;; ## Current Features ##
;;
;; ### Several Improvements ###
;;
;; Paradox implements many small improvements to the package menu
;; itself. They all work out of the box and are completely customizable!
;; *(Also, hit `h' to see all keys.)*
;;
;; * Visit the package's homepage with `v' (or just use the provided buttons).
;; * Shortcuts for package filtering:
;;     * <f r> filters by regexp (`occur');
;;     * <f u> display only packages with upgrades;
;;     * <f k> filters by keyword.
;; * `hl-line-mode' enabled by default.
;; * Display useful information on the mode-line and cleanup a bunch of
;;   useless stuff.
;; * **Customization!** Just call M-x `paradox-customize' to see what you can
;;   do.
;;     * Customize column widths.
;;     * Customize faces (`paradox-star-face', `paradox-status-face-alist' and `paradox-archive-face').
;;     * Customize local variables.
;;
;; ### Package Ratings ###
;;
;; Paradox also integrates with
;; **GitHub Stars**, which works as **rough** package rating system.
;; That is, Paradox package menu will:
;;
;; 1. Display the number of GitHub Stars each package has (assuming it's
;;    in a github repo, of course);
;; 2. Possibly automatically star packages you install, and unstar
;;    packages you delete (you will be asked the first time whether you
;;    want this);
;; 3. Let you star and unstar packages by hitting the `s' key;
;; 4. Let you star all packages you have installed with M-x `paradox-star-all-installed-packages'.
;;
;; Item **1.** will work out of the box, the other items obviously
;; require a github account (Paradox will help you generate a token the
;; first time you call `paradox-list-packages').
;;
;; ## How Star Displaying Works ##
;;
;; We generate a map of Package Name -> Repository from
;; [Melpa](https://github.com/milkypostman/melpa.git)'s `recipe'
;; directory, some repos may correspond to more than one package.
;; This map is used count the stars a given package has.
;; _This doesn't mean you need Melpa to see the star counts, the numbers
;; will be displayed regardless of what archives you use._
;;
;; Currently, packages that are not hosted on GitHub are listed with a
;; blank star count, which is clearly different from 0-star packages
;; (which are displayed with a 0, obviously).
;; If you know of an alternative that could be used for these packages,
;; [open an issue](https://github.com/Bruce-Connor/paradox/issues/new)
;; here, I'd love to hear.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.


;;; Code:

(require 'package)
(require 'cl-lib)

(require 'paradox-core)
(require 'paradox-execute)
(require 'paradox-menu)

(defconst paradox-version "2.5.4" "Version of the paradox.el package.")
(defun paradox-bug-report ()
  "Opens github issues page in a web browser.  Please send any bugs you find.
Please include your Emacs and paradox versions."
  (interactive)
  (message "Your paradox-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
    paradox-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/paradox/issues/new"))
(defun paradox-customize ()
  "Open the customization menu in the `paradox' group."
  (interactive)
  (customize-group 'paradox t))
(defgroup paradox nil
  "Customization group for paradox."
  :prefix "paradox-"
  :group 'emacs
  :package-version '(paradox . "0.1"))


;;; External Commands
;;;###autoload
(defun paradox-list-packages (no-fetch)
  "Improved version of `package-list-packages'.  The heart of Paradox.
Function is equivalent to `package-list-packages' (including the
prefix NO-FETCH), but the resulting Package Menu is improved in
several ways.

Among them:

1. Uses `paradox-menu-mode', which has more functionality and
keybinds than `package-menu-mode'.

2. Uses some font-locking to improve readability.

3. Optionally shows the number GitHub stars and Melpa downloads
for packages.

4. Adds useful information in the mode-line."
  (interactive "P")
  (when (paradox--check-github-token)
    (paradox-enable)
    (let ((is-25 (fboundp 'package--with-response-buffer)))
      (unless no-fetch
        (unless is-25
          (paradox--refresh-remote-data)))
      (package-list-packages no-fetch)
      (unless no-fetch
        (when is-25
          (add-to-list 'package--downloads-in-progress 'paradox--data)
          (paradox--refresh-remote-data))
        (when (stringp paradox-github-token)
          (paradox--refresh-user-starred-list
           (bound-and-true-p package-menu-async)))))))

;;;###autoload
(defun paradox-upgrade-packages (&optional no-fetch)
  "Upgrade all packages.  No questions asked.
This function is equivalent to `list-packages', followed by a
`package-menu-mark-upgrades' and a `package-menu-execute'.  Except
the user isn't asked to confirm deletion of packages.

If `paradox-execute-asynchronously' is non-nil, part of this
operation may be performed in the background.

The NO-FETCH prefix argument is passed to `list-packages'.  It
prevents re-download of information about new versions.  It does
not prevent downloading the actual packages (obviously)."
  (interactive "P")
  (save-window-excursion
    (let ((package-menu-async nil))
      (paradox-list-packages no-fetch))
    (package-menu-mark-upgrades)
    (paradox-menu-execute 'noquery)))

;;;###autoload
(defun paradox-enable ()
  "Enable paradox, overriding the default package-menu."
  (interactive)
  (when (and (fboundp 'package--update-downloads-in-progress)
             (not (fboundp 'package--with-response-buffer)))
    (message "[Paradox] Your Emacs snapshot is outdated, please install a more recent one.")
    (setq package-menu-async nil))
  (paradox--override-definition 'package-menu--print-info 'paradox--print-info)
  (when (fboundp 'package-menu--print-info-simple)
    (paradox--override-definition 'package-menu--print-info-simple 'paradox--print-info))
  (paradox--override-definition 'package-menu--generate 'paradox--generate-menu)
  ;; Tough it may not look like it, this is totally necessary too.
  (paradox--override-definition 'package-menu-mode 'paradox-menu-mode)
  (paradox--core-enable))

;;;###autoload
(defun paradox-require (feature &optional filename noerror package refresh)
  "Like `require', but also install FEATURE if it is absent.
FILENAME is passed to `require'.
If NOERROR is non-nil, don't complain if the feature couldn't be
installed, just return nil.

- If FEATURE is present, `require' it and return t.

- If FEATURE is not present, install PACKAGE with `package-install'.
If PACKAGE is nil, assume FEATURE is the package name.
After installation, `require' FEATURE.

By default, the current package database is only updated if it is
empty.  Passing a non-nil REFRESH argument forces this update."
  (or (require feature filename t)
      (let ((package (or package
                         (if (stringp feature)
                             (intern feature)
                           feature))))
        (require 'package)
        (unless (and package-archive-contents (null refresh))
          (package-refresh-contents))
        (and (condition-case e
                 (package-install package)
               (error (if noerror nil (error (cadr e)))))
             (require feature filename noerror)))))

(provide 'paradox)
;;; paradox.el ends here
