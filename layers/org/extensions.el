;;; extensions.el --- Org Extensions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq org-post-extensions '(ox-gfm))

(defun org/init-ox-gfm ()
  ;; installing this package from melpa is buggy,
  ;; so we install it as an extension for now.
  (use-package ox-gfm
    :if org-enable-github-support
    :defer t
    :init
    (progn
      ;; seems to be required otherwise the extension is not
      ;; loaded properly by org
      (with-eval-after-load 'org (require 'ox-gfm))
      (autoload 'org-gfm-export-as-markdown "ox-gfm" "\
 Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org GFM Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

      (autoload 'org-gfm-convert-region-to-md "ox-gfm" "\
Assume the current region has org-mode syntax, and convert it
to Github Flavored Markdown.  This can be used in any buffer.
For example, you can write an itemized list in org-mode syntax in
a Markdown buffer and use this command to convert it.

\(fn)" t nil))))
