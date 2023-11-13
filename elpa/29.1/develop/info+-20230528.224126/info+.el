;;; info+.el --- Extensions to `info.el'.     -*- coding:utf-8 -*-
;;
;; Filename: info+.el
;; Description: Extensions to `info.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2022, Drew Adams, all rights reserved.
;; Created: Tue Sep 12 16:30:11 1995
;; Version: 0
;; Package-Version: 20230528.224126
;; Package-Requires: ()
;; Last-Updated: Sun May 28 19:39:43 2023 (-0700)
;;           By: dradams
;;     Update #: 7532
;; URL: https://www.emacswiki.org/emacs/download/info%2b.el
;; Doc URL: https://www.emacswiki.org/emacs/InfoPlus
;; Keywords: help, docs, internal
;; Compatibility: GNU Emacs: 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `auth-source', `avoid', `backquote',
;;   `bookmark', `bookmark+', `bookmark+-1', `bookmark+-bmu',
;;   `bookmark+-key', `bookmark+-lit', `button', `bytecomp', `cconv',
;;   `cl', `cl-generic', `cl-lib', `cl-macs', `cmds-menu',
;;   `col-highlight', `crosshairs', `eieio', `eieio-core',
;;   `eieio-loaddefs', `epg-config', `fit-frame', `font-lock',
;;   `font-lock+', `frame-fns', `gv', `help+', `help-fns',
;;   `help-fns+', `help-macro', `help-macro+', `help-mode',
;;   `hl-line', `hl-line+', `info', `info+', `kmacro', `macroexp',
;;   `menu-bar', `menu-bar+', `misc-cmds', `misc-fns', `naked',
;;   `package', `password-cache', `pp', `pp+', `radix-tree', `rect',
;;   `replace', `second-sel', `seq', `strings', `syntax',
;;   `tabulated-list', `text-mode', `thingatpt', `thingatpt+',
;;   `url-handlers', `url-parse', `url-vars', `vline',
;;   `w32browser-dlgopen', `wid-edit', `wid-edit+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `info.el'.
;;
;;  More description below.
;;
;;  If you use Emacs 20, 21, or 22 then use library `info+20.el'
;;  instead of `info+.el'.
 
;;(@> "Index")
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el', load `linkd.el' and turn on
;;  `linkd-mode' now.  It lets you easily navigate around the sections
;;  of this doc.  Linkd mode will highlight this Index, as well as the
;;  cross-references and section headings throughout this file.  You
;;  can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Things Defined Here")
;;  (@> "Documentation")
;;  (@> "Macros")
;;  (@> "Faces (Customizable)")
;;  (@> "User Options (Customizable)")
;;  (@> "Internal Variables")
;;  (@> "New Commands")
;;  (@> "Replacements for Existing Functions")
;;  (@> "Non-Interactive Functions")
 
;;(@* "Things Defined Here")
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Commands defined here:
;;
;;    `Info-breadcrumbs-in-mode-line-mode',
;;    `Info-change-visited-status' (Emacs 24+),
;;    `Info-cycle-fontify-glossary-words',
;;    `Info-cycle-link-glossary-words',
;;    `Info-cycle-fontify-quotations', `Info-describe-bookmark' (Emacs
;;    24.2+), `Info-define-custom-delimiting',
;;    `Info-follow-nearest-node-new-window', `Info-glossary',
;;    `Info-goto-node-web', `Info-history-clear',
;;    `Info-make-node-unvisited', `info-manual',
;;    `info-manual+node-buffer-name-mode', `Info-merge-subnodes',
;;    `Info-mouse-follow-nearest-node-new-window',
;;    `Info-outline-demote', `Info-outline-promote',
;;    `Info-persist-history-mode' (Emacs 24.4+), `info-rename-buffer',
;;    `Info-save-current-node', `Info-search-case-sensitively-next',
;;    `Info-set-breadcrumbs-depth',
;;    `Info-set-face-for-bookmarked-xref' (Emacs 24.2+),
;;    `Info-toggle-breadcrumbs-in-header', `Info-toggle-fontify-all',
;;    `Info-toggle-fontify-angle-bracketed',
;;    `Info-toggle-fontify-bookmarked-xrefs' (Emacs 24.2+),
;;    `Info-toggle-fontify-custom-delimited',
;;    `Info-toggle-fontify-emphasis', `Info-toggle-fontify-extra',
;;    `Info-toggle-fontify-glossary-words',
;;    `Info-toggle-fontify-isolated-quote',
;;    `Info-toggle-fontify-local-angle-bracketed',
;;    `Info-toggle-fontify-local-angle-bracketed-same-line',
;;    `Info-toggle-fontify-local-custom-delimited',
;;    `Info-toggle-fontify-local-emphasis',
;;    `Info-toggle-fontify-local-isolated-backquote',
;;    `Info-toggle-fontify-local-isolated-quote',
;;    `Info-toggle-fontify-local-quotation',
;;    `Info-toggle-fontify-local-quotation-same-line',
;;    `Info-toggle-fontify-reference-items',
;;    `Info-toggle-fontify-visited-nodes',
;;    `Info-toggle-node-access-invokes-bookmark' (Emacs 24.4+),
;;    `Info-toc-outline', `Info-toc-outline-refontify-region',
;;    `Info-url-for-node', `Info-variable-pitch-text-mode',
;;    `Info-virtual-book'.
;;
;;  Faces defined here:
;;
;;    `info-command-ref-item', `info-constant-ref-item',
;;    `info-custom-delimited', `info-double-quoted-name',
;;    `info-emphasis', `info-file', `info-fixed-pitch',
;;    `info-function-ref-item', `info-glossary-word',
;;    `info-homoglyph', `info-indented-text',
;;    `info-isolated-backquote', `info-isolated-quote',
;;    `info-macro-ref-item', `info-menu', `info-node',
;;    `info-quoted-name', `info-reference-item',
;;    `info-special-form-ref-item', `info-string',
;;    `info-syntax-class-item', `info-user-option-ref-item',
;;    `info-variable-ref-item', `info-xref-bookmarked' (Emacs 24.2+).
;;
;;  Options (user variables) defined here:
;;
;;    `Info-apropos-manuals', `Info-bookmarked-node-xref-faces' (Emacs
;;    24.2+), `Info-bookmark-use-only-node-not-file-flag',
;;    `Info-breadcrumbs-in-header-flag', `info-buffer-name-function',
;;    `Info-display-node-header-fn', `Info-emphasis-regexp',
;;    `Info-fit-frame-flag', `Info-fontify-angle-bracketed-flag',
;;    `Info-fontify-bookmarked-xrefs-flag' (Emacs 24.2+),
;;    `Info-fontify-custom-delimited', `Info-fontify-emphasis-flag',
;;    `Info-fontify-extra-function', `Info-fontify-glossary-words',
;;    `Info-fontify-indented-text-chars',
;;    `Info-fontify-indented-text-manuals',
;;    `Info-fontify-isolated-quote-flag', `Info-fontify-quotations',
;;    `Info-fontify-reference-items-flag',
;;    `Info-glossary-fallbacks-alist', `Info-link-glossary-words',
;;    `Info-node-access-invokes-bookmark-flag' (Emacs 24.4+),
;;    `Info-saved-history-file' (Emacs 24.4+), `Info-saved-nodes',
;;    `Info-subtree-separator', `Info-toc-outline-no-redundancy-flag'.
;;
;;  Macros defined here:
;;
;;    `info-user-error'.
;;
;;  Non-interactive functions defined here:
;;
;;    `Info--manuals', `Info--member-string-nocase',
;;    `info--msg-Info-fontify-glossary-words-now',
;;    `Info--pop-to-buffer-same-window', `info--user-search-failed',
;;    `Info-bookmark-for-node', `Info-bookmark-name-at-point',
;;    `Info-bookmark-named-at-point', `Info-bookmark-name-for-node',
;;    `info-buffer-name-function-default',
;;    `Info-case-insensitive-string=',
;;    `Info-case-insensitive-string-hash', `info-custom-delim-1',
;;    `info-custom-delim-2', `Info-defontify',
;;    `Info-defontify-current-node',
;;    `Info-display-node-default-header',
;;    `info-fallback-manual-for-glossary',
;;    `Info-fontify-custom-delimited', `Info-fontify-glossary-words',
;;    `Info-fontify-indented-text', `info-fontifying-regexp',
;;    `Info-fontify-quotations', `Info-fontify-reference-items',
;;    `Info-get-glossary-hash-table-create',
;;    `Info-goto-glossary-definition', `info-indented-text-regexp',
;;    `Info-insert-breadcrumbs-in-mode-line', `Info-isearch-search-p',
;;    `Info-manual-string', `Info-manual-symbol',
;;    `Info-node-name-at-point', `Info-no-glossary-manuals',
;;    `Info-read-bookmarked-node-name', `Info-refontify-current-node',
;;    `Info-remap-default-face-to-variable-pitch',
;;    `Info-restore-history-list' (Emacs 24.4+),
;;    `Info-save-history-list' (Emacs 24.4+), `Info-search-beg',
;;    `Info-search-end', `Info-toc-outline-find-node',
;;    `Info-toc-outline-refontify-links', `Info-toggle-fontify-local'.
;;
;;  Internal variables defined here:
;;
;;    `Info-breadcrumbs-depth-internal',
;;    `info-custom-delimited-same-line-regexp',
;;    `info-fontify-emphasis', `Info-glossary-link-history',
;;    `info-glossary-link-map', `info-good-fixed-pitch-font-families',
;;    `info-indented-text-regexp', `info-isolated-backquote-regexp',
;;    `info-isolated-quote-regexp',
;;    `info-last-non-nil-fontify-extra-function',
;;    `info-last-non-nil-fontify-glossary-words', `Info-link-faces',
;;    `Info-merged-map', `Info-mode-syntax-table', `info-nomatch',
;;    `info-quotation-regexp', `info-quotation-same-line-regexp',
;;    `info-quoted+<>-regexp', `info-quoted+<>-same-line-regexp',
;;    `info-remap-default-face-cookie',
;;    `info-symbols-and-replacements' (Emacs <27),
;;    `Info-toc-outline-map'.
;;
;;
;;  ***** NOTE: The following standard faces defined in `info.el'
;;              have been REDEFINED HERE:
;;
;;  `info-title-1', `info-title-2', `info-title-3', `info-title-4'.
;;
;;
;;  ***** NOTE: The following standard functions defined in `info.el'
;;              have been REDEFINED or ADVISED HERE:
;;
;;  `info-apropos' - Apropos, not literal string, match by default.
;;                   Use other window if not already in Info.
;;  `Info-apropos-matches' - Added optional arg REGEXP-P.
;;  `Info-backward-node' - Prefix arg clones buffer.
;;  `Info-bookmark-jump' -
;;     Respect option `Info-bookmark-use-only-node-not-file-flag'.
;;  `Info-directory' - Prefix arg clones buffer.
;;  `info-display-manual' - Use completion to input manual name.
;;  `Info-find-emacs-command-nodes' - Added arg MSGP and message.
;;  `Info-find-file' - Handle virtual books.
;;  `Info-find-node', `Info-find-node-2' -
;;     Call `fit-frame' if `Info-fit-frame-flag'.
;;     Added optional arg NOMSG.
;;  `Info-fontify-node' -
;;     1. Show breadcrumbs in header line and/or mode line.
;;     2. File name in face `info-file'.
;;     3. Node names in face `info-node'.
;;     4. Menu items in face `info-menu'.
;;     5. Only 5th and 9th menu items have their `*' colored.
;;     6. Notes in face `info-xref'.
;;     7. If `Info-fontify-emphasis-flag', then fontify _..._.
;;     8. If `Info-fontify-quotations', then fontify ‘...’ or
;;        `...' in face `info-quoted-name', “...” in face
;;        `info-double-quoted-name',  and "..." in face `info-string'.
;;     9. If `Info-fontify-quotations' and
;;        `Info-fontify-angle-bracketed-flag' then fontify <...> in
;;        face `info-quoted-name'.
;;    10. If `Info-fontify-quotations' and
;;        `Info-fontify-isolated-quote-flag' then fontify ' in face
;;        `info-isolated-quote', and fontify ` in face
;;        `info-isolated-backquote'.
;;  `Info-forward-node' - Prefix arg clones buffer.
;;  `Info-goto-emacs-command-node' -
;;     1. Uses `completing-read' in interactive spec, with,
;;        as default, `symbol-nearest-point'.
;;     2. Added optional arg MSGP.
;;     3. Message if single node found.
;;     4. Returns `num-matches' if found; nil if not.
;;  `Info-goto-emacs-key-command-node' -
;;     1. Added optional arg MSGP.
;;     2. If key's command not found, then `Info-search's for key
;;        sequence in text and displays message about repeating.
;;  `Info-goto-node' - Respect option
;;     `Info-node-access-invokes-bookmark-flag' (Emacs 24.4+).
;;  `Info-history' - A prefix arg clears the history.
;;  `Info-history-back' - Prefix arg clones buffer.
;;  `Info-history-forward' - Prefix arg clones buffer.
;;  `Info-insert-dir' -
;;     Added optional arg NOMSG to inhibit showing progress msgs.
;;  `Info-isearch-search' - Respect restriction to active region.
;;  `Info-isearch-wrap' - Respect restriction to active region.
;;  `Info-mode' - Doc string shows all bindings.
;;  `Info-next' - Prefix arg clones buffer.
;;  `Info-prev' - Prefix arg clones buffer.
;;  `Info-read-node-name'   - Added optional arg DEFAULT.
;;  `Info-search' - 1. Fits frame.
;;                  2. Highlights found regexp if `search-highlight'.
;;  `Info--search-loop' - Use `Info-search-beg' and `Info-search-end'.
;;  `Info-set-mode-line' - Handles breadcrumbs in the mode line.
;;  `Info-mouse-follow-nearest-node' - With prefix arg, show node in
;;                                     a new Info buffer.
;;  `Info-up' - Prefix arg clones buffer.
;;
;;
;;  ***** NOTE: The following standard function
;;              has been REDEFINED HERE:
;;
;;  `outline-invisible-p' - Fixes Emacs bug #28080.
 
;;(@* "Documentation")
;;
;;  Documentation
;;  -------------
;;
;;  Library `info+.el' extends the standard Emacs library `info.el' in
;;  many ways.  It provides these features:
;;
;;  * Association of additional information (metadata) with Info
;;    nodes.  You do this by bookmarking the nodes.  Library Bookmark+
;;    gives you the following features in combination with `info+.el'.
;;    In many ways an Info node and its default bookmark can be
;;    thought of as the same animal.
;;
;;    - Rich node metadata.  In particular, you can tag nodes with any
;;      number of arbitrary tags, to classify them in different and
;;      overlapping ways.  You can also annotate them (in Org mode, by
;;      default).
;;
;;    - You can use `C-h C-b' to show the metadata for a (bookmarked)
;;      node.  This is all of the associated bookmark information,
;;      including the annotation and tags for that node and the number
;;      of times you have visited it.  If invoked with point on a
;;      link, the targeted node is described; otherwise, you are
;;      prompted for the node name.
;;
;;    - Links for bookmarked nodes can have a different face, to let
;;      you know that those nodes have associated metadata.  Option
;;      `Info-fontify-bookmarked-xrefs-flag' controls whether this is
;;      done.
;;
;;    - The face for this is `info-xref-bookmarked' by default, but
;;      you can set the face to use for a given Info bookmark using
;;      `C-x f' (command `Info-set-face-for-bookmarked-xref').  This
;;      gives you an easy way to classify nodes and show the class of
;;      a node by its links.  Uses faces to make clear which nodes are
;;      most important to you, or which are related to this or that
;;      general topic.
;;
;;    - You can use command `Info-make-node-unvisited' to make a node
;;      be considered unvisited.  By default the node at point is
;;      used.  For a node that is not bookmarked this also gives links
;;      to the node face `info-xref', which indicates that their
;;      targets are unvisited.  If for some reason you want to make
;;      links to a bookmarked node also have this face then just use
;;      command `Info-set-face-for-bookmarked-xref', specifying face
;;      `info-xref'.
;;
;;    - If option `Info-node-access-invokes-bookmark-flag' is non-nil
;;      then going to a bookmarked Info node invokes its bookmark, so
;;      that the node metadata (such as number of visits) gets
;;      updated.  Command `Info-toggle-node-access-invokes-bookmark'
;;      toggles the option value.
;;
;;    - You can automatically bookmark nodes you visit, by enabling
;;      mode `bmkp-info-auto-bookmark-mode'.  Toggle the mode off
;;      anytime you do not want to record Info visits.
;;
;;    - In the bookmark-list display (from `C-x r l') you can sort
;;      bookmarks by the time of last visit (`s d') or by the number
;;      of visits (`s v').  This gives you an easy way to see which
;;      parts of which Info manuals you have visited most recently and
;;      how much you have visited them.
;;
;;  * Editable, outline-enabled tables of contents (TOCs).  Command
;;    `Info-toc-outline' (bound to `O') opens a separate Info buffer
;;    showing the table of contents (TOC).  This is similar to the
;;    standard command `Info-toc' (bound to `T'), but the buffer is
;;    cloned from the manual and is in `outline-minor-mode'.  Also,
;;    there is no redundancy, by default: each TOC entry is listed
;;    only once, not multiple times.  (This is controlled by option
;;    `Info-toc-outline-no-redundancy-flag'.)
;;
;;    - You can have any number of such TOCs, for the same manual or
;;      for different manuals.
;;
;;    - Outline minor mode lets you hide and show, and promote and
;;      demote, various parts of the TOC tree for a manual.  And since
;;      the TOC is editable you can make other changes to it: sort
;;      parts of it, delete parts of it, duplicate parts of it, move
;;      parts aroundin an ad hoc way, and so on.  Info+ makes the
;;      outlining commands behave, so that hidden Info text
;;      (e.g. markup text such as `*note'...`::' surrounding links) is
;;      kept hidden.
;;
;;    - (Emacs 24.4+) Especially when combined with
;;      `Info-persist-history-mode', command
;;      `Info-change-visited-status' (`C-x DEL', see below), and the
;;      Info+ bookmarking enhancements (e.g., special link
;;      highlighting and persistently tracking the number of visits
;;      per node), `Info-toc-outline' gives you a way to organize
;;      access and visibility of a manual's nodes, to reflect how you
;;      use it.
;;
;;  * Additional, finer-grained Info highlighting.  This can make a
;;    big difference in readability.
;;
;;    - In the Emacs Lisp manual, reference items are highlighted, so
;;      they stand out.  This means: constants, commands, functions,
;;      macros, special forms, syntax classes, user options, and other
;;      variables.
;;
;;    - Single-quoted text, like `text' or ‘text’, and double-quoted
;;      text, like "text" or “text”, is highlighted if option
;;      `Info-fontify-quotations' is non-`nil'.  If the non-nil value
;;      is `t' (the default) then, for the case of `...', only text
;;      quoted on the same line is highlighted.  If the non-nil value
;;      is `multiline' then even multiline text quoted with `...' is
;;      highlighted.
;;
;;    - Angle-bracketed names, like <tab>, are highlighted if
;;      `Info-fontify-angle-bracketed-flag' and
;;      `Info-fontify-quotations' are both non-`nil'.
;;
;;    - Isolated single quotes and backquote chars, as in 'foobar and
;;      `foobar, are highlighted if `Info-fontify-quotations' and
;;      `Info-fontify-isolated-quote-flag' are both non-`nil'.
;;
;;    - Emphasized text, that is, text enclosed in underscore
;;      characters, like _this is emphasized text_, is
;;      highlighted if `Info-fontify-emphasis-flag' is non-`nil'.
;;      (But if internal variable `info-fontify-emphasis' is `nil'
;;      then there is no such highlighting, and that option has no
;;      effect.)
;;
;;    - Glossary words, that is, words that are defined in a manual's
;;      `Glossary' node, are highlighted and linked to their glossary
;;      entries, if option `Info-fontify-glossary-words' is non-nil.
;;
;;      Glossary terms of more than one word are not highlighted or
;;      linked.  Currently only the Emacs and Semantic manuals have
;;      `Glossary' nodes, as far as I know.
;;
;;      By default, a glossary link is fontified (with face
;;      `info-glossary-word'), and by default the link (the face) is
;;      shown only until you follow a link for that word to the
;;      glossary.  This reduces clutter for words you've already
;;      looked up.  Their first occurrences in nodes are still linked,
;;      but the links are only evident on mouseover (using property
;;      `mouse-face').
;;
;;      By default also, mouseover shows the word's definition in a
;;      tooltip (or in the echo area if `tooltip-mode' is off).  You
;;      can optionally not show the definition this way.
;;
;;      To reduce clutter even more, you can optionally hide glossary
;;      links (no face), showing them only on mouseover.
;;
;;      All glossary-link behaviors are specified with option
;;      `Info-fontify-glossary-words'.
;;
;;      You can cycle or toggle the option value using command
;;      `Info-cycle-fontify-glossary-words' or
;;      `Info-toggle-fontify-glossary-words'.
;;
;;    - Text between two delimiters that you specify, if the car of
;;      option `Info-fontify-custom-delimited' is non-nil.
;;
;;    - Any extra highlighting you want in a node, as defined by the
;;      value of option `Info-fontify-extra-function' - a function
;;      that accepts no args.
;;
;;    - Be aware that any such highlighting is not 100% foolproof.
;;      Especially for a manual such as Emacs or Elisp, where
;;      arbitrary keys and characters can be present anywhere, the
;;      highlighting can be thrown off.
;;
;;    - You can cycle or toggle the `Info-fontify-*' options from the
;;      `Info' menu, or using command `Info-cycle-fontify-quotations'
;;      or an `Info-toggle-fontify-*' command.  For example, command
;;      `Info-toggle-fontify-emphasis' toggles option
;;      `Info-fontify-emphasis-flag'.
;;
;;    - Minor mode `Info-variable-pitch-text-mode' uses a
;;      variable-pitch font for Info text.  If you enable this then
;;      you might also want to customize option
;;      `Info-fontify-indented-text-chars', so indented text such as
;;      code uses a fixed-pitch font (face `info-indented-text').
;;
;;    - Non-nil option `Info-fontify-indented-text-chars' means
;;      fontify text that is indented at least that many characters
;;      (default 10).  In the Elisp manual this often means blocks of
;;      code and ASCII-art diagrams.  But in general there's no
;;      telling what is indented at any given level, so caveat emptor.
;;      Think of this as an experimental feature.
;;
;;    - Option `Info-fontify-indented-text-manuals' is a list of
;;      manuals that should use `Info-fontify-indented-text-chars'.
;;      By default this is just the Elisp manual: (elisp).
;;
;;    - You can define specific highlighting for individual manuals.
;;      To do this, you `put' the regexp you want for a given regexp
;;      variable on the manual symbol.  For example, if MY-REGEXP is a
;;      regexp string then this defines the regexp to use for a
;;      quotation as being MY-REGEXP, but only for the Elisp manual:
;;
;;        (put 'elisp 'info-quotation-regexp MY-REGEXP)
;;
;;      Then you can toggle that highlighting separately, using
;;      command `Info-toggle-fontify-local-quotation'.  There's such a
;;      command for each regexp variable.  When you toggle a
;;      particular kind of manual-local highlighting OFF in the
;;      current manual, the global highlighting of that kind takes
;;      over there.
;;
;;      Instead of explicitly setting the variable value for a manual
;;      using `put', you can just use the local toggle command (such
;;      as `Info-toggle-fontify-local-quotation') with a prefix arg.
;;      That prompts you for the regexp to use locally, for the
;;      current manual.
;;
;;      You can also use such local highlighting to just turn OFF the
;;      global highlighting for a given regexp variable.  To do that,
;;      use a prefix arg with the toggle command, and when prompted
;;      for the regexp, type `$-'.  That's a regexp that cannot match
;;      anything.  When using Lisp, use the value of constant
;;      `info-nomatch' - that prevents even trying to match. For
;;      example:
;;
;;        (put 'some-manual 'info-isolated-quote-regexp info-nomatch)
;;
;;      This is already done by default for the isolated-quote regexp
;;      variables, for several manuals that don't involve (much) Elisp
;;      code with such chars: `ada', `bovine', `calc', `emacs-gnutls',
;;      `epa', `eshell', `eww', `info', `nxml', `pcl-cvs', `smtpmail',
;;      `srecode', `todo-mode', `wisent'.  The manuals you have may
;;      well be different from those Emacs provides by default, and
;;      you might want to add or remove such highlighting.
;;
;;  * Optionally showing breadcrumbs in the mode line or the header
;;    line, or both. See where you are in the Info hierarchy, and
;;    access higher nodes directly.
;;
;;    - In the mode line.  Turned on by default.
;;
;;      See ‘Toggle Breadcrumbs’ in the `mouse-3' mode-line menu and
;;      `Toggle Breadcrumbs in Mode Line' in the `Info' menu (in the
;;      menu-bar or in the minor-mode indicator). You can customize
;;      option `Info-breadcrumbs-in-mode-line-mode' if you want to
;;      turn this off by default. (Available for Emacs 23+ only.)
;;
;;    - In the header (just below the header line).
;;
;;      (I also added this to vanilla Emacs 23.)  This is OFF by
;;      default in `Info+'.  See `Toggle Breadcrumbs in Header Line'
;;      in `Info' menu.  Be aware that unlike breadcrumbs in the mode
;;      line, this can occasionally throw off the destination accuracy
;;      of cross references and searches slightly.
;;
;;  * Optional automatic renaming of Info buffers to include the
;;    manual (file) and node names, using minor mode
;;    `info-manual+node-buffer-name-mode'.  You can use option
;;    `info-buffer-name-function' to customize the format of the
;;    buffer names.
;;
;;  * Additional commands, including:
;;
;;    - `Info-virtual-book' (bound to `v') – Open a virtual Info
;;      manual of saved nodes from any number of manuals.  The nodes
;;      are those saved in option `Info-virtual-book'.  With `C-u',
;;      bookmarked Info nodes are also included.  (If you use Icicles,
;;      see also `icicle-Info-virtual-book'.)
;;
;;    - `Info-persist-history-mode' (Emacs 24.4+) - Enabling this
;;      minor mode saves the list of your visited Info nodes between
;;      Emacs sessions.  Together with command `Info-history' (bound
;;      to `L' by default), this gives you a persistent virtual manual
;;      of the nodes you have visited in the past.  If the mode is
;;      enabled then the list of visited nodes is saved to the file
;;      named by option `Info-saved-history-file' when you quit Emacs
;;      (not Info) or when you kill an Info buffer.
;;
;;      (If you also use library Bookmark+ then you can bookmark Info
;;      nodes, including automatically.  This records how many times
;;      you have visited each node and when you last did so.)
;;
;;    - `Info-change-visited-status' (Emacs 24.4+) (bound to `C-x
;;      DEL') - Toggle or set the visited status of the node at point
;;      or the nodes in the active region.  Useful if you use
;;      `Info-fontify-visited-nodes' to show you which nodes you have
;;      visited.  No prefix arg: toggle.  Non-negative prefix arg: set
;;      to visited.  Negative prefix arg: set to unvisited.
;;
;;    - `Info-save-current-node' (bound to `.') – Save the name of the
;;      current node to list `Info-saved-nodes', for use by `v'
;;      (`Info-virtual-book').
;;
;;    - `Info-merge-subnodes' – Integrate the current Info node with
;;      its subnodes (the nodes in its Menu), perhaps recursively.
;;
;;      Use `Info-merge-subnodes' to extract a self-contained report
;;      (possibly the whole manual) from an Info manual.  The report
;;      is itself an Info buffer, with hyperlinks and normal Info
;;      behavior.
;;
;;      There are various prefix-argument possibilities that govern
;;      just how subnodes are treated (recursively or not, for
;;      instance).  There are a few user options that let you
;;      customize the report appearance.
;;
;;
;;  The following bindings are made here for Info-mode:
;;
;;    `?'              `describe-mode' (replaces `Info-summary')
;;    `+'              `Info-merge-subnodes'
;;    `.'              `Info-save-current-node'
;;    `a'              `info-apropos'
;;    `G'              `Info-goto-node-web'
;;    `O'              `Info-toc-outline'
;;    `v'              `Info-virtual-book'
;;    `mouse-4'        `Info-history-back'
;;    `mouse-5'        `Info-history-forward'
;;    `S-down-mouse-2' `Info-mouse-follow-nearest-node-new-window'
;;    `S-RET'          `Info-follow-nearest-node-new-window'
;;
;;  The following bindings are made here for merged Info buffers:
;;
;;    `.'              `beginning-of-buffer'
;;    `b'              `beginning-of-buffer'
;;    `q'              `quit-window'
;;    `s'              `nonincremental-re-search-forward'
;;    `M-s'            `nonincremental-re-search-forward'
;;    `TAB'            `Info-next-reference'
;;    `ESC TAB'        `Info-prev-reference'
;;
;;  The global binding `C-h r' is changed from `info-emacs-manual' to
;;  `info-manual', which behaves the same except if you use a prefix
;;  arg.  With a prefix arg you can open any manual, choosing either
;;  from all installed manuals or from those that are already shown in
;;  Info buffers.
;;
;;  The following behavior defined in `info.el' has been changed:
;;   "*info" has been removed from `same-window-buffer-names', so that
;;   a separate window can be used if you so choose.
;;
;;  Suggestion: Use a medium-dark background for Info.  Try, for
;;  example, setting the background to "LightSteelBlue" in your
;;  `~/.emacs' file.  You can do this as follows:
;;
;;         (setq special-display-buffer-names
;;               (cons '("*info*" (background-color . "LightSteelBlue"))
;;                     special-display-buffer-names))
;;
;;  Alternatively, you can change the background value of
;;  `special-display-frame-alist' and set `special-display-regexps' to
;;  something matching "*info*":
;;
;;         (setq special-display-frame-alist
;;               (cons '(background-color . "LightSteelBlue")
;;                     special-display-frame-alist))
;;         (setq special-display-regexps '("[ ]?[*][^*]+[*]"))
;;
;;  If you do use a medium-dark background for Info, consider
;;  customizing face to a lighter foreground color - I use "Yellow".
;;
;;  Also, consider customizing face `link' to remove its underline
;;  attribute.
;;
;;  This file should be loaded after loading the standard GNU file
;;  `info.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "info" '(require 'info+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2023/05/28 dadams
;;     Added redefinition of Info-menu-update.
;; 2022/11/11 dadams
;;     Added back removed editing feature from vanilla Emacs 26.3 (but leave it disabled):
;;       Info-edit-mode, Info-edit, Info-cease-edit, Info-edit-mode-hook, Info-edit-mode-map.
;; 2022/10/30 dadams
;;     Info-set-mode-line: Update for Emacs 28.1+,
;; 2022/09/18 dadams
;;     Added vacuous defvar for Info-minibuf-history.
;; 2022/08/28 dadams
;;     Added face info-homoglyph and defvar info-symbols-and-replacements (Emacs <27).
;;     Info-mode
;;       Use define-derived-mode.  Add code using info-symbols-and-replacements.
;;       Apply Alan Mackenzie's fix from https://lists.gnu.org/archive/html/emacs-devel/2022-08/msg01301.html.
;; 2022/05/02 dadams
;;     info-display-manual: Apply bug #54961's (partial) fix.
;; 2021/12/24 dadams
;;     Added: Info-fontify-indented-text-manuals, function and variable info-indented-text-regexp.
;; 2021/11/11 dadams
;;     Bind Info-url-for-node to W.
;; 2021/10/16 dadams
;;     Added: Info-toggle-fontify-extra, info-last-non-nil-fontify-extra-function.
;;     Info-fontify-extra-function is now a user option.
;;     Info-mode-menu submenu Toggle/Cycle: Added Info-toggle-fontify-extra.
;; 2021/10/15 dadams
;;     Added: Info-refontify-current-node, Info-defontify-current-node, Info-defontify.
;;     Use Info-refontify-current-node everywhere, instead of font-lock-defontify plus Info-fontify-node.
;;     Info-fontify-glossary-words (option): Added hide possibilities and renamed option values.  Updated doc.
;;     Info-fontify-glossary-words (function): Handle hide possibilities.  Use new/renamed option values.
;;     Info-cycle-fontify-glossary-words, Info-toggle-fontify-glossary-words,
;;       info--msg-Info-fontify-glossary-words-now, info-last-non-nil-fontify-glossary-words:
;;         Use new/renamed option values.
;; 2021/09/19 dadams
;;     Added: info--msg-Info-fontify-glossary-words-now.
;;     Info-fontify-glossary-words: Default now: show-link-and-def-till-visited.  show-link-and-def -> t.
;;     info-last-non-nil-fontify-glossary-words: Default now:  show-link-and-def-till-visited.
;;     Info-(cycle|toggle)-fontify-glossary-words: Use info--msg-Info-fontify-glossary-words-now.
;;     Info-cycle-fontify-glossary-words: show-link-and-def -> t.
;;     Info-fontify-glossary-words: Don't include mouse-2 info if using echo area.
;; 2021/09/10 dadams
;;     Added Info-cycle-(fontify|link)-glossary-words, Info-link-glossary-words, Info-glossary-link-history,
;;           info-last-non-nil-fontify-glossary-words.
;;     Info-fontify-glossary-words: Added more values: can show link only until followed.
;;                                  Renamed link-only value.  If you had customized to that, customize again.
;;     Info-history-clear: Clear also Info-glossary-link-history.
;;     Info-toggle-fontify-glossary-words: Toggle to last non-nil value.
;;     Info-fontify-glossary-words:
;;       Respect show-link(-and-def)-till-visited - add link face only if not yet visited.
;;     Info-goto-glossary-definition: Add word to Info-glossary-link-history.
;;     Everywhere: changed eq to derived-mode-p for Info-mode.
;;     Fixed multiple commands, to echo msg regardless of whether in Info-mode.
;; 2021/09/09 dadams
;;     Info-glossary-fallbacks-alist: Prepend semantic manual to default value.  Say in doc to put t entries last.
;;     info-fallback-manual-for-glossary: Try explicit-list fallback entries before t entries.
;;     Info-goto-glossary-definition: Get short manual name before calling info-fallback-manual-for-glossary.
;; 2021/08/24 dadams
;;     info-good-fixed-pitch-font-families: Added autoload cookie.
;;       Thx to wiku user emacs18 and https://github.com/syl20bnr/spacemacs/issues/15010.
;; 2021/06/14 dadams
;;     Added info-nomatch; use & doc it.  Turn off highlighting isolated quote chars by default for some manuals.
;; 2021/06/03 dadams
;;     Info-toggle-fontify-local, Info-toggle-fontify-local-*, Info-fontify-node, Info-fontify-quotations,
;;       Info-fontify-custom-delimited:
;;         You can use manual-local fontifying to turn off the corresponding global highlighting.
;; 2021/06/02 dadams
;;     Info-toggle-fontify-local: Do not allow empty-string regexp as input.
;;     Info-manual-(string|symbol): Added optional INFO-FILE arg.
;;     Info-bookmark-(for-node|named-at-point): Don't pass Info-current-file as arg.
;; 2021/06/01 dadams
;;     Added: Info-manual-string, Info-manual-symbol, info-fontifying-regexp, Info-toggle-fontify-local,
;;            Info-toggle-fontify-local-angle-bracketed, Info-toggle-fontify-local-angle-bracketed-same-line,
;;            Info-toggle-fontify-local-custom-delimited, Info-toggle-fontify-local-emphasis,
;;            Info-toggle-fontify-local-isolated-backquote, Info-toggle-fontify-local-isolated-quote,
;;            Info-toggle-fontify-local-quotation, Info-toggle-fontify-local-quotation-same-line, 
;;     Added functions (not vars) info-custom-delimited-same-line-regexp, info-isolated-backquote-regexp,
;;           info-isolated-quote-regexp, info-quotation-regexp, info-quotation-same-line-regexp,
;;           info-quoted+<>-regexp, info-quoted+<>-same-line-regexp, Info-emphasis-regexp.
;;     Info-fontify-custom-delimited function: Use function, not var.
;;     Info-fontify-custom-delimited option: Corrected :type spec.
;;     Info-emphasis-regexp: Updated doc to say it must have a regexp group.
;;     Info-fontify-quotations: Use functions, not vars: (info-*-regexp), not info-*-regexp.
;;     Info-fontify-node: Use function Info-emphasis-regexp, not var.  Wrap stuff in EMPHASIS clause with
;;       ignore-errors because using function and in case regexp is faulty.  This is a special case, because we
;;       use text property`invisible, not just font-lock.
;;     Info-bookmark-(for-node|named-at-point): use Info-manual-string.
;; 2021/05/24 dadams
;;     Added: Info--manuals - factored out from info-display-manual.
;;     Info-apropos-manuals: Updated doc string.
;;     info-apropos: Non-positive prefix arg prompts for the manuals to search.  Added args ARG and NARG.
;;     info-display-manual: Use Info--manuals.
;; 2021/05/23 dadams
;;     Added: Info-apropos-manuals.
;;     info-apropos, Info-apropos-matches: Respect Info-apropos-manuals.
;; 2021/03/07 dadams
;;     Added: info-good-fixed-pitch-font-families, info-remap-default-face-cookie.
;;     Face info-fixed-pitch: Merge attributes from first family found in info-good-fixed-pitch-font-families.
;;     Info-remap-default-face-to-variable-pitch: Set info-remap-default-face-cookie.
;;     Info-variable-pitch-text-mode: No longer on Info-mode-hook - takes effect immediately.
;;     Info-fontify-indented-text-chars: Default value is now nil (no-op), not 10.
;;     Info-fontify-indented-text: Allow arbitrary indentation level, not just 10.
;; 2021/05/06 dadams
;;     Added Info-remap-default-face-to-variable-pitch, Info-variable-pitch-text-mode, face info-fixed-pitch,
;;           Info-fontify-indented-text-chars, Info-fontify-indented-text.
;;     Faces *-custom-delimited, *(-double)-quoted-name, *-string, *-isolated-quote, *-reference-item:
;;       Inherit from info-fixed-pitch, not from font-lock-string-face.
;;     Info-fontify-node: Use Info-fontify-indented-text if Info-fontify-indented-text-chars.
;; 2021/01/13 dadams
;;     Info-search: Deactivate mark only if search moves to a different node or isearchp-deactivate-region-flag
;;                  is undefined or non-nil.
;; 2020/11/14 dadams
;;     Info-fontify-quotations, Info-fontify-custom-delimited, info-display-manual:
;;       Use ignore-errors instead of nil condition-case.
;; 2020/11/12 dadams
;;     Added: Info-fontify-extra-function.
;;     Info-fontify-node: Invoke Info-fontify-extra-function if non-nil.
;; 2020/11/08 dadams
;;     Be able to use glossary of another manual (Emacs manual, by default) for manuals that don't have one:
;;       Added: Info-glossary-fallbacks-alist (option), info-fallback-manual-for-glossary.
;;       Info-find-node-2, Info-fontify-glossary-words: Use fallback glossary, if specified.
;;       Info-find-node-2: Don't try to get/create hash table if it already exists.
;;       Info-get-glossary-hash-table-create:
;;         If manual already has a hash table, return it.  Return to (original node in) original manual.
;;       Info-goto-glossary-definition: Add manual to destination, so go to fallback glossary, if appropriate.
;; 2020/11/04 dadams
;;     Added: face info-custom-delimited, option and function Info-fontify-custom-delimited,
;;            var info-custom-delimited-same-line-regexp, info-custom-delim-1, info-custom-delim-2,
;;            Info-define-custom-delimiting, Info-toggle-fontify-custom-delimited.
;;     Info-mode-menu: Added Info-define-custom-delimiting, Info-toggle-fontify-custom-delimited.
;;     Info-fontify-node: Added custom-delimited fontifying( Info-fontify-custom-delimited).
;; 2020/11/02 dadams
;;     Faces info-file, info-*-item, info-xref-bookmarked: Tweaked defaults.
;; 2020/11/01 dadams
;;     Added: info--user-search-failed, (redefinition of) Info--search-loop.
;;     Info-search: Bind isearchp-restrict-to-region-flag to (not Info-isearch-search), for isearch+.el.
;;                  Update for recent Emacs as of 26.3.
;;     Info-isearch-search: Updated for Emacs 25+.
;; 2020/10/30 dadams
;;     Added: Info-toggle-fontify-all, Info-toggle-fontify-reference-items, Info-toggle-fontify-visited-nodes.
;;     Info-mode-menu: Added those toggle commands to submenu Toggle/Cycle.
;;     info-(quotation|quoted+<>)(-same-line)-regexp: Use +, not *, for all but "..." string.
;;     Info-fontify-quotations: Adjust doc to reflect new regexps for same line (only "..." is not same line).
;;                              Use \\= to ensure ` and ' appear without conversion.
;;     info-isolated-(back)quote-regexp: Simplify to just any char after ` and before '.  (Performance.)
;;     Info-emphasis-regexp: Use single word as default, to avoid converting names in code.
;;     Info-fontify-glossary-words: Use text property font-lock-face, not face.
;;     Face info-isolated-backquote: Use PaleGreen1, not Chartreuse1, for background.
;; 2020/10/28 dadams
;;     Info-mode-menu: Typo: had Info-toggle-fontify-glossary-words instead of Info-fontify-glossary-words as var.
;;     Info-get-glossary-hash-table-create: Bind Info-fit-frame-flag to nil, to avoid unnecessary frame-fit.
;;     info-glossary-link-map: Added follow-link.
;;     Info-fontify-glossary-words: Swapped order of help-echo text: put mouse-2 text before definition, so it
;;                                  gets changed to mouse-1 when mouse-1-click-follows-link is non-nil.
;; 2020/10/27 dadams
;;     Added: Info-glossary.
;; 2020/10/26 dadams
;;     Added: Info-toggle-fontify-glossary-words, Info--member-string-nocase, Info-link-faces.
;;     Info-fontify-node, Info-fontify-glossary-words: Made glossary stuff work also with Emacs < 24.
;;     Info-fontify-node: Move Info-fontify-glossary-words after link highlighting.
;;     Info-fontify-glossary-words: Don't fontify glossary words in an Index.  Don't fontify a word in Glossary
;;                                  unless its occurrence is in a definition other than its own.
;; 2020/10/25 dadams
;;     Added:
;;       info-glossary-word face, Info-fontify-glossary-words option and function, Info-case-insensitive-string=,
;;       Info-case-insensitive-string-hash, Info-get-glossary-hash-table-create, Info-goto-glossary-definition,
;;       info-glossary-link-map, Info-no-glossary-manuals.
;;     Renamed: info-fontify-(quotations|reference-items) to Info-fontify-(quotations|reference-items).
;;     Info-find-node-2: create glossary-words hash table.
;;     Info-fontify-node: If Info-fontify-glossary-words then fontify glossary words.
;; 2020/10/23 dadams
;;     Added: info-isolated-backquote, info-isolated-backquote-regexp, info-isolated-quote-regexp.
;;     Renamed: Info-toggle-fontify-single-quote to Info-toggle-fontify-isolated-quote,
;;              Info-fontify-single-quote-flag to Info-fontify-isolated-quote-flag
;;              info-single-quote to info-isolated-quote.
;;     info-isolated-quote face: changed default value.
;;     info-fontify-quotations: Handle Info-fontify-isolated-quote-flag - separate loops.
;; 2020/10/19 dadams
;;     Quotation highlighting is now same-line only, by default.
;;       Added: info-quotation-same-line-regexp, info-quoted+<>-same-line-regexp.
;;       Renamed: Info-fontify-quotations-flag to Info-fontify-quotations,
;;                Info-toggle-fontify-quotations to Info-cycle-fontify-quotations,
;;                Toggle submenu to Toggle/Cycle.
;;       Info-fontify-quotations: 3 values now, not just Boolean.
;;       info-fontify-quotations: Handle info-quotation-same-line-regexp, info-quoted+<>-same-line-regexp.
;; 2020/10/17 dadams
;;     Do not bind mouse-4 and mouse-5 to history movement on systems where they scroll window instead.
;; 2020/10/13 dadams
;;     Info-fontify-node (Emacs 24.2+): Use header-line-highlight for mouse-face in header.
;; 2020/10/09 dadams
;;     Info-read-bookmarked-node-name: remove-if-not -> bmkp-remove-if-not.
;;     Everywhere: added nil second arg to looking-back (arg is required now).
;;     Added more defvars to quiet byte-compiler, for Emacs 27+.
;; 2020/09/26 dadams
;;     Info-goto-emacs-key-command-node: If this-file is nil then return nil.
;; 2020/08/30 dadams
;;     Added: Info-bookmark-use-only-node-not-file-flag, redefinition of Info-bookmark-jump (fixes Emacs
;;            bug #42993), Info-search-case-sensitively-next.
;;     Restructure Info menu (fixes Emacs bug #43115).
;; 2020/05/20 dadams
;;     Info-insert-dir: Removed use of Info-following-node-name.  Emacs Dev replaced it by its code.
;; 2020/04/21 dadams
;;     Added: info-buffer-name-function, info-buffer-name-function-default, info-rename-buffer,
;;            info-manual+node-buffer-name-mode.
;; 2019/05/31 dadams
;;     Info-emphasis-regexp: Add another possible value to doc string.
;; 2018/09/21 dadams
;;     Added: Info--pop-to-buffer-same-window.
;;     Info-find-node, Info-(next|prev|up), info-display-manual:
;;       Use Info--pop-to-buffer-same-window, not switch-to-buffer.
;; 2018/09/09 dadams
;;     Added: redefinitions of Info-backward-node, Info-directory, Info-forward-node, Info-history-back,
;;            Info-history-forward, Info-next, Info-prev, Info-up.
;; 2018/06/14 dadams
;;     Added: redefinitions of info-apropos, Info-apropos-matches.
;;     bmkp-string-match-p - > string-match-p everywhere.
;; 2018/06/03 dadams
;;     info-quotation-regexp, info-quoted+<>-regexp:
;;       Use shy groups everywhere.  [CHAR] -> CHAR, \\CHAR -> CHAR.  Added equivalent rx sexps.
;;     info-fontify-reference-items, Info-node-name-at-point: [\n] -> \n.
;; 2018/04/12 dadams
;;     Info-read-node-name: Use Info-minibuffer-history, not Info-history, for completing-read.
;;     Info-find-node(-2): Added arg STRICT-CASE, moving arg MSG to the end.
;;     Info-find-node-2: Updated for Emacs 25+ - use filepos-to-bufferpos etc.
;; 2017/11/17 dadams
;;     Info-TOC-outline stuff needs Info-virtual-nodes.  Thx to Mike Fitzgerald.
;;     http -> https everywhere.
;; 2017/11/09 dadams
;;     info-quotation-regexp, info-quoted+<>-regexp:
;;       Added \\ to first alternative of each ... type, to exclude \ from it.
;; 2017/09/23 dadams
;;     Info-url-for-node: Fix per TeXInfo manual - encode embedded hyphens etc.
;; 2017/08/30 dadams
;;     Renamed: Info-refontify-toc-outline-region to Info-toc-outline-refontify-region.
;; 2017/08/28 dadams
;;     Added: Info-refontify-toc-outline-region.
;;     Info-refontify-toc-outline-region:
;;       Add Info-refontify-toc-outline-region to post-command-hook and bind to C-x M-l.
;;     Info-toc-outline: Turn off Info-breadcrumbs-in-mode-line-mode in TOC buffer.
;;     Info-change-visited-status: Typo: go-to-char.
;; 2017/08/25 dadams
;;     Added: Info-change-visited-status.  Bound to `C-x DEL (instead of Info-make-node-unvisited).
;;     Info-node-name-at-point: Replace newline chars by spaces.
;;     Info-toc-outline: Pass NEWNAME arg to clone-buffer, instead of explicitly renaming buffer.
;; 2017/08/22 dadams
;;     Added: Info-toc-outline, Info-outline-demote, Info-outline-promote, Info-toc-outline-no-redundancy-flag,
;;            Info-toc-outline-find-node, Info-toc-outline-map, Info-toc-outline-refontify-links, redefinition of
;;            outline-invisible-p.
;;     Bind Info-toc-outline to O.
;;     Info-mode-menu: Added Editable Outline TOC item for Info-toc-outline.
;;     Info-node-access-invokes-bookmark-flag, Info-toggle-node-access-invokes-bookmark, Info-goto-node advice:
;;       Reserve for Emacs 24.4+.
;; 2017/08/10 dadams
;;     Info-goto-node: Define it for Emacs 23 also.
;;     Info-mode-menu:
;;       Add menu items for Info-toggle-node-access-invokes-bookmark, Info-toggle-fontify-bookmarked-xrefs.
;; 2017/08/07 dadams
;;     Added: Info-make-node-unvisited.  Bound to C-x DEL.
;; 2017/08/06 dadams
;;     Added: Info-bookmarked-node-xref-faces, Info-read-bookmarked-node-name,
;;            Info-set-face-for-bookmarked-xref.
;;     Bind Info-set-face-for-bookmarked-xref to C-x f.
;;     Info-describe-bookmark: If no bookmarked node name at point, use Info-read-bookmarked-node-name.
;;     Info-bookmark-for-node: Made NODE arg optional - if nil then read the node name.  Added LOCALP arg.
;;     Info-fontify-node (Emacs 24.2+): Get face for bookmarked xref from bmkp-info-face tag value, if any.
;;                                      Call Info-bookmark-for-node with arg LOCALP.
;; 2017/08/04 dadams
;;     Info-describe-bookmark: Use Info-bookmark-name-at-point, not Info-node-name-at-point.
;;     Info-goto-node: Do it only for Emacs 24.2+.
;; 2017/08/02 dadams
;;     Info-goto-node: Define only if can soft-require bookmark+.el.
;;                     No-op if NODE is in Info-index-nodes.
;;                     Bind Info-node-access-invokes-bookmark-flag to nil while invoking bookmark.
;;                     Use bookmark--jump-via with ignore as display function, instead of bookmark-jump.
;; 2017/07/30 dadams
;;     Added advice of Info-goto-node, to respect Info-node-access-invokes-bookmark-flag.
;;     Removed redefinitions of Info-follow-nearest-node, Info-try-follow-nearest-node.
;;     Replaced Info-follow-xref-bookmarks-flag by Info-node-access-invokes-bookmark-flag.
;;     Replaced Info-toggle-follow-bookmarked-xrefs by Info-toggle-node-access-invokes-bookmark.
;;     Info-bookmark-for-node, Info-bookmark-named-at-point: Include manual name in bookmark name.
;; 2017/07/29 dadams
;;     Added: Info-fontify-bookmarked-xrefs-flag, face info-xref-bookmarked, Info-describe-bookmark,
;;            Info-bookmark-for-node, Info-bookmark-name-at-point, Info-bookmark-named-at-point,
;;            Info-bookmark-name-for-node, Info-toggle-fontify-bookmarked-xrefs,
;;            Info-follow-xref-bookmarks-flag, Info-toggle-follow-bookmarked-xrefs.
;;     Added (redefinition of): Info-follow-nearest-node, Info-try-follow-nearest-node.
;;     Info-fontify-node (24.2+): Respect Info-fontify-bookmarked-xrefs-flag.
;;     Bind Info-describe-bookmark to C-h C-b.
;; 2017/02/20 dadams
;;     Added: Info-saved-history-file, Info-persist-history-mode, Info-save-history-list,
;;            Info-restore-history-list.
;;     Added autoload cookies: Info-breadcrumbs-in-mode-line-mode, Info-set-breadcrumbs-depth,
;;           Info-search, Info-mouse-follow-nearest-node, info-display-manual.
;; 2017/01/09 dadams
;;     Info-find-emacs-command-nodes: Updated to handle LINE-NUMBER (Emacs 24.5+).
;; 2016/12/13 dadams
;;     Removed obsolete face aliases: info-menu-5, Info-title-*-face.
;; 2016/12/11 dadams
;;     Added defvars for isearch(-regexp)-lax-whitespace for Emacs 24.1 and 24.2.
;; 2016/12/10 dadams
;;     Use string as 3rd arg to make-obsolete.
;; 2016/10/31 dadams
;;     info-quotation-regexp: Typo: misplaced curly double-quote.  Thx to Don March.
;; 2016/07/02 dadams
;;     Added: Info-toggle-fontify-emphasis, Info-breadcrumbs-in-header-flag, Info-emphasis-regexp,
;;            Info-fontify-emphasis-flag, info-fontify-emphasis, and face info-emphasis.
;;     Added some doc from Emacs Wiki to commentary.
;;     Info-mode-menu:
;;       Add toggle indicators.  Moved toggle commands to Toggle submenu.  Added Info-toggle-fontify-emphasis.
;;     Info-fontify-node: Fontify emphasis.
;; 2015/09/14 dadams
;;     info-double-quoted-name: Changed default colors.
;; 2015/09/13 dadams
;;     Added face info-double-quoted-name.
;;     info-quotation-regexp, info-quoted+<>-regexp: Added pattern for curly double-quotes (“...”).
;;                                                   Use shy groups for all parts.
;;     info-fontify-quotations: Fontify text between curly double-quotes (“...”).
;; 2015/03/19 dadams
;;     info-quoted+<>-regexp: Highlight <...> only if the first char is alphabetic.
;; 2015/03/06 dadams
;;     Added: info-manual.  Bound it globally to C-h r.
;;     Info-fontify-node (Emacs 24.1.N+): Updated per Emacs 24.4: allow Info-fontify-maximum-menu-size to be t.
;;     info-display-manual: Updated for Emacs 25: use info--manual-names with prefix arg.
;; 2015/02/28 dadams
;;     Added: redefinition of Info-read-node-name.
;;     Info-goto-node-web, Info-url-for-node: Use Info-current-node as default.
;; 2014/12/21 dadams
;;     Added: Info-goto-node-web, Info-url-for-node.
;;     Reorganized.  Code cleanup.  Improved commentary.  Added index links.
;;     Info-toggle-breadcrumbs-in-header-line: Added 3rd arg to make-obsolete.
;;     Info-breadcrumbs-in-mode-line-mode: (default-value 'mode-line-format), not default-mode-line-format,
;;     Info-display-node-default-header: (goto-char (point-min)), not (beginning-of-buffer).
;;     Info-merge-subnodes: with-current-buffer, not save-excursion + set-buffer.
;; 2014/05/04 dadams
;;     REMOVED SUPPORT for Emacs 20-22.  That support is offered by a new library now: info+20.el.
;;     Added coding:utf-8 declaration.  Replace \x2018, \x2019 with literal ‘ and ’, since now Emacs 23+.
;; 2014/05/03 dadams
;;     info-quotation-regexp, info-quoted+<>-regexp: Handle also curly single quotes (Emacs 24.4+).
;;                                                   Removed double * and moved openers outside \(...\) group.
;;     info-fontify-quotations: Handle also curly single quotes (Emacs 24.4+).
;; 2014/03/04 dadams
;;     Renamed Info-toggle-breadcrumbs-in-header-line to Info-toggle-breadcrumbs-in-header.
;;       Declared old name obsolete.
;; 2014/03/02 dadams
;;     Info-find-file: Go to directory if no previous file (per Emacs 24.4+).
;;     Info-find-node-2 (Emacs > 22): Go to Top node at end, if no history.
;; 2013/10/17 dadams
;;     Added: Info-search-beg, Info-search-end, Info-isearch-search-p.
;;     Added redefinition: Info-isearch-wrap, Info-isearch-search.
;;     Info-display-node-default-header, Info-merge-subnodes: Renamed node-name to infop-node-name.
;; 2013/03/17 dadams
;;     Added: Info-history-clear, macro info-user-error (and font-lock it).  Advised: Info-history.
;;     Use info-user-error instead of error, where appropriate.
;; 2013/02/26 dadams
;;     Info-mode-menu and Info-mode doc string: Removed Info-edit, Info-enable-edit (now obsolete).
;; 2013/02/09 dadams
;;     Info-read-node-name-1: Removed Emacs 23+ redefinition.
;; 2013/02/03 dadams
;;     Added: Info-fontify-angle-bracketed-flag, Info-toggle-fontify-angle-bracketed,
;;            Info-toggle-fontify-quotations, Info-toggle-fontify-single-quote, info-quoted+<>-regexp.
;;     info-fontify-quotations: Fixed case for Info-toggle-fontify-single-quote = nil.
;;                              Handle also Info-fontify-angle-bracketed-flag.
;;     Added  Info-fontify-*-flag to Info menu (so menu bar and C-mouse-3).
;; 2012/09/24 dadams
;;     Info-search. Info-mode: Applied latest Emacs 24 updates by Juri (from 2012-09-12).
;; 2012/08/25 dadams
;;     Info-fontify-node: Hide any empty lines at end of node (fixes bug #12272).
;; 2012/08/24 dadams
;;     info-fontify-reference-items: Fontify parameters on continuation lines also.
;;     Info-fontify-node: Juri's fix for Emacs bug #12187.
;;     Reverted Juri's change from 08/20, since Juri fixed it elsewhere afterward.
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/20 dadams
;;     Applied Juri's fix for Emacs bug #12230:
;;       Added: Info-file-attributes.
;;       Info-find-file: Clear caches of modified Info files.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;; 2012/08/12 dadams
;;     Added: info-constant-ref-item (face).
;;     info-fontify-reference-items: Handle constants, using face info-constant-ref-item.
;;     Info-toggle-breadcrumbs-in-header-line, Info-save-current-node: Added MSGP arg.
;; 2012/08/10 dadams
;;     Info-search: Use latest Emacs 24 msg: _end of node_, not _initial node_.
;; 2012/08/09 dadams
;;     Info-fontify-node: Updated guards for Emacs 24 versions.
;; 2012/07/28 dadams
;;     Info-fontify-node: Typo on guard: (/= 1 emacs-minor-version) should have been =, not /=.
;; 2012/07/17 dadams
;;     Added redefinition of Info-fontify-node for post-Emacs 24.1.
;;     Added redefinitions of Info-insert-dir, Info(-directory)-find-node, with args controlling msgs.
;;     info-find-node-2: Added optional arg NOMSG.
;;     Info-find-emacs-command-nodes, Info-goto-emacs(-key)-command-node: Added optional arg MSGP.
;;     Info-search, Info-save-current-node: Show messages only if interactive-p.
;; 2012/01/15 dadams
;;     Added: info-display-manual (redefinition).
;;     Info-find-file: Do not define for < Emacs 23.2 - no virtual books.
;; 2011/11/15 dadams
;;     Added: redefinition of Info-find-file for Emacs 23+, to handle virtual books.
;; 2011/08/23 dadams
;;     Removed hard-code removal of info from same-window-(regexps|buffer-names).  Thx to PasJa.
;; 2011/02/06 dadams
;;     info-user-option-ref-item: Corrected background for light-bg case.
;; 2011/02/03 dadams
;;     All deffaces: Provided default values for dark-background screens too.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non def* sexps.  Added for defgroup and defface.
;; 2010/05/27 dadams
;;     Added: Info-set-mode-line.
;;     Info-find-node-2:
;;       Added redefinition of it for Emacs 23.2 (they keep twiddling it).
;;       Do not call Info-insert-breadcrumbs-in-mode-line.  Do that in Info-set-mode-line now.
;; 2010/04/06 dadams
;;     Added: Info-breadcrumbs-in-header-flag, Info-toggle-breadcrumbs-in-header-line,
;;            Info-breadcrumbs-in-mode-line-mode, Info-set-breadcrumbs-depth,
;;            Info-insert-breadcrumbs-in-mode-line, Info-breadcrumbs-depth-internal.
;;     Added to Info-mode-menu (Emacs 23+): Info-breadcrumbs-in-mode-line-mode.
;;     Info-find-node-2 (Emacs 23+): Add breadcrumbs to header line & mode line only according to vars.
;;     Info-fontify-node (Emacs 23+): Handle breadcrumbs in header only if flag says to.
;; 2010/01/12 dadams
;;     Info-find-node for Emacs 20, Info-find-node-2 for Emacs 21, 22, Info-search:
;;       save-excursion + set-buffer -> with-current-buffer.
;; 2010/01/10 dadams
;;     Info-find-node-2 for Emacs 23+: Updated for Emacs 23.2 (pretest) - virtual function stuff.
;; 2009/12/13 dadams
;;     Typo: Incorrectly used Emacs 22 version for Emacs 21 also.
;; 2009/12/11 dadams
;;     info-fontify-(node|quotations|reference-items), Info-merge-subnodes:
;;       Use font-lock-face property, not face, if > Emacs 21.
;; 2009/08/03 dadams
;;     Updated for Emacs 23.1 release: Info-find-node-2, Info-fontify-node, Info-search: new version.
;; 2009/06/10 dadams
;;     Added: Info-fontify-reference-items-flag, Info-mode-syntax-table.
;;     Info-mode: Use Info-mode-syntax-table, not text-mode-syntax-table.
;;     Info-fontify-node: Fontify ref items if *-reference-items-flag, not just for Elisp manual.
;;     Renamed: info-elisp-* to info-*.
;; 2009/06/09 dadams
;;     info-fontify-quotations: Allow \ before ', just not before`.
;; 2009/06/08 dadams
;;     info-fontify-quotations: Rewrote, using better regexp.  Don't fontify escaped ` or '.
;;       Fontify `\', `\\', etc.  Respect Info-fontify-single-quote-flag.
;;     Added: info-single-quote, Info-fontify-single-quote-flag, info-quotation-regexp.
;;     info-quoted-name: Changed face spec to (:inherit font-lock-string-face :foreground "DarkViolet")
;; 2009/05/25 dadams
;;     Info-virtual-book: Treat info-node bookmarks too.
;; 2009/05/23 dadams
;;     Added: Info-mode for Emacs 23.
;;            They added Info-isearch-filter, Info-revert-buffer-function, Info-bookmark-make-record.
;; 2009/05/22 dadams
;;     Added: Info-saved-nodes, Info-save-current-node, Info-virtual-book.  Added to Info-mode-menu.
;;     Bind info-apropos, Info-save-current-node, Info-virtual-book to a, ., and v.
;;     Info-mode: Updated doc string.
;; 2009/04/26 dadams
;;     Info-merge-subnodes: Bind inhibit-field-text-motion to t, for end-of-line.
;; 2008/10/07 dadams
;;     Require cl.el at compile time for all Emacs versions, because of case.
;; 2008/10/05 dadams
;;     Added: Info-read-node-name-1, Info-read-node-name-2.
;; 2008-07-11 dadams
;;     Info-fontify-node (Emacs 22+): Protect histories when getting ancestor nodes for breadcrumbs.
;;     (Emacs 22+) Don't change faces info-menu-header, *-title-*, *(-header)-node, header-line.
;;     (Emacs 20, 21): Removed bold and italic attributes from info-node and info-xref.
;;     Removed commented out defface for info-xref and info-node.
;;     Face info-file: Blue, not DarkBlue, foreground, by default.
;; 2008/06/12 dadams
;;     Info-fontify-node (Emacs 22+):
;;       Prevent infinite recursion from Info-goto-node calling Info-fontify-node.
;;       Fixed for nil Info-hide-note-references.
;; 2008/06/10 dadams
;;     Info-fontify-node (Emacs 22+): Added breadcrumbs.
;; 2008/03/06 dadams
;;     info-mode: Use fboundp for Info-clone-buffer, not version test, for Emacs 22+. Thx to Sebastien Vauban.
;; 2008/02/01 dadams
;;     Info-mode: Renamed Info-clone-buffer-hook to Info-clone-buffer for Emacs 22.1.90.
;; 2008/01/08 dadams
;;     Info-search (Emacs 22): Removed phony pred arg.
;; 2008/01/06 dadams
;;     Removed soft require of Icicles due to cirular dependency.  Thx to Tennis Smith.
;; 2007/11/27 dadams
;;     Info-search: Use icicle-read-string-completing, if available.
;;     Added soft require Icicles.
;; 2007/11/20 dadams
;;     Info-subtree-separator: Escaped slashes in doc string: \f -> \\f.
;; 2007/09/26 dadams
;;     Better default color for info-quoted-name.  Added group face to all deffaces.
;; 2007/09/25 dadams
;;     Bound Info-mouse-*-new-* to S-down-mouse-2, not S-mouse-2, because of mouse-scan-lines-or-M-:.
;;     Info-goto-emacs-command-node: Convert completion default value to string.
;; 2007/08/27 dadams
;;     Info-fontify-node: Ensure Info-fontify-node is a string when fontifiy quotations. Updated for Emacs 22.
;; 2007/07/13 dadams
;;     Info-find-node: Redefine only for Emacs < 21.
;; 2006/09/15 dadams
;;     Info-mouse-follow-nearest-node redefinition is only for Emacs >= 22.
;;     Changed Emacs 22 tests to just (>= emacs-major-version 22).
;;     Bind tool-bar-map for Emacs 21.  Otherwise, binding of [tool-bar] gives an error (why?).
;; 2006/08/18 dadams
;;     Everywhere: Corrected previous change: minibuffer-selected-window to window-minibuffer-p.
;; 2006/08/14 dadams
;;     Everywhere: fit-frame only if not a minibuffer window.
;; 2006/08/12 dadams
;;     Info-merge-subnodes: Bug fixes:
;;       Added concat for insertion of main node when recursive-display-p is negative.
;;       Don't recurse down Index menus.
;;       When checking for subnodes menu, check for nonfile menu item also.
;;       After come back from recursion, go back to Info buffer before trying to go back in history.
;;       Call fit-frame at end.
;; 2006/06/10 dadams
;;     Added: Info(-mouse)-follow-nearest-node-new-window.  Bound to S-RET, S-mouse-2.
;; 2006/03/31 dadams
;;     info-menu-header: Removed :underline, because links are underlined in Emacs 22.
;;     No longer use display-in-minibuffer.
;; 2006/01/08 dadams
;;     Added: redefinition of Info-mouse-follow-nearest-node.
;; 2006/01/07 dadams
;;     Added :link for sending bug report.
;; 2006/01/06 dadams
;;     Added defgroup Info-Plus and used it. Added :link.
;; 2005/12/30 dadams
;;     Moved everything from setup-info.el to here, after getting rid of some of it.
;;     Use defface for all faces.  Renamed faces, without "-face".
;;     Use minibuffer-prompt face, not info-msg-face.
;;     No longer require setup-info.el.  No longer require cl.el when compile.
;; 2005/11/21 dadams
;;     Info-search for Emacs 22: Don't display repeat `s' message if isearch-mode.
;; 2005/11/09 dadams
;;     Info-fontify-node: Updated to reflect latest CVS (replaced Info-escape-percent header).
;; 2005/10/31 dadams
;;     Use nil as init-value arg in calls to completing-read, everywhere.
;; 2005/07/04 dadams
;;     info-fontify-quotations: Use font-lock-face property, instead of face, for Emacs 22.
;;                              Wrap re-search-forward in condition-case for stack overflow.
;; 2005/07/02 dadams
;;     Info-search: fit-frame. Added Emacs 22 version too.
;;     Info-goto-emacs-command-node, Info-goto-emacs-key-command-node, Info-merge-subnodes:
;;       Use Info-history-back for Emacs 22.
;;     Info-mode: Added Emacs 22 version.
;; 2005/06/23 dadams
;;     Info-fontify-node: Fontify reference items if in Emacs-Lisp manual.
;;     Added: info-fontify-reference-items
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2004/11/20 dadams
;;     Info-find-emacs-command-nodes: bug fix: regexp (cmd-desc) was only for Emacs 21.
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/10/09 dadams
;;     info-fontify-quotations:
;;       1) Allow all characters inside `...'.
;;       2) Treat case of "..." preceded by backslashes
;;     Info-fontify-node (for Emacs 21): Moved info-fontify-quotations before fontification of titles.
;; 2004/10/07 dadams
;;     Renamed Info-resize-frame-p to Info-fit-frame-flag.
;; 2004/10/05 dadams
;;     Improved regexp treatment further for fontifying quotations.
;; 2004/10/04 dadams
;;     Improved regexp treatment for fontifying quotations.
;;       Added info-fontify-quotations. Removed info-fontify-strings-p.
;;       Renamed Info-fontify-quotations-p to Info-fontify-quotations-flag.
;; 2004/10/03/dadams
;;     Major update: updated to work with Emacs 21 also.
;;       Made require of setup-info.el mandatory.
;;       Removed all variables and keys to setup-info.el.
;;       Renamed to Emacs 21 names and only define for Emacs < 21: emacs-info -> info-emacs-manual.
;; 2004/09/28 dadams
;;     Removed dir-info (same as Info-directory).
;;     Renamed to Emacs 21 names and only define for Emacs < 21: emacs-lisp-info -> menu-bar-read-lispref
;; 2004/06/01 dadams
;;     Renamed: Info-fit-frame-p to Info-resize-frame-p, shrink-frame-to-fit to resize-frame.
;; 2000/09/27 dadams
;;     1. Added: Info-fit-frame-p.
;;     2. Info-find-node: added shrink-frame-to-fit.
;; 1999/04/14 dadams
;;     Info-fontify-node: Fontify indexes too.
;; 1999/04/14 dadams
;;     1. Added vars: info-file-face, info-menu-face, info-node-face, info-quoted-name-face, info-string-face,
;;                    info-xref-face.
;;     2. No longer use (or define) faces: info-node, info-file, info-xref, info-menu-5, info-quoted-name,
;;                                         info-string.
;;     3. Info-fontify-node: Use new face variables instead of faces in #2, above.
;;        Corrected: node names in info-node-face (was xref). Use info-menu-face for * and menu item.
;;     4. Info-mode: Redefined like original, but: no make-face's; use face vars.
;;                   Added user options description to doc string.
;; 1999/04/08 dadams
;;     Info-goto-emacs-key-command-node: regexp-quote pp-key for Info-search.
;; 1999/04/07 dadams
;;     Info-goto-emacs-key-command-node: a) msgs only if interactive, b) return nil if not found, else non-nil,
;;       c) "is undefined" -> "doc not found", d) use display-in-minibuffer more, e) corrected error handler.
;; 1999/04/01 dadams
;;     1. Added: (remove-hook 'same-window-buffer-names "*info*").
;;     2. Info-find-node: switch-to-buffer-other-window -> pop-to-buffer.
;; 1999/03/31 dadams
;;     1. Added (put 'Info-goto-emacs-(key-)command-node 'info-file "emacs").
;;     2. Info-find-node: Mention searched file in error messages.
;;     3. Added (replacement): Info-find-emacs-command-nodes, with progress msg.
;;     4. a. Info-goto-emacs-key-command-node: Use global-map, unless menu item.
;;        b. Added message "Not found using Index ...".
;; 1999/03/31 dadams
;;     1. Info-goto-emacs(-key)-command-node: Only display-in-minibuffer if
;;        interactive-p.
;;     2. Info-goto-emacs-key-command-node: Messages: "key"; other entries.
;; 1999/03/31 dadams
;;     1. Added (put 'info 'info-file "emacs") so find doc on `info' cmd.
;;     2. Info-goto-emacs-command-node:
;;        a. Added message when =< 1 match.
;;        b. Return num-matches if found.
;;        c. Uses `display-in-minibuffer' instead of `message'.
;;     3. a. Wrapped call to Info-search in condition-case, not if.
;;        b. Info-goto-emacs-key-command-node: Return num-matches if found.
;; 1999/03/30 dadams
;;     1. Added Info menu bar menu.
;;     2. Info-goto-emacs-command-node: Only error if interactive-p.
;;     3. Info-goto-emacs-key-command-node:
;;        a. Print key in msgs
;;        b. If Info-goto-emacs-command-node doesn't find it, then try Info-search.
;;           If found & interactive-p, then msg ("repeat").  Else error.
;;     4. Info-search: Msg ("repeat") if found & interactive-p.
;; 1999/03/17 dadams
;;     1. Updated to correspond with Emacs 34.1 version.
;;     2. Protect with fboundp.
;; 1996/07/11 dadams
;;     Added redefinitions of Info-goto-emacs-(key-)command-node.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/16 dadams
;;     Added: info-file, info-quoted-name, info-string, Info-fontify-quotations-flag, info-fontify-strings-p.
;;     Take into account in Info-fontify-node.
;; 1996/02/23 dadams
;;     1. Changed binding of Info-merge-subnodes back to `r', but now requires user confirmation when invoked.
;;     2. Info-subtree-separator: Incorporates "\n* ".  variable-interactive prop.
;; 1996/02/22 dadams
;;     display-Info-node-subtree:
;;       1. display-Info-node-subtree -> Info-merge-subnodes (renamed).
;;       2. Changed binding of Info-merge-subnodes from `r' to `C-d'.
;;       3. Don't pick up text between menu-item-line and "\n* ".  Hardwire "\n* ".
;;       4. Untabify menu-item-line, so can count chars to underline.
;;       5. indent-rigidly, not indent-region.
;; 1996/02/22 dadams
;;     1. Bind describe-mode and display-Info-node-subtree.
;;     2. Added redefinition of Info-mode: Only the doc string was changed.
;;     3. Added Info-subtree-separator.
;;     3. display-Info-node-subtree: Info-subtree-separator. Doc. Garbage-collect.
;; 1996/02/22 dadams
;;     Info-merge-subnodes: Rewrote, adding optional args.  Renamed (defaliased) to display-Info-node-subtree.
;; 1996/02/22 dadams
;;     Added redefinition of Info-merge-subnodes (cleanup, corrections).
;; 1996/02/20 dadams
;;     1. Make info-node, info-xref, info-menu-5 here. (Diff faces than before.)
;;     2. Added redefinition of Info-find-node.  (Uses other window.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'info)
(eval-when-compile (require 'cl)) ;; case

;; These are optional, for cosmetic purposes.
(require 'thingatpt nil t) ;; (no error if not found): symbol-at-point

(when (and (require 'thingatpt+ nil t) ;; (no error if not found): symbol-nearest-point
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))

(require 'strings nil t) ;; (no error if not found): concat-w-faces
(require 'fit-frame nil t) ;; (no error if not found): fit-frame

;; Took this out because it leads to a circular `require' dependency.
;; (require 'icicles nil t) ;; (no error if not found): icicle-read-string-completing

;; Quiet the byte compiler a bit.
;;
(defvar apropos-regexp)                 ; In `apropos.el'.
(defvar bookmark-alist)                 ; In `bookmark.el'.
(defvar bookmark-make-record-function)  ; In `bookmark.el'.
(defvar browse-url-new-window-flag)     ; In `browse-url.el'.
(defvar desktop-save-buffer)
(defvar header-line-format)
(defvar Info-breadcrumbs-depth)
(defvar Info-breadcrumbs-depth-internal)
(defvar Info-breadcrumbs-in-header-flag)
(defvar Info-breadcrumbs-in-mode-line-mode)
(defvar Info-current-node-virtual)
(defvar Info-bookmarked-node-xref-faces) ; Here, Emacs 24.2+, with Bookmark+.
(defvar Info-edit-mode-hook)
(defvar Info-edit-mode-map)
(defvar Info-fontify-bookmarked-xrefs-flag) ; Here, Emacs 24.2+, with Bookmark+.
(defvar Info-fontify-visited-nodes)
(defvar Info-hide-note-references)
(defvar Info-history-list)
(defvar Info-history-skip-intermediate-nodes) ; Emacs 24+
(defvar Info-isearch-initial-node)
(defvar Info-isearch-search)
(defvar Info-last-search)
(defvar Info-link-keymap)
(defvar Info-menu-entry-name-re)
(defvar Info-minibuf-history)
(defvar Info-next-link-keymap)
(defvar Info-mode-line-node-keymap)
(defvar Info-node-spec-re)
(defvar Info-persist-history-mode)
(defvar Info-point-loc)
(defvar Info-prev-link-keymap)
(defvar Info-read-node-completion-table)
(defvar Info-refill-paragraphs)
(defvar Info-saved-history-file)
(defvar Info-saved-nodes)
(defvar Info-search-case-fold)
(defvar Info-search-history)
(defvar Info-search-whitespace-regexp)
(defvar info-symbols-and-replacements)   ; Here, Emacs <27.
(defvar info-tool-bar-map)
(defvar Info-up-link-keymap)
(defvar Info-use-header-line)
(defvar infop-node-name)                 ; Here, in `Info-merge-subnodes'.
(defvar isearch-adjusted)                ; In `isearch.el', Emacs 25+.
(defvar isearch-filter-predicate)        ; In `isearch.el', Emacs 23+.
(defvar isearch-lax-whitespace)          ; In `isearch.el'.
(defvar isearch-regexp-lax-whitespace)   ; In `isearch.el'.
(defvar isearchp-deactivate-region-flag) ; In `isearch+.el'.
(defvar isearchp-reg-beg)                ; In `isearch+.el'.
(defvar isearchp-reg-end)                ; In `isearch+.el'.
(defvar mouse-wheel-up-event)            ; In `mwheel.el', Emacs 22+.
(defvar outline-heading-alist)           ; In `outline.el'.
(defvar widen-automatically)

;;;;;;;;;;;;;;;;;;;;

(provide 'info+)
(require 'info+) ;; Ensure loaded before compiling.

;;;;;;;;;;;;;;;;;;;;

 
;;(@* "Macros")
;;; Macros -----------------------------------------------------------

(defmacro info-user-error (&rest args)
  "`user-error' if defined, otherwise `error'."
  `(if (fboundp 'user-error) (user-error ,@args) (error ,@args)))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(info-user-error\\)\\>" 1 font-lock-warning-face)))

;;; KEYS & MENUS ;;;;;;;;;;;;;;;;;;;;;;;;

(define-key Info-mode-map "?"               'describe-mode) ; Don't use `Info-summary'.
(define-key Info-mode-map "+"               'Info-merge-subnodes)
(define-key Info-mode-map "."               'Info-save-current-node)
(define-key Info-mode-map "a"               'info-apropos)
(define-key Info-mode-map "G"               'Info-goto-node-web)
(define-key Info-mode-map "O"               'Info-toc-outline)
(define-key Info-mode-map "v"               'Info-virtual-book)
(define-key Info-mode-map "W"               'Info-url-for-node)
(define-key Info-mode-map (kbd "C-x DEL")   'Info-change-visited-status)
;; Mouse back and forward buttons
(define-key Info-mode-map [S-down-mouse-2]  'Info-mouse-follow-nearest-node-new-window)
(define-key Info-mode-map [S-return]        'Info-follow-nearest-node-new-window)

;; For other than `w32-win' and `ns-win', `mouse-4' and `mouse-4' scroll, so cannot be used for history movement.
(when (and (boundp 'mouse-wheel-up-event) ; Emacs 22+
           (eq mouse-wheel-up-event 'wheel-down))
  (define-key Info-mode-map [mouse-4]       'Info-history-back)
  (define-key Info-mode-map [mouse-5]       'Info-history-forward))
 
;;(@* "Faces (Customizable)")
;;; Faces (Customizable) ---------------------------------------------

;;;###autoload
(defgroup Info-Plus nil
  "Various enhancements to Info."
  :group 'info
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
info+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/info%2b.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/InfoPlus")
  :link '(emacs-commentary-link :tag "Commentary" "info+")
  )

(defconst info-nomatch (intern "$-")
  "Symbol whose name is a regexp that cannot be matched.")

;;;###autoload
(defvar info-good-fixed-pitch-font-families '("Lucida Console" "Lucida Sans Typewriter"
                                              "Consolas" "ProggyVector" "OCR A Extended")
  "Some good fixed-pitch font families.
The default for face `info-fixed-pitch' inherits from the first one
available, checked in list order.")

;;;###autoload
(defface info-fixed-pitch
  `((t (:inherit fixed-pitch
                 ,@(let ((family (catch 'info-fixed-pitch
                                   (dolist (fam  info-good-fixed-pitch-font-families)
                                     (when (member fam (font-family-list))
                                       (throw 'info-fixed-pitch fam))))))
                     (and family `(:family ,family))))))
  "Fixed-pitch face for Info.
The default value inherits from face `fixed-pitch'.  And if you have a
font family listed in variable `info-good-fixed-pitch-font-families',
then the face attributes of the first one you have listed there are
merged with those of face `fixed-pitch'.

By default, face `info-fixed-pitch' is inherited by faces
`info-custom-delimited',`info-double-quoted-name',
`info-isolated-quote', `info-quoted-name', `info-reference-item', and
`info-string'."
  :group 'Info-Plus :group 'faces)

;;;###autoload
(defface info-custom-delimited
  '((((background dark)) (:inherit info-fixed-pitch :foreground "Red"))
    (t (:inherit info-fixed-pitch :foreground "Red")))
  "Face for text surrounded by custom delimiter chars on the same line."
  :group 'Info-Plus :group 'faces)

;;;###autoload
(defface info-double-quoted-name        ; For “...”
  '((((background dark)) (:inherit info-fixed-pitch :foreground "Cyan"))
    (t (:inherit info-fixed-pitch :foreground "DarkOrange")))
  "Face for names enclosed in curly double-quotes (“...”) in `info'."
  :group 'Info-Plus :group 'faces)

;;;###autoload
(defface info-emphasis                  ; For _..._
  '((t (:inherit italic)))
  "Face for emphasizing text enclosed with underscores (_..._) in `info'."
  :group 'Info-Plus :group 'faces)

;;;###autoload
(defface info-indented-text
  '((((background dark)) (:inherit info-fixed-pitch :foreground "DarkBlue"))
    (t (:inherit info-fixed-pitch :foreground "DarkBlue")))
  "Face for indented text such as that used in a code block."
  :group 'Info-Plus :group 'faces)

;;;###autoload
(defface info-file
  '((((background dark)) (:inherit info-reference-item :foreground "Yellow"))
    (t (:inherit info-reference-item :foreground "Blue")))
  "Face for file heading labels in `info'." :group 'Info-Plus :group 'faces)

;;;###autoload
(defface info-glossary-word
  '((t (:box (:line-width 1 :style pressed-button))))
  "Face for first occurrences of glossary words in an `info' node."
  :group 'Info-Plus :group 'faces)

;;;###autoload
(defface info-homoglyph
  (if (facep 'homoglyph)                ; Emacs 24+
      '((t :inherit homoglyp))
    '((((background dark)) :foreground "cyan")
      (((type pc)) :foreground "magenta")
      (t :foreground "brown")))
  "Face for lookalike characters in `info'."
  :group 'Info-Plus :group 'face)

;;;###autoload
(defface info-menu
  '((((background dark)) (:foreground "Yellow"))
    (t (:foreground "Blue")))
  "Face used for menu items in `info'." :group 'Info-Plus :group 'faces)

;; FWIW, I use a `LightSteelBlue' background for `*info*', and I use `yellow' for this face.
;;;###autoload
(defface info-quoted-name               ; For ‘...’ and `...'
  ;; Inheriting now also from `fixed-pitch'.
  ;; But see Emacs bug #44316.  If that gets fixed then maybe revisit this default definition somehow.
  ;;     '((((background dark)) (:inherit fixed-pitch :foreground "#6B6BFFFF2C2C")) ; ~ bright green
  ;;       (((background light)) (:inherit fixed-pitch :foreground "DarkViolet"))
  '((((background dark)) (:inherit info-fixed-pitch :foreground "#6B6BFFFF2C2C")) ; ~ bright green
    (((background light)) (:inherit info-fixed-pitch :foreground "DarkViolet"))
    (t (:foreground "yellow")))
  "Face for quoted names (‘...’ or `...') in `info'."
  :group 'Info-Plus :group 'faces)

;; FWIW, I use a `LightSteelBlue' background for `*info*', and I use `red3' for this face.
;;;###autoload
(defface info-string                    ; For "..."
  ;; See comment for `info-quoted-name'.
  '((((background dark)) (:inherit info-fixed-pitch :foreground "Orange"))
    (t (:inherit info-fixed-pitch :foreground "red3")))
  "Face for strings (\"...\") in `info'."
  :group 'Info-Plus :group 'faces)

;;;###autoload
(defface info-isolated-quote    ; For 'foobar, '(...) etc.
  ;; See comment for `info-quoted-name'.
  '((((background dark))
     (:inherit info-fixed-pitch :foreground "Green"  :background "#46462C2C1111")) ; ~ very dark brown
    (t (:inherit info-fixed-pitch :foreground "Magenta" :background "SlateGray2")))
  "Face for an isolated single-quote mark (') in `info'.
That is, one that is not part of `...'."
  :group 'Info-Plus :group 'faces)
(define-obsolete-face-alias 'info-single-quote' info-isolated-quote "2020-10-22")

;;;###autoload
(defface info-isolated-backquote        ; For `foobar, `(...) etc.
  '((((background dark)) (:inherit info-isolated-quote :background "Magenta"))
    (t (:inherit info-isolated-quote :background "PaleGreen1")))
  "Face for an isolated backquote mark (`) in `info'.
That is, one that is not part of `...'."
  :group 'Info-Plus :group 'faces)

;; Standard faces from vanilla Emacs `info.el', but without `:weight', `:height' and `:inherit'.
;;;###autoload
(defface info-title-1
  '((((type tty pc) (class color) (background dark))  :foreground "yellow" :weight bold)
    (((type tty pc) (class color) (background light)) :foreground "brown"  :weight bold))
  "Face for info titles at level 1."
  :group 'info)

;;;###autoload
(defface info-title-2
  '((((type tty pc) (class color)) :foreground "lightblue" :weight bold))
  "Face for info titles at level 2."
  :group 'info)

;;;###autoload
(defface info-title-3
  '((((type tty pc) (class color)) :weight bold))
  "Face for info titles at level 3."
  :group 'info)

;;;###autoload
(defface info-title-4
  '((((type tty pc) (class color)) :weight bold))
  "Face for info titles at level 4."
  :group 'info)

;;; Faces for highlighting reference items
;;;###autoload
(defface info-command-ref-item
  '((((background dark)) (:inherit info-reference-item :foreground "#7474FFFF7474")) ; ~ light green
    (t (:inherit info-reference-item :foreground "Blue")))
  "Face used for \"Command:\" reference items in `info' manual."
  :group 'Info-Plus :group 'faces)
;;;###autoload
(defface info-constant-ref-item
  '((((background dark))
     (:inherit info-reference-item :foreground "DeepPink"))
    (t (:inherit info-reference-item :foreground "DeepPink")))
  "Face used for \"Constant:\" reference items in `info' manual."
  :group 'Info-Plus :group 'faces)
;;;###autoload
(defface info-function-ref-item
  '((((background dark))
     (:inherit info-reference-item :foreground "#4D4DDDDDDDDD")) ; ~ cyan
    (t (:inherit info-reference-item :foreground "DarkBlue")))
  "Face used for \"Function:\" reference items in `info' manual."
  :group 'Info-Plus :group 'faces)
;;;###autoload
(defface info-macro-ref-item
  '((((background dark))
     (:inherit info-reference-item :foreground "Yellow"))
    (t (:inherit info-reference-item :foreground "DarkMagenta")))
  "Face used for \"Macro:\" reference items in `info' manual."
  :group 'Info-Plus :group 'faces)
;;;###autoload
(defface info-reference-item
  ;; See comment for `info-quoted-name'.
  '((((background dark)) (:inherit info-fixed-pitch :background "gray12"))
    (t (:inherit info-fixed-pitch :background "gray88")))
  "Face used for reference items in `info' manual."
  :group 'Info-Plus :group 'faces)
;;;###autoload
(defface info-special-form-ref-item
  '((((background dark))
     (:inherit info-reference-item :foreground "Green")) ; ~ pink
    (t (:inherit info-reference-item :foreground "Magenta")))
  "Face used for \"Special Form:\" reference items in `info' manual."
  :group 'Info-Plus :group 'faces)
;;;###autoload
(defface info-syntax-class-item
  '((((background dark))
     (:inherit info-reference-item :foreground "#FFFF9B9BFFFF")) ; ~ pink
    (t (:inherit info-reference-item :foreground "DarkGreen")))
  "Face used for \"Syntax Class:\" reference items in `info' manual."
  :group 'Info-Plus :group 'faces)
;;;###autoload
(defface info-user-option-ref-item
  '((t (:inherit info-reference-item :foreground "Red")))
  "Face used for \"User Option:\" reference items in `info' manual."
  :group 'Info-Plus :group 'faces)
;;;###autoload
(defface info-variable-ref-item
  '((((background dark))
     (:inherit info-reference-item :foreground "Orange"))
    (t (:inherit info-reference-item :foreground "FireBrick")))
  "Face used for \"Variable:\" reference items in `info' manual."
  :group 'Info-Plus :group 'faces)

(when (and (require 'bookmark+ nil t) ; Emacs 24.2+ (do not bother for Emacs 23-24.1)
           (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 1))))

  (defface info-xref-bookmarked
    '((((background dark)) (:foreground "Violet"))
      (t (:foreground "DarkGreen")))
    "Face for bookmarked Info nodes."
    :group 'Info-Plus :group 'faces)

  )
 
;;(@* "User Options (Customizable)")
;;; User Options (Customizable) --------------------------------------

;;;###autoload
(defcustom Info-apropos-manuals 'all
  "Manuals for `info-apropos' to search.
The default value is the symbol `all', which means search all known
manuals (Info files) on your system.

Any other value is a list of manual names (strings), and it means
search only those manuals.

Manual names are the Info \"file\" names you see in parens before the
current node name, in Info, for example, `emacs' and `elisp'.

NOTE:

 Searching many manuals can take a while.  Once searched for a given
 pattern the search hits are cached, however, so this is a one-time
 cost.

 And when you use command `info-apropos', if you use a non-positive
 prefix arg then you are prompted for the manuals to search, ignoring
 the value of `Info-apropos-manuals'."
  :set #'(lambda (sym defs)
           (custom-set-default sym defs)
           (with-current-buffer (get-buffer-create "*info*")
             (setq Info-apropos-nodes  ()
                   Info-current-node   nil
                   Info-current-file   Info-apropos-file)))
  :type '(choice :tag "Manuals to Search"
                 (repeat :tag "Specific Manuals (files)" string)
                 (const  :tag "All Manuals"              all))
  :group 'help)

;; This and the version of function `Info-bookmark-jump' defined here are also defined in `bookmark+-1.el',
;; so that their feature is available if you use either `Info+' or `Bookmark+'.
;;
;;;###autoload
(defcustom Info-bookmark-use-only-node-not-file-flag t
  "Non-nil means an Info bookmark uses only the node name.
The recorded Info file name is ignored.  This means use only manuals
corresponding to the current Emacs session, regardless of the Emacs
version or platform used to record the bookmark.

A nil value means use the manuals whose absolute file names are
recorded in the bookmarks.  (But if the file doesn't exist or is
unreadable, then act as if the value is non-nil.)

A non-nil value means you can use the same bookmark with different
Emacs installations, including on different platforms.  A nil value
means that you can use a bookmark to consult the Info manual for a
different Emacs version from that of the current session."
  :type 'boolean :group 'Info-Plus)

;;;###autoload
(defcustom Info-breadcrumbs-in-header-flag nil
  "Non-nil means breadcrumbs are shown in the header line."
  :type 'boolean :group 'Info-Plus)

(defun info-buffer-name-function-default (file node)
  "Default function value for `info-buffer-name-function'."
  (format "*info* (%s) %s" file node))

;;;###autoload
(defcustom info-buffer-name-function 'info-buffer-name-function-default
  "Function to name Info buffer, or nil to use default buffer naming.
This is used by `info-manual+node-buffer-name-mode' and command
`info-rename-buffer'.

The function must accept the current Info file and node names as its
first two args, in that order."
  :type '(choice
          (const    :tag "Use default Info buffer naming" nil)
          (function :tag "Function accepting Info file and node names, returning string"))
  :group 'Info-Plus)

;;;###autoload
(defcustom Info-display-node-header-fn 'Info-display-node-default-header
  "Function to insert header by `Info-merge-subnodes'."
  :type 'function :group 'Info-Plus)

;;;###autoload
(defcustom Info-emphasis-regexp "_\\(\\sw+\\)_"
  "Regexp to match text enclosed in underscore (`_') characters.
It must have at least one regexp group.  Regexp group #1 gets
highlighted with face `info-emphasis'.  The enclosing underscore chars
are made invisible.

The default value matches the following (enclosed in underscores):
word, punctuation, and whitespace characters, plus hyphens, with at
least one word character.  Hyphen is included explicitly because it
generally has symbol syntax in Info.

Some possible values include:

_\\(\\sw+\\)_\t\t  (single words)

_\\(\\sw+\\(\\s-+\\sw+\\)*\\)_\t  (single words, maybe whitespace-separated)

_\\([^_\\n]+\\)_\t\t  (anything except underscore and newline chars)

_\\([^_]+\\)_\t\t  (anything except underscore chars)

_\\(\\(\\s-\\|\\sw\\|\\s.\\)+\\)_ (word, punctuation, whitespace)

_\\(\\(\\sw\\(\\s-\\|\\sw\\|\\s.\\)*\\)\\|\\(\\(\\s-\\|\\sw\\|\\s.\\)\\sw*\\)\\)_

_\\(\\(\\sw\\(\\s-\\|\\sw\\|\\s.\\|\\s(\\|\\s)\\)*\\)\\|
\\(\\(\\s-\\|\\sw\\|\\s.\\|\\s(\\|\\s)\\)\\sw*\\)\\)_ (but joined, with no newline)
  (like previous, but also open and close delimiters, such as ()[])

Note that any value can be problematic for some Info text - see
`Info-fontify-emphasis-flag'."
  :type 'regexp :group 'Info-Plus)

;;;###autoload
(defcustom Info-fit-frame-flag t
  "Non-nil means call `fit-frame' on Info buffer."
  :type 'boolean :group 'Info-Plus :group 'Fit-Frame)

;;;###autoload
(defcustom Info-fontify-angle-bracketed-flag t
  "Non-nil means `info' fontifies text within <...>.
A non-nil value has no effect unless `Info-fontify-quotations' is also
non-nil.

Note: This fontification can never be 100% reliable.  It aims to be
useful in most Info texts, but it can occasionally result in
fontification that you might not expect.  This is not a bug; it is
part of the design to be able to appropriately fontify a great variety
of texts.  Set this flag to nil if you do not find this fontification
useful.  You can use command `Info-toggle-fontify-angle-bracketed' to
toggle the option value."
  :type 'boolean :group 'Info-Plus)

(when (and (require 'bookmark+ nil t) ; Emacs 24.2+ (do not bother for prior)
           (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 1))))

  (defcustom Info-bookmarked-node-xref-faces '()
    "List of faces to use to classify bookmarked nodes.
The faces are used for links to bookmarked nodes.  They classify nodes
by serving as the values of bookmark tag \"bmkp-info-face\".

You can use any face for such a link.  The faces in this option list
are just provided as defaults when you are asked to enter a face for a
node link. "
    :type '(repeat face) :group 'Info-Plus)

  (defcustom Info-fontify-bookmarked-xrefs-flag t
    "Non-nil means fontify references to bookmarked nodes.
The face used is `info-xref-bookmarked'."
    :type 'boolean :group 'Info-Plus)

  )

;;;###autoload
(defcustom Info-fontify-custom-delimited (list nil ?' ?')
  "Non-nil means `info' fontifies text between delimiters you specify.
The text that is highlighted matches the value of variable
`info-custom-delimited-same-line-regexp'.

The option value is a list (ON/OFF BEG END), where:

 * Non-nil ON/OFF means fontifying is on, and nil means it is OFF.
 * BEG is the beginning delimiter character.
 * END is the ending delimiter character.

By default, this fontifying is off.  When turned on, by default the
text between apostrophes ('...') is fontified.

You can use command `Info-toggle-fontify-custom-delimited' to toggle
this fontifying on/off.  With a prefix arg, that command also prompts
you for the (new) delimiters to use.

You can use command `Info-define-custom-delimiting' to just do the
latter: set the delimiters.

For example, if you have an Info file that uses {...}, you can
highlight the enveloped ... text by setting the delimiters to chars {
and }, and turning the option on."
  :type '(list (boolean :tag "ON or OFF")
               (character :tag "Open delimiter")
               (character :tag "Close delimiter"))
  :group 'Info-Plus)

;;;###autoload
(defcustom Info-fontify-emphasis-flag t
  "Non-nil means `info' fontifies text between underscores (`_').
The text that is highlighted matches the value of option
`Info-emphasis-regexp'.

Note 1:
This fontification hides the underscores that surround text that is
emphasized.  Because this fontification is not 100% reliable (see Note
2), in cases where it is inappropriate or unhelpful you might want to
see the hidden underscore characters.  You can toggle showing all
hidden text (not just hidden underscores) using `M-x visible-mode'.
See (info) `Help-Inv' for more information about this.

Note 2:
This fontification can never be 100% reliable.  It aims to be useful
in most Info texts, but it can occasionally result in fontification
that you might not expect.  This is not a bug; it is part of the
design to be able to appropriately fontify a great variety of texts.
Set this flag to nil if you do not find this fontification useful.
You can use command `Info-toggle-fontify-emphasis' to toggle the
option value.

Note 3:
If internal variable `info-fontify-emphasis' is `nil' then emphasis is
never highlighted, and this option has no effect.  This gives you a
way to turn off all matching of `Info-emphasis-regexp'."
  :type 'boolean :group 'Info-Plus)

(defvar info-last-non-nil-fontify-extra-function nil
  "Last non-nil value of `Info-fontify-extra-function'.")

;;;###autoload
(defcustom Info-fontify-extra-function nil
  "If non-nil then a function used to provide additional highlighting.
The function is passed no arguments.

The function is invoked by `Info-fontify-node', before fontifying
reference items (`Info-fontify-reference-items').  When it is called,
the value of point is `point-min' in the node to be fontified."
  :type '(choice (const :tag "OFF - no extra fontifying" nil)
                 (function :tag "Function accepting no arguments"))
  :group 'Info-Plus)

(defvaralias 'Info-link-glossary-words 'Info-fontify-glossary-words)
;;;###autoload
(defcustom Info-fontify-glossary-words 'face-till-visit-+-mouseover-def
  "Non-nil means link first word occurrences in node to glossary entries.
Glossary terms of more than one word are not linked.

You can click `mouse-2' on the word, or use `RET' on it, to go to its
definition in the `Glossary' node of the current manual.

The link can optionally be shown, with face `info-glossary-word'.
Mouseover on the link can optionally show the glossary definition in a
tooltip (or in the echo area if `tooltip-mode' is disabled).

By default, a glossary link for a given word is shown (with face
`info-glossary-word') only until you follow a link for that word to
the glossary.  You can optionally show the links always, instead.

You can optionally hide glossary links always (not even show them
until visited).  You may feel that hiding such links reduces clutter.

If a link is hidden, it is still noticeable on mouseover: It's
highlighted using property `mouse-face', and a tooltip is shown for
it.

You can cycle or toggle the option value with command
`Info-cycle-fontify-glossary-words' or
`Info-toggle-fontify-glossary-words'.

See option `Info-glossary-fallbacks-alist' for the glossaries to look
in.

Note: This fontification can never be 100% reliable.  It aims to be
useful in most Info texts, but it can occasionally result in
fontification that you might not expect.  This is not a bug; it's part
of the design to be able to appropriately fontify a great variety of
texts.  Set this option to nil if you don't find this linking useful."
  :type
  '(choice
    (const :tag "OFF - don't link glossary words"                         nil)
    (const :tag "Show link until visited.  Show definition on mouseover"  face-till-visit-+-mouseover-def)
    (const :tag "Show link until visited.  NO definition on mouseover"    face-till-visit-+-NO-mouseover-def)
    (const :tag "Show link always.  Show definition on mouseover"         face-+-mouseover-def)
    (const :tag "Show link always.  NO definition on mouseover"           face-+-NO-mouseover-def)
    (const :tag "Hide link.  Show definition on mouseover"                NO-face-+-mouseover-def)
    (const :tag "Hide link.  NO definition on mouseover)"                 NO-face-+-NO-mouseover-def))
  :group 'Info-Plus)

;;;###autoload
(defcustom Info-fontify-indented-text-chars nil
  "A number means fontify text indented at least that many chars.
The default value is nil, which does nothing - no such fontifying.

The indented text is fontified with face `info-indented-text', which
by default uses a fixed-pitch font.

This can be useful especially if minor mode
`Info-variable-pitch-text-mode' is enabled, by keeping indented code
block, ASCII-art diagrams etc. in a fixed-pitch font.

A value of 10 works well for the Elisp manual.  But be aware that no
number works well across multiple manuals, because indented text at
any level is not necessarily something you want to fontify.

This fontification is not done for nodes named `Top', in order to
avoid fontifying continuation lines of menu-item descriptions."
  :type '(choice
          (const   :tag "OFF - don't fontify indented text"   nil)
          (integer :tag "Fontify text indented at least this many chars" :value 10))
  :group 'Info-Plus)

;;;###autoload
(defcustom Info-fontify-indented-text-manuals '(elisp)
  "List of manuals for which to use fixed-pitch for indented text.
This has no effect if `Info-fontify-indented-text-chars' is nil."
  :type '(repeat (symbol :tag "Manual")) :group 'info
  :set #'(lambda (sym defs)
           (custom-set-default sym defs)
           (mapatoms (lambda (x) (put x 'info-indented-text-regexp nil)))
           (dolist (manual  (symbol-value sym))
             (put manual 'info-indented-text-regexp 'info-indented-text-regexp))))


(define-obsolete-variable-alias 'Info-fontify-single-quote-flag 'Info-fontify-isolated-quote-flag "2020-10-22")
;;;###autoload
(defcustom Info-fontify-isolated-quote-flag t
  "Non-nil means `info' fontifies ' when not preceded by `....
A non-nil value has no effect unless `Info-fontify-quotations' is also
non-nil.

Note: This fontification can never be 100% reliable.  It aims to be
useful in most Info texts, but it can occasionally result in
fontification that you might not expect.  This is not a bug; it is
part of the design to be able to appropriately fontify a great variety
of texts.  Set this flag to nil if you do not find this fontification
useful.  You can use command `Info-toggle-fontify-isolated-quote' to
toggle the option value."
  :type 'boolean :group 'Info-Plus)


(define-obsolete-variable-alias 'Info-fontify-quotations-flag 'Info-fontify-quotations "2020-10-19")
;;;###autoload
(defcustom Info-fontify-quotations t
  "Non-nil means `info' fontifies text between quotes.
This applies to double-quoted text (“...” or \"...\") and text
between single-quotes (‘...’ or \\=`...\\=').

A value of `multiline' means fontify all such quotations, even those
that span multiple lines.

Any other non-nil value (`t' is the default value) means highlight all
such quotations, but in the case of all except \"...\" (strings), do
so only if the quoted sexp is all on the same line.

Note: This fontification can never be 100% reliable.  It aims to be
useful in most Info texts, but it can occasionally result in
fontification that you might not expect.  This is not a bug; it is
part of the design to be able to appropriately fontify a great variety
of texts.  Set this flag to nil if you do not find this fontification
useful.  You can use command `Info-cycle-fontify-quotations' to
cycle the option value."
  :type '(choice
          (const :tag "OFF - no quotations"                      nil)
          (const :tag "All quotations, even multiline"           multiline)
          (other :tag "All quotations, but on same-line except \"...\"" t))
  :group 'Info-Plus)

;;;###autoload
(defcustom Info-fontify-reference-items-flag t
  "Non-nil means `info' fontifies reference items such as \"Function:\"."
  :type 'boolean :group 'Info-Plus)

;;;###autoload
(defcustom Info-glossary-fallbacks-alist '((semantic . semantic) (emacs . t))
  "Alist of fallback manuals to use for glossary links.
Each element is of the form (GLOSSARY-MANUAL . MANUALS).  The glossary
of GLOSSARY-MANUAL is used to create glossary links for each manual in
MANUALS, if it has no glossary of its own.  If MANUALS is `t' then all
manuals use the glossary of GLOSSARY-MANUAL.

Put any alist entries that have `t' for MANUALS after all entries that
have an explicit list for MANUALS, so that the explicit lists are
handled before the `t' catch-alls.

This option has no effect if `Info-fontify-glossary-words' is nil."
  :type '(alist
          :key-type   (symbol :tag "Fallback manual (use its glossary)")
          :value-type (choice
                       (repeat (symbol :tag "A manual that uses fallback manual's glossary"))
                       (const :tag "Use fallback for all manuals with no glossary" t)))
  :group 'Info-Plus)

(when (and (require 'bookmark+ nil t)   ; Emacs 24.4+
           (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 3))))

  (defcustom Info-node-access-invokes-bookmark-flag t
    "Non-nil means invoke the bookmark when you access an Info node.
This applies to Info bookmarks whose names correspond to the default
name.  This is normally the full node name, `(MANUAL) NODE', where
MANUAL is the lowercase name of the Info manual.  For example, node
`Modes' in the Emacs manual has full name `(emacs) Modes', and the
bookmark must have that same name.

This automatic bookmark invocation can be useful to update the
bookmark data, such as the number of visits to the node."
    :type 'boolean :group 'Info-Plus)

  )

(when (fboundp 'advice-add)             ; Emacs 24.4+

  (defcustom Info-saved-history-file (locate-user-emacs-file "info-history" ".emacs.info-history")
    "File where `Info-persist-history-mode' saves `Info-history-list'."
    :type '(file :must-match t) :group 'Info-Plus)

  )

;;;###autoload
(defcustom Info-saved-nodes ()
  "List of Info node names you can visit using `\\<Info-mode-map>\\[Info-virtual-book]'.
Each node name is a string.  The node name can be absolute, including
a filename, such as \"(emacs)Basic\", or it can be relative, such as
\"Basic\".
You can customize this option, but you can also add node names to it
easily using `\\[Info-save-current-node]' (`Info-save-current-node')."
  :type '(repeat (string :tag "Node name")) :group 'info)

;;;###autoload
(defcustom Info-subtree-separator "\n* "
  "A string used to separate Info node descriptions.
Inserted by `Info-merge-subnodes' just before each node title.
Setting this to a string that includes a form-feed (^L), such as
\"\\f\\n* \", will cause a page break before each node description.

Use command `set-variable' to set this, quoting any control characters
you want to include, such as form-feed (^L) and newline (^J), with ^Q.
For example, type `^Q^L^Q^J* ' to set this to \"\\f\\n* \"."
  :type 'string :group 'Info-Plus)

;;;###autoload
(defcustom Info-toc-outline-no-redundancy-flag t
  "Non-nil means `Info-toc-outline' TOC has no redundancy.
If nil then section headings from the TOC manual are included, and 
nodes can be repeated because they are in more than one section."
  :type 'boolean :group 'Info-Plus)
 

;;(@* "Internal Variables")
;;; Internal Variables -----------------------------------------------

(defvar info-fontify-emphasis t
  "Non-nil means allow `Info-fontify-emphasis-flag' to work.
If nil then emphasis is never fontified, regardless of that flag.")

(defvar Info-glossary-link-history ()
  "List of glossary words whose links have been followed so far.
This is empty at the beginning of an Emacs session, and after you use
`\\[Info-history-clear].")

(defvar info-glossary-link-map (let ((map  (make-sparse-keymap)))
                                 (define-key map (kbd "RET")   'Info-goto-glossary-definition)
                                 (define-key map [mouse-2]     'Info-goto-glossary-definition)
                                 (define-key map [follow-link] 'mouse-2)
                                 map)
  "Keymap for glossary-word links.")

(defvar Info-link-faces '(info-xref info-xref-visited info-xref-bookmarked)
  "List of `font-lock-face' property faces used for links in Info.
Used to prevent glossary-word highlighting and linking from overriding
node links, when` Info-fontify-glossary-words' is non-nil.")

;; I reported this as Emacs bug #3312.  If it gets fixed, this can be removed.
(defvar Info-mode-syntax-table
  (let ((table  (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?'    "." table) ; Punctuation syntax for apostrophe (').
    (modify-syntax-entry ?\240 "." table) ; Punctuation syntax for non-breaking space.
    table)
  "Syntax table for `info'.")

(defvar Info-merged-map nil "Keymap for merged Info buffer.")
(if Info-merged-map
    nil
  (setq Info-merged-map  (make-keymap))
  (suppress-keymap Info-merged-map)
  (define-key Info-merged-map "." 'beginning-of-buffer)
  (define-key Info-merged-map "\t" 'Info-next-reference)
  (define-key Info-merged-map "\e\t" 'Info-prev-reference)
  (define-key Info-merged-map "b" 'beginning-of-buffer)
  (define-key Info-merged-map "q" 'quit-window)
  (define-key Info-merged-map "s" 'nonincremental-re-search-forward)
  (define-key Info-merged-map "\M-s" 'nonincremental-re-search-forward))

(defvar Info-no-glossary-manuals '(info efaq efaq-32 elisp eintr
                                        ada-mode ccmode idlwave
                                        octave-mode org vhdl-mode
                                        nxml-mode erc eudc
                                        emacs-gnutls gnus mh-e
                                        mairix-el message newsticker
                                        pgg rcirc sasl sc sieve tramp
                                        autotype bovine calc dired-x
                                        ede edt eieio ert eww epa
                                        ebrowse ediff eshell flymake
                                        forms htmlfontify ido pcl-cvs
                                        reftex remember ses srecode
                                        speedbar todo-mode vip viper
                                        wisent woman auth cl dbus
                                        emacs-mime smtpmail url widget)
  ;; Emacs and Semantic manuals have a glossary.
  "List of Info manuals that have no manuals.
The elements are symbols whose names can be used as string arg to
`info'.

You can set this manually, but you need not do so.  Manuals are added
automatically, when Emacs can't find a `Glossary' node in them.")

(defvar Info-breadcrumbs-depth-internal Info-breadcrumbs-depth
  "Current breadcrumbs depth for Info.")

(defvar info-custom-delimited-same-line-regexp "'\\(?:[^\n']\\|\\\\\\(?:.\\|\n\\)\\)+'"
  "Regexp to match text between custom delimiters.
You can use command `Info-define-custom-delimiting' (or command
`Info-toggle-fontify-custom-delimited' with `C-u') to define the
regexp.  They prompt you for the delimiters to use.

\(You can of course also let-bind this in Lisp code.\)")

(defvar info-indented-text-regexp info-nomatch
  "Dummy value.  Function `info-indented-text-regexp' is used instead.")

(defvar info-isolated-backquote-regexp "`\\(.\\|\n\\)"
  ;; Another possibility: "[^`]`"
  ;; OK in practice, since backquote is rarer than quote, but still, play safe as with quote.
  ;; "\\(`\\)\\(?:[^\n']\\|\\\\\\(?:.\\|\n\\)\\)*[^']"
  "Regexp to match an isolated backquote character.
That is, one that is not part of `...'.")

(defvar info-isolated-quote-regexp "\\(.\\|\n\\)'"
  ;; Another possibility: "[^']'"
  ;; "[^`]\\(?:[^\n']\\|\\\\\\(?:.\\|\n\\)\\)*'" ; Can be way too slow in rare cases.
  "Regexp to match an isolated single-quote character.
That is, one that is not part of `...'.")


;; Match has, inside “...”, "...", ‘...’, or `...', zero or more of these characters:
;;   - any character except ”, ", ’, or ', respectively
;;   - \ followed by any character
;;
;; The regexp matches also ‘...’, `...', “...”, and "..." where at least one of the
;; ‘, ’, `, ', “, ”, or " is escaped by a backslash.
;; So we check those cases explicitly and do not highlight them.
;;
(defvar info-quotation-regexp
  (concat
   "\"\\(?:[^\"\\]\\|\\\\\\(?:.\\|\n\\)\\)*\"\\|" ; "..."
   "`\\(?:[^']\\|\\\\\\(?:.\\|\n\\)\\)+'\\|"      ; `...'
   "‘\\(?:[^’]\\|\\\\\\(?:.\\|\n\\)\\)+’\\|"      ; ‘...’
   "“\\(?:[^”]\\|\\\\\\(?:.\\|\n\\)\\)+”"         ; “...”
   )
  "Regexp to match `...', ‘...’, “...”, \"...\".
If ... contains an end char then that char must be backslashed.")

;; (rx (or (seq ?\"
;;              (zero-or-more (or (not (any ?\" ?\\))  (seq ?\\ anything)))
;;              ?\")
;;         (seq ?\`
;;              (zero-or-more (or (not (any ?'))  (seq ?\\ anything)))
;;              ?\')
;;         (seq ?‘
;;              (zero-or-more (or (not (any ?’))  (seq ?\\ anything)))
;;              ?’)
;;         (seq ?“
;;              (zero-or-more (or (not (any ?”))  (seq ?\\ anything)))
;;              ?”)))

(defvar info-quotation-same-line-regexp
  (concat
   "\"\\(?:[^\"\\]\\|\\\\\\(?:.\\|\n\\)\\)*\"\\|" ; "..."
   "`\\(?:[^\n']\\|\\\\\\(?:.\\|\n\\)\\)+'\\|"    ; `...' on one line
   "‘\\(?:[^\n’]\\|\\\\\\(?:.\\|\n\\)\\)+’\\|"    ; ‘...’ on one line
   "“\\(?:[^\n”]\\|\\\\\\(?:.\\|\n\\)\\)+”"       ; “...” on one line
   )
  "`info-quotation-regexp', but on same line (all but \"...\").")

;; (rx (or (seq ?\"
;;              (zero-or-more (or (not (any ?\" ?\\))  (seq ?\\ anything)))
;;              ?\")
;;         (seq ?\`
;;              (one-or-more  (or (not (any ?\n ?'))   (seq ?\\ anything)))
;;              ?\')
;;         (seq ?‘
;;              (one-or-more  (or (not (any ?\n ?’))   (seq ?\\ anything)))
;;              ?’)
;;         (seq ?“
;;              (one-or-more  (or (not (any ?\n ?”))   (seq ?\\ anything)))
;;              ?”)))

(defvar info-quoted+<>-regexp
  (concat
   "\"\\(?:[^\"\\]\\|\\\\\\(?:.\\|\n\\)\\)*\"\\|"             ; "..."
   "`\\(?:[^']\\|\\\\\\(?:.\\|\n\\)\\)+'\\|"                  ; `...'
   "‘\\(?:[^’]\\|\\\\\\(?:.\\|\n\\)\\)+’\\|"                  ; ‘...’
   "“\\(?:[^”]\\|\\\\\\(?:.\\|\n\\)\\)+”\\|"                  ; “...”
   "<\\(?:[[:alpha:]][^>]*\\|\\(?:\\\\\\(?:.\\|\n\\)\\)+\\)>" ; <...>
   )
  "Same as `info-quotation-regexp', but matches also <...>.
If ... contains an end char then that char must be backslashed.")

;; (rx (or (seq ?\"
;;              (zero-or-more (or (not (any ?\" ?\\))  (seq ?\\ anything)))
;;              ?\")
;;         (seq ?\`
;;              (zero-or-more (or (not (any ?'))       (seq ?\\ anything)))
;;              ?\')
;;         (seq ?‘
;;              (zero-or-more (or (not (any ?’))       (seq ?\\ anything)))
;;              ?’)
;;         (seq ?“
;;              (zero-or-more (or (not (any ?”))       (seq ?\\ anything)))
;;              ?”)
;;         (seq ?<
;;              (or (seq (any alpha) (zero-or-more (not (any ?>))))
;;                  (zero-or-more (seq ?\\ anything)))
;;              ?>)))

(defvar info-quoted+<>-same-line-regexp
  (concat
   "\"\\(?:[^\"\\]\\|\\\\\\(?:.\\|\n\\)\\)*\"\\|" ; "..."
   "`\\(?:[^\n']\\|\\\\\\(?:.\\|\n\\)\\)+'\\|"    ; `...'
   "‘\\(?:[^\n’]\\|\\\\\\(?:.\\|\n\\)\\)+’\\|"    ; ‘...’
   "“\\(?:[^\n”]\\|\\\\\\(?:.\\|\n\\)\\)+”\\|"    ; “...”
   "<\\(?:[[:alpha:]][^\n>]*\\|\\(?:\\\\\\(?:.\\|\n\\)\\)+\\)>" ; <...>
   )
  "`info-quoted+<>-same-line-regexp', but on same line (all but \"...\").")

;; (rx (or (seq ?\"
;;              (zero-or-more (or (not (any ?\" ?\\))  (seq ?\\ anything)))
;;              ?\")
;;         (seq ?\`
;;              (one-or-more  (or (not (any ?\n ?'))   (seq ?\\ anything)))
;;              ?\')
;;         (seq ?‘
;;              (one-or-more  (or (not (any ?\n ?’))   (seq ?\\ anything)))
;;              ?’)
;;         (seq ?“
;;              (one-or-more  (or (not (any ?\n ?”))   (seq ?\\ anything)))
;;              ?”)
;;         (seq ?<
;;              (or (seq (any alpha) (zero-or-more (not (any ?\n ?>))))
;;                  (one-or-more (seq ?\\ anything)))
;;              ?>)))

;; See `info-utils.c': `degrade_utf8' in Texinfo for the source of the list below.
(when (< emacs-major-version 27)        ; Emacs 23-26.
  (defvar info-symbols-and-replacements '((?\‘ . "`")
                                          (?\’ . "'")
                                          (?\“ . "\"")
                                          (?\” . "\"")
                                          (?© . "(C)")
                                          (?\》 . ">>")
                                          (?→ . "->")
                                          (?⇒ . "=>")
                                          (?⊣ . "-|")
                                          (?★ . "-!-")
                                          (?↦ . "==>")
                                          (?‐ . "-")
                                          (?‑ . "-")
                                          (?‒ . "-")
                                          (?– . "-")
                                          (?— . "--")
                                          (?− . "-")
                                          (?… . "...")
                                          (?• . "*"))
    "Alist of Unicode chars used in Info files and their ASCII translations.
Each element is a cons with car a char and cdr an ASCII string."))

(defvar Info-toc-outline-map (let ((map  (make-sparse-keymap))) (set-keymap-parent map Info-mode-map))
  "Keymap for Info TOC with outlining.")
 
;;(@* "New Commands")
;;; New Commands -----------------------------------------------------

;; Make `Info-find-emacs-command-nodes' look for these commands in the Emacs manual.
;; In particular, don't look for command `info' in Info manual, because that has no index.
(put 'info 'info-file "emacs")
(put 'Info-goto-emacs-command-node 'info-file "emacs")
(put 'Info-goto-emacs-key-command-node 'info-file "emacs")

;;;###autoload (autoload 'Info-mouse-follow-nearest-node-new-window "info+")
(defun Info-mouse-follow-nearest-node-new-window (click)
  "Open the link at the mouse pointer in a new window."
  (interactive "e")
  (Info-mouse-follow-nearest-node click t))

;;;###autoload (autoload 'Info-follow-nearest-node-new-window "info+")
(defun Info-follow-nearest-node-new-window ()
  "Open the link near the text cursor in a new window."
  (interactive)
  (Info-follow-nearest-node t))

;;;###autoload (autoload 'Info-history-clear "info+")
(defun Info-history-clear (&optional msgp)
  "Clear Info history and reload current manual.
This resets links, including glossary links, to their initial,
unvisited state."
  (interactive (progn (unless (y-or-n-p "Clear the Info history? ") (info-user-error "OK, canceled"))
                      (list t)))
  (setq Info-history                ()
        Info-history-list           ()
        Info-glossary-link-history  ())
  (when (derived-mode-p 'Info-mode) (revert-buffer nil t))
  (when msgp (message "Info history cleared")))

(when (fboundp 'advice-add)             ; Emacs 24.4+

  (define-minor-mode Info-persist-history-mode
      "Automatically persist the Info history in `Info-saved-history-file'."
    :init-value nil :global t :group 'Info-Plus
    (cond (Info-persist-history-mode
           (add-hook 'kill-emacs-hook 'Info-save-history-list)
           (advice-add 'Info-kill-buffer :before 'Info-save-history-list)
           (advice-add 'Info-directory :after 'Info-restore-history-list))
          (t
           (remove-hook 'kill-emacs-hook 'Info-save-history-list)
           (advice-remove 'Info-kill-buffer 'Info-save-history-list)
           (advice-remove 'Info-directory 'Info-restore-history-list))))

  )

(when (> emacs-major-version 23) ; Emacs 23 `revert-buffer' invokes a poor `kill-buffer' etc.

  (defun Info-change-visited-status (start end &optional arg)
    "Change whether the nodes in the region have been visited.
If the region is not active then act on only the node at point.
No prefix arg means toggle the visited status of each node.
A non-negative prefix arg means consider the nodes visited.
A negative prefix arg means consider the nodes not visited."
    (interactive "r\nP")
    (let ((toggle   (not arg))
          (visit    (and arg  (natnump (prefix-numeric-value arg))))
          (unvisit  (and arg  (not (natnump (prefix-numeric-value arg)))))
          (opoint   (point))
          (omark    (mark))
          (active   mark-active)
          (file     (Info-find-file Info-current-file))
          node visitedp trim)
      (unless mark-active (setq start  (setq end  (point))))
      (save-excursion
        (goto-char start)
        (while (<= (point) end)
          (unless (setq node  (Info-node-name-at-point)) (Info-next-reference))
          (when (setq node  (Info-node-name-at-point))
            (save-match-data
              (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)" node)
              (setq node  (match-string 3 node))
              (setq trim  (string-match "\\s +\\'" node)))
            (when trim (setq node  (substring node 0 trim)))
            (when (or (not node)  (string= node "")) (setq node  "Top"))
            (setq visitedp  (member (list file node) Info-history-list))
            (if (and visitedp  (or toggle  unvisit))
                (setq Info-history-list  (delete (list file node) Info-history-list))
              (if (and (not visitedp)  (or toggle  visit))
                  (setq Info-history-list  (cons (list file node) Info-history-list)))))
          (when (> (forward-line 1) 0) (goto-char (point-max)))))
      (when (derived-mode-p 'Info-mode) (revert-buffer nil t))
      (when omark (set-mark omark))
      (goto-char opoint)
      (message (if toggle
                   "Node visited status toggled"
                 (format "Node visited status is now %s" (if visit 'VISITED 'UNvisited))))
      (if (not active)
          (deactivate-mark)
        (activate-mark)
        (setq deactivate-mark  nil))))

  )

;; Not bound.  Use `Info-change-visited-status' instead.
;;
;;;###autoload (autoload 'Info-make-node-unvisited "info+")
(defun Info-make-node-unvisited (node &optional msgp)
  "Reset the visited status of NODE to unvisited."
  (interactive (list (or (Info-node-name-at-point)  (Info-read-node-name "Node: " Info-current-node))
                     t))
  (let (file trim)
    (save-match-data
      (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)" node)
      (setq file  (if (= (match-beginning 1) (match-end 1)) "" (match-string 2 node))
            node  (match-string 3 node)
            trim  (string-match "\\s +\\'" file))
      (when trim (setq file  (substring file 0 trim)))
      (setq trim  (string-match "\\s +\\'" node)))
    (when trim (setq node  (substring node 0 trim)))
    (when (or (not node)  (string= node "")) (setq node  "Top"))
    (when (or (not file)  (string= file "")) (setq file  Info-current-file))
    (setq file               (Info-find-file file)
          Info-history-list  (remove (list file (substring-no-properties node)) Info-history-list))
    ;; Emacs 23 has a poor `kill-buffer', which is invoked by `revert-buffer' and deletes
    ;; the window/frame if dedicated.
    (when (and (> emacs-major-version 23)  (derived-mode-p 'Info-mode)) (revert-buffer nil t))
    (when msgp (message "Node %sis now unvisited" 
                        (if (string= "dir" Info-current-file) ""
                          (format "`%s%s' "
                                  (if (equal file Info-current-file)
                                      ""
                                    (format "(%s) " (file-name-nondirectory file)))
                                  node))))))

;; I made this a global minor mode and turned it on by default, contrary to "the rules".
;; I did this so (a) users could easily customize it but (b) it would be on by default, otherwise.
;;
;;;###autoload (autoload 'Info-breadcrumbs-in-mode-line-mode "info+")
(define-minor-mode Info-breadcrumbs-in-mode-line-mode
    "Toggle the use of breadcrumbs in Info mode line.
With arg, show breadcrumbs iff arg is positive.
Change the default behavior by customizing option
`Info-breadcrumbs-in-mode-line-mode'."
  :init-value t :global t :group 'mode-line :group 'Info-Plus
  (if (not Info-breadcrumbs-in-mode-line-mode)
      (setq Info-breadcrumbs-depth-internal  0
            mode-line-format                 (default-value 'mode-line-format))
    (setq Info-breadcrumbs-depth-internal  Info-breadcrumbs-depth)
    (Info-insert-breadcrumbs-in-mode-line)))

;;;###autoload (autoload 'Info-toggle-breadcrumbs-in-header "info+")
(defun Info-toggle-breadcrumbs-in-header (&optional msgp)
  "Toggle option `Info-breadcrumbs-in-header-flag'.
Toggles showing breadcrumbs in the Info header (top of the node).
This means the area at the top of the node, not the separate header
line from non-nil `Info-use-header-line'."
  (interactive "p")
  (setq Info-breadcrumbs-in-header-flag  (not Info-breadcrumbs-in-header-flag))
  (when msgp (message "Showing breadcrumbs in Info header is now %s"
                      (if Info-breadcrumbs-in-header-flag "ON" "OFF"))))

(defalias 'Info-toggle-breadcrumbs-in-header-line 'Info-toggle-breadcrumbs-in-header)
(make-obsolete 'Info-toggle-breadcrumbs-in-header-line 'Info-toggle-breadcrumbs-in-header "2014/03/04")


(when (boundp 'Info-node-access-invokes-bookmark-flag) ; Emacs 24.4+

  (defun Info-toggle-node-access-invokes-bookmark (&optional msgp)
    "Toggle option `Info-node-access-invokes-bookmark-flag'."
    (interactive "p")
    (setq Info-node-access-invokes-bookmark-flag  (not Info-node-access-invokes-bookmark-flag))
    (Info-refontify-current-node)
    (when msgp (message "`Info-node-access-invokes-bookmark-flag' is now %s"
                        (if Info-node-access-invokes-bookmark-flag 'ON 'OFF))))

  )

(defvar info-remap-default-face-cookie nil
  "Cookie returned by `Info-remap-default-face-to-variable-pitch'.")

;;;###autoload (autoload 'Info-remap-default-face-to-variable-pitch "info+")
(defun Info-remap-default-face-to-variable-pitch ()
  "Remap face `default' to face `variable-pitch'."
  (setq info-remap-default-face-cookie  (face-remap-add-relative 'default 'variable-pitch)))

;;;###autoload (autoload 'Info-variable-pitch-text-mode "info+")
(define-minor-mode Info-variable-pitch-text-mode
  "Use a variable-pitch font for Info text."
  :init-value nil :global t :group 'Info-Plus
  (unless (derived-mode-p 'Info-mode) (info))    
  (cond (Info-variable-pitch-text-mode
         (Info-remap-default-face-to-variable-pitch))
        (info-remap-default-face-cookie
         (face-remap-remove-relative info-remap-default-face-cookie))))

;;;###autoload (autoload 'info-manual+node-buffer-name-mode "info+")
(define-minor-mode info-manual+node-buffer-name-mode
  "Name Info buffers using the Info manual (file) and node names."
  :init-value nil :global t :group 'Info-Plus
  (if (and info-buffer-name-function  info-manual+node-buffer-name-mode)
      (add-hook 'Info-selection-hook #'info-rename-buffer)
    (remove-hook 'Info-selection-hook #'info-rename-buffer)))

;;;###autoload (autoload 'info-rename-buffer "info+")
(defun info-rename-buffer (&optional msgp)
  "Rename current Info buffer, per `info-buffer-name-function'.
To do this automatically when you visit any Info buffer, use
`info-manual+node-buffer-name-mode'."
  (interactive "p")
  (when info-buffer-name-function
    (unless Info-current-file (error "This function must be invoked from Info"))
    (let* ((manual    (file-name-sans-extension (file-name-nondirectory Info-current-file)))
           (node      Info-current-node)
           (bufname   (funcall info-buffer-name-function manual node)))
      (unless (equal (buffer-name) bufname)
        (rename-buffer bufname t)
        (when msgp (message "Info buffer renamed to `%s'" bufname))))))


(when (and (require 'bookmark+ nil t) ; Emacs 24.2+ (do not bother for prior)
           (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 1))))

  (defun Info-set-face-for-bookmarked-xref (node)
    "Specify the face to use for Info links to bookmarked NODE.
Sets the value of bookmark tag \"bmkp-info-face\" to a face symbol you
name.

If option `Info-bookmarked-node-xref-faces' is non-nil then only the
faces in that list are available for completion, but you can enter any
face name.  If that option is nil then all faces are available for
completion.

NODE defaults to the bookmarked node named at point.  If none then you
are prompted for NODE."
    (interactive (list (or (Info-bookmark-name-at-point)  (Info-read-bookmarked-node-name))))
    (let* ((alist  (bmkp-info-alist-only))
           (bmk    (bmkp-get-bookmark-in-alist node t alist)))
      (unless bmk (error "No Info bookmark for node `%s'" node))
      (bmkp-set-tag-value bmk "bmkp-info-face"
                          (if Info-bookmarked-node-xref-faces
                              (intern (completing-read "Face: " Info-bookmarked-node-xref-faces
                                                       (lambda (ff) (memq ff (face-list)))))
                            (read-face-name "Face" 'info-xref-bookmarked)))))

  (define-key Info-mode-map (kbd "C-x f") 'Info-set-face-for-bookmarked-xref)


  (defun Info-toggle-fontify-bookmarked-xrefs (&optional msgp)
    "Toggle option `Info-fontify-bookmarked-xrefs-flag'."
    (interactive "p")
    (setq Info-fontify-bookmarked-xrefs-flag  (not Info-fontify-bookmarked-xrefs-flag))
    (Info-refontify-current-node)
    (when msgp (message "`Info-fontify-bookmarked-xrefs-flag' is now %s"
                        (if Info-fontify-bookmarked-xrefs-flag 'ON 'OFF))))

  )

(defsubst info-custom-delim-1 ()
  "Beginning custom delimiter."
  (cadr Info-fontify-custom-delimited))

(defsubst info-custom-delim-2 ()
  "Ending custom delimiter."
  (car (cddr Info-fontify-custom-delimited)))

;;;###autoload (autoload 'Info-define-custom-delimiting "info+")
(defun Info-define-custom-delimiting (&optional msgp)
  "Read custom delimiter chars and set fontifying regexp from them.
You can also do this when toggling fontification of the delimited text
with command `Info-toggle-fontify-custom-delimited', by using a prefix
argument.

The default value uses ' (apostrophe) for both beginning and ending
delimiter, with the result that in '...' the ... is fontified."
  (interactive "p")
  (let ((read-fn  (if (fboundp 'read-char-from-minibuffer)
                      'read-char-from-minibuffer ; Emacs 27+
                    'read-char-exclusive)))
    (setcar (cdr Info-fontify-custom-delimited)  (funcall read-fn "Beginning delimiter (character): "
                                                          'INHERIT-INPUT-METHOD 5))
    (setcar (cddr Info-fontify-custom-delimited) (funcall read-fn "Ending delimiter (character): "
                                                          'INHERIT-INPUT-METHOD 5))
    (setq info-custom-delimited-same-line-regexp  (format "[%c]\\(?:[^\n%c]\\|\\\\\\(?:.\\|\n\\)\\)+[%c]"
                                                          (info-custom-delim-1)
                                                          (info-custom-delim-2)
                                                          (info-custom-delim-2))))
  (when msgp (message "Custom delimiters are now `%c' and `%c'" (info-custom-delim-1) (info-custom-delim-2))))

;;;###autoload (autoload 'Info-toggle-fontify-custom-delimited "info+")
(defun Info-toggle-fontify-custom-delimited (&optional newp msgp)
  "Toggle option `Info-fontify-custom-delimited'.
With a prefix arg, also set the beginning and ending delimiter chars -
you are prompted for the chars to use."
  (interactive "P\np")
  (setq Info-fontify-custom-delimited (cons (not (car Info-fontify-custom-delimited))
                                            (cdr Info-fontify-custom-delimited)))
  (when newp (call-interactively #'Info-define-custom-delimiting))
  (Info-refontify-current-node)
  (when msgp (message "`Info-fontify-custom-delimited' is now %s%s"
                      (if (car Info-fontify-custom-delimited) 'ON 'OFF)
                      (if newp
                          (format ", with delimiters %c and %c" (info-custom-delim-1) (info-custom-delim-2))
                        ""))))

;; Turn off highlighting of isolated quote marks for non-Emacs manuals
;;
(put 'info         'info-isolated-quote-regexp     info-nomatch)
(put 'info         'info-isolated-backquote-regexp info-nomatch)
(put 'ada-mode     'info-isolated-quote-regexp     info-nomatch)
(put 'ada-mode     'info-isolated-backquote-regexp info-nomatch)
(put 'nxml-mode    'info-isolated-quote-regexp     info-nomatch)
(put 'nxml-mode    'info-isolated-backquote-regexp info-nomatch)
(put 'emacs-gnutls 'info-isolated-quote-regexp     info-nomatch)
(put 'emacs-gnutls 'info-isolated-backquote-regexp info-nomatch)
(put 'bovine       'info-isolated-quote-regexp     info-nomatch)
(put 'bovine       'info-isolated-backquote-regexp info-nomatch)
(put 'calc         'info-isolated-quote-regexp     info-nomatch)
(put 'calc         'info-isolated-backquote-regexp info-nomatch)
(put 'eww          'info-isolated-quote-regexp     info-nomatch)
(put 'eww          'info-isolated-backquote-regexp info-nomatch)
(put 'epa          'info-isolated-quote-regexp     info-nomatch)
(put 'epa          'info-isolated-backquote-regexp info-nomatch)
(put 'eshell       'info-isolated-quote-regexp     info-nomatch)
(put 'eshell       'info-isolated-backquote-regexp info-nomatch)
(put 'pcl-cvs      'info-isolated-quote-regexp     info-nomatch)
(put 'pcl-cvs      'info-isolated-backquote-regexp info-nomatch)
(put 'srecode      'info-isolated-quote-regexp     info-nomatch)
(put 'srecode      'info-isolated-backquote-regexp info-nomatch)
(put 'todo-mode    'info-isolated-quote-regexp     info-nomatch)
(put 'todo-mode    'info-isolated-backquote-regexp info-nomatch)
(put 'wisent       'info-isolated-quote-regexp     info-nomatch)
(put 'wisent       'info-isolated-backquote-regexp info-nomatch)
(put 'smtpmail     'info-isolated-quote-regexp     info-nomatch)
(put 'smtpmail     'info-isolated-backquote-regexp info-nomatch)



(defun Info-toggle-fontify-local (regexp-var &optional readp msgp)
  "Toggle the local value of variable REGEXP-VAR for this manual.
Helper for `Info-toggle-fontify-local-*' commands.
Non-nil READP means read a new value for it, instead of toggling it.
Non-nil MSGP means show a status message when done."
  (let* ((manual    (Info-manual-symbol))
         (val       (get manual regexp-var))
         (last-var  (intern (format "last-%s" regexp-var)))
         (last-val  (get manual last-var))
         (default   (symbol-value regexp-var)))
    (if (or readp  (and (not val)  (not last-val)))
        ;; Read new regexp.
        (let ((newval  (read-regexp
                        (format "Regexp for `%s' in `%s' (Use `$-' for NO such local highlighting): "
                                regexp-var manual)
                        default)))
          (when (equal newval default)
            (message "That's the _global_ regexp.  (Maybe use it as a guide.)  Try again.") (sleep-for 2)
            (setq newval (read-regexp (format "Regexp for `%s' in `%s' (Use `$-' for NO such highlight): "
                                              regexp-var manual)
                                      default)))
          ;; If regexp read contains `$' followed by any char then use symbol `nomatch' instead.
          (let ((off-rx (string-match "[$]" newval)))
            (when (and off-rx  (< (match-end 0) (length newval))) (setq newval  info-nomatch)))
          (put manual regexp-var newval)
          (put manual last-var (setq last-val  (setq val  newval))))
      ;; Toggle current value.
      (let ((newval  (if val nil last-val)))
        (unless (equal val last-val) (put manual last-var (setq last-val  val)))
        (put manual regexp-var (setq val  newval))))
    (Info-refontify-current-node)
    (when msgp
      (if (eq info-nomatch val)
          (message "`%s' manual now has NO `%s' highlighting (OVERRIDES global)" manual regexp-var)
        (message "`%s' manual local `%s' highlighting is now %s"
                 manual
                 regexp-var
                 (if val 'ON (format "OFF (global is %s)"
                                     (if (symbol-value regexp-var) 'ON 'OFF))))))))

;;;###autoload (autoload 'Info-toggle-fontify-local-custom-delimited "info+")
(defun Info-toggle-fontify-local-custom-delimited (&optional readp msgp)
  "Toggle `info-custom-delimited-same-line-regexp' for current Info manual.
With a prefix arg, read a new value for it instead.

When reading a new value, You can use `$-' as the regexp, to NOT do
any such local highlighting, that is, to override any such global
highlighting.  (`$-' is a regexp that cannot match anything.)"
  (interactive "P\np")
  (Info-toggle-fontify-local 'info-custom-delimited-same-line-regexp readp msgp))

;;;###autoload (autoload 'Info-toggle-fontify-all "info+")
(defun Info-toggle-fontify-all (&optional msgp)
  "Toggle all Info+ fontify options.
This turns all of these options off (sets them to nil), if any of them
is on, and it turns them all on, if any is off:

 Info-fontify-angle-bracketed-flag
 Info-fontify-bookmarked-xrefs-flag (if defined)
 Info-fontify-emphasis-flag
 Info-fontify-glossary-words
 Info-fontify-isolated-quote-flag
 Info-fontify-quotations
 Info-fontify-reference-items-flag
 Info-fontify-visited-nodes"
  (interactive "p")
  (let ((opts  (append '(Info-fontify-angle-bracketed-flag
                         Info-fontify-emphasis-flag
                         Info-fontify-glossary-words
                         Info-fontify-isolated-quote-flag
                         Info-fontify-quotations
                         Info-fontify-reference-items-flag
                         Info-fontify-visited-nodes)
                       (and (boundp 'Info-fontify-bookmarked-xrefs-flag)
                            '(Info-fontify-bookmarked-xrefs-flag)))))
    (if (or Info-fontify-angle-bracketed-flag
            (and (boundp 'Info-fontify-bookmarked-xrefs-flag)
                 Info-fontify-bookmarked-xrefs-flag)
            Info-fontify-emphasis-flag
            Info-fontify-glossary-words
            Info-fontify-isolated-quote-flag
            Info-fontify-quotations
            Info-fontify-reference-items-flag
            Info-fontify-visited-nodes)
        (dolist (opt  opts) (set opt nil))
      (dolist (opt  opts) (set opt t))))
  (Info-refontify-current-node)
  (when msgp (message "Info+ fontify options are now ALL %s" (if Info-fontify-angle-bracketed-flag 'ON 'OFF))))

;;;###autoload (autoload 'Info-toggle-fontify-angle-bracketed "info+")
(defun Info-toggle-fontify-angle-bracketed (&optional msgp)
  "Toggle option `Info-fontify-angle-bracketed-flag'."
  (interactive "p")
  (setq Info-fontify-angle-bracketed-flag  (not Info-fontify-angle-bracketed-flag))
  (Info-refontify-current-node)
  (when msgp (message "`Info-fontify-angle-bracketed-flag' is now %s"
                      (if Info-fontify-angle-bracketed-flag 'ON 'OFF))))

;;;###autoload (autoload 'Info-toggle-fontify-emphasis "info+")
(defun Info-toggle-fontify-emphasis (&optional msgp)
  "Toggle option `Info-fontify-emphasis-flag'."
  (interactive "p")
  (unless info-fontify-emphasis (error "`info-fontify-emphasis' must be non-nil to use this command"))
  (setq Info-fontify-emphasis-flag  (not Info-fontify-emphasis-flag))
  (Info-refontify-current-node)
  (when msgp (message "`Info-fontify-emphasis-flag' is now %s" (if Info-fontify-emphasis-flag 'ON 'OFF))))

(defvar info-last-non-nil-fontify-glossary-words 'face-till-visit-+-mouseover-def
  "Last non-nil value of `Info-fontify-glossary-words'.")

(defun info--msg-Info-fontify-glossary-words-now ()
  "Echo current value of `Info-fontify-glossary-words'."
  (message "`Info-fontify-glossary-words': %s"
           (case Info-fontify-glossary-words
             ((nil)                               "OFF - no glossary links")
             (face-till-visit-+-mouseover-def     "Show link until visited.  Show definition on mouseover")
             (face-till-visit-+-NO-mouseover-def  "Show link until visited.  NO definition on mouseover")
             (face-+-mouseover-def                "Show link always.  Show definition on mouseover")
             (face-+-NO-mouseover-def             "Show link always.  NO definition on mouseover")
             (NO-face-+-mouseover-def             "Hide link.  Show definition on mouseover")
             (NO-face-+-NO-mouseover-def          "Hide link.  NO definition on mouseover)")
             (otherwise                           "Show link until visited.  Show definition on mouseover"))))

;;;###autoload (autoload 'Info-toggle-link-glossary-words "info+")
(defalias 'Info-toggle-link-glossary-words 'Info-toggle-fontify-glossary-words)
;;;###autoload (autoload 'Info-toggle-fontify-glossary-words "info+")
(defun Info-toggle-fontify-glossary-words (&optional msgp)
  "Toggle option `Info-fontify-glossary-words'.
This toggles between nil and the last non-nil setting (or
`face-till-visit-+-mouseover-def' if none)."
  (interactive "p")
  (if Info-fontify-glossary-words
      (setq info-last-non-nil-fontify-glossary-words  Info-fontify-glossary-words
            Info-fontify-glossary-words               nil)
    (setq Info-fontify-glossary-words  info-last-non-nil-fontify-glossary-words))
  (Info-refontify-current-node)
  (when msgp (info--msg-Info-fontify-glossary-words-now)))

;;;###autoload (autoload 'Info-cycle-link-glossary-words "info+")
(defalias 'Info-cycle-link-glossary-words 'Info-cycle-fontify-glossary-words)
;;;###autoload (autoload 'Info-cycle-fontify-glossary-words "info+")
(defun Info-cycle-fontify-glossary-words (&optional msgp)
  "Cycle option `Info-fontify-glossary-words' through its possible values."
  (interactive "p")
  (when Info-fontify-glossary-words (setq info-last-non-nil-fontify-glossary-words  Info-fontify-glossary-words))
  (setq Info-fontify-glossary-words  (case Info-fontify-glossary-words
                                       ((nil)                               'face-till-visit-+-mouseover-def)
                                       (face-till-visit-+-mouseover-def     'face-till-visit-+-NO-mouseover-def)
                                       (face-till-visit-+-NO-mouseover-def  'face-+-mouseover-def)
                                       (face-+-mouseover-def                'face-+-NO-mouseover-def)
                                       (face-+-NO-mouseover-def             'NO-face-+-mouseover-def)
                                       (NO-face-+-mouseover-def             'NO-face-+-NO-mouseover-def)
                                       (NO-face-+-NO-mouseover-def          nil)))
  (Info-refontify-current-node)
  (when msgp (info--msg-Info-fontify-glossary-words-now)))

;;;###autoload (autoload 'Info-toggle-fontify-local-isolated-backquote "info+")
(defun Info-toggle-fontify-local-isolated-backquote (&optional readp msgp)
  "Toggle `info-isolated-backquote-regexp' for the current Info manual.
With a prefix arg, read a new value for it instead.

When reading a new value, You can use `$-' as the regexp, to NOT do
any such local highlighting, that is, to override any such global
highlighting.  (`$-' is a regexp that cannot match anything.)"
  (interactive "P\np")
  (Info-toggle-fontify-local 'info-isolated-backquote-regexp readp msgp))

;; @@@ THERE IS NO `Info-toggle-fontify-isolated-backquote'!  Do we need one?

;;;###autoload (autoload 'Info-toggle-fontify-isolated-quote "info+")
(defun Info-toggle-fontify-isolated-quote (&optional msgp)
  "Toggle option `Info-fontify-isolated-quote-flag'."
  (interactive "p")
  (setq Info-fontify-isolated-quote-flag  (not Info-fontify-isolated-quote-flag))
  (Info-refontify-current-node)
  (when msgp (message "`Info-fontify-isolated-quote-flag' is now %s"
                      (if Info-fontify-isolated-quote-flag 'ON 'OFF))))
(define-obsolete-function-alias 'Info-toggle-fontify-single-quote 'Info-toggle-fontify-isolated-quote
  "2020-10-22")

;;;###autoload (autoload 'Info-toggle-fontify-local-isolated-quote "info+")
(defun Info-toggle-fontify-local-isolated-quote (&optional readp msgp)
  "Toggle `info-isolated-quote-regexp' for the current Info manual.
With a prefix arg, read a new value for it instead.

When reading a new value, You can use `$-' as the regexp, to NOT do
any such local highlighting, that is, to override any such global
highlighting.  (`$-' is a regexp that cannot match anything.)"
  (interactive "P\np")
  (Info-toggle-fontify-local 'info-isolated-quote-regexp readp msgp))

;;;###autoload (autoload 'Info-toggle-fontify-local-quotation "info+")
(defun Info-toggle-fontify-local-quotation (&optional readp msgp)
  "Toggle `info-quotation-regexp' for the current Info manual.
With a prefix arg, read a new value for it instead.

When reading a new value, You can use `$-' as the regexp, to NOT do
any such local highlighting, that is, to override any such global
highlighting.  (`$-' is a regexp that cannot match anything.)"
  (interactive "P\np")
  (Info-toggle-fontify-local 'info-quotation-regexp readp msgp))

;;;###autoload (autoload 'Info-toggle-fontify-local-quotation-same-line "info+")
(defun Info-toggle-fontify-local-quotation-same-line (&optional readp msgp)
  "Toggle `info-quotation-same-line-regexp' for the current Info manual.
With a prefix arg, read a new value for it instead.

When reading a new value, You can use `$-' as the regexp, to NOT do
any such local highlighting, that is, to override any such global
highlighting.  (`$-' is a regexp that cannot match anything.)"
  (interactive "P\np")
  (Info-toggle-fontify-local 'info-quotation-same-line-regexp readp msgp))

;;;###autoload (autoload 'Info-toggle-fontify-local-angle-bracketed "info+")
(defun Info-toggle-fontify-local-angle-bracketed (&optional readp msgp)
  "Toggle `info-quoted+<>-regexp' for the current Info manual.
With a prefix arg, read a new value for it instead.

When reading a new value, You can use `$-' as the regexp, to NOT do
any such local highlighting, that is, to override any such global
highlighting.  (`$-' is a regexp that cannot match anything.)"
  (interactive "P\np")
  (Info-toggle-fontify-local 'info-quoted+<>-regexp readp msgp))

;;;###autoload (autoload 'Info-toggle-fontify-local-angle-bracketed-same-line "info+")
(defun Info-toggle-fontify-local-angle-bracketed-same-line (&optional readp msgp)
  "Toggle `info-quoted+<>-same-line-regexp' for the current Info manual.
With a prefix arg, read a new value for it instead.

When reading a new value, You can use `$-' as the regexp, to NOT do
any such local highlighting, that is, to override any such global
highlighting.  (`$-' is a regexp that cannot match anything.)"
  (interactive "P\np")
  (Info-toggle-fontify-local 'info-quoted+<>-same-line-regexp readp msgp))

;;;###autoload (autoload 'Info-toggle-fontify-local-emphasis "info+")
(defun Info-toggle-fontify-local-emphasis (&optional readp msgp)
  "Toggle `Info-emphasis-regexp' for the current Info manual.
With a prefix arg, read a new value for it instead.

NOTE: The regexp must match the surrounding underscore chars (`_'),
      and it must have at least one regexp group.  Regexp group #1
      gets highlighted with face `info-emphasis'.  The enclosing
      underscore chars are made invisible.

When reading a new value, You can use `$-' as the regexp, to NOT do
any such local highlighting, that is, to override any such global
highlighting.  (`$-' is a regexp that cannot match anything.)"
  (interactive "P\np")
  (Info-toggle-fontify-local 'Info-emphasis-regexp readp msgp))

;;;###autoload (autoload 'Info-toggle-fontify-reference-items "info+")
(defun Info-toggle-fontify-reference-items (&optional msgp)
  "Toggle option `Info-fontify-reference-items-flag'."
  (interactive "p")
  (setq Info-fontify-reference-items-flag  (not Info-fontify-reference-items-flag))
  (Info-refontify-current-node)
  (when msgp (message "`Info-fontify-reference-items-flag' is now %s"
                      (if Info-fontify-reference-items-flag 'ON 'OFF))))

;;;###autoload (autoload 'Info-toggle-fontify-extra "info+")
(defun Info-toggle-fontify-extra (&optional msgp)
  "Toggle option `Info-fontify-extra-function'."
  (interactive "p")
  (when Info-fontify-extra-function (setq info-last-non-nil-fontify-extra-function  Info-fontify-extra-function))
  (unless info-last-non-nil-fontify-extra-function
    (error "Non-nil option `Info-fontify-extra-function' has never been defined"))
  (setq Info-fontify-extra-function  (and (not Info-fontify-extra-function)
                                          info-last-non-nil-fontify-extra-function))
  (Info-refontify-current-node)
  (when msgp (message "`Info-fontify-extra-function' is now %s"
                      (if Info-fontify-extra-function Info-fontify-extra-function 'OFF))))

;;;###autoload (autoload 'Info-cycle-fontify-quotations "info+")
;;;###autoload (autoload 'Info-toggle-fontify-visited-nodes "info+")
(defun Info-toggle-fontify-visited-nodes (&optional msgp)
  "Toggle option `Info-fontify-visited-nodes'."
  (interactive "p")
  (setq Info-fontify-visited-nodes  (not Info-fontify-visited-nodes))
  (Info-refontify-current-node)
  (when msgp (message "`Info-fontify-visited-nodes' is now %s" (if Info-fontify-visited-nodes 'ON 'OFF))))

(defun Info-cycle-fontify-quotations (&optional msgp)
  "Cycle option `Info-fontify-quotations'.
The three states are off (nil), multiline (symbol `multiline'), and
same line (other non-nil value)."
  (interactive "p")
  (setq Info-fontify-quotations  (case Info-fontify-quotations
                                   ((nil)  t)
                                   ((t)    'multiline)
                                   (t      nil)))
  (Info-refontify-current-node)
  (when msgp (message "`Info-fontify-quotations' is now %s" (case Info-fontify-quotations
                                                              ((nil)      'OFF)
                                                              (multiline  "on (MULTILINE too)")
                                                              (t          "on (SAME LINE only)")))))

;;;###autoload (autoload 'Info-save-current-node "info+")
(defun Info-save-current-node (&optional msgp)
  "Save name of current Info node to list `Info-saved-nodes'."
  (interactive "p")
  (unless (derived-mode-p 'Info-mode) (info-user-error "You must be in Info to use this command"))
  (unless Info-current-node          (info-user-error "No current Info node"))
  (unless Info-current-file          (info-user-error "No Info file"))
  (add-to-list 'Info-saved-nodes (concat "(" (file-name-nondirectory Info-current-file) ")"
                                         Info-current-node))
  (when msgp (message (format "Node `%s' saved" Info-current-node))))

(when (require 'bookmark+ nil t)

  (defun Info-describe-bookmark (&optional node show-definition-p)
    "Describe bookmark for NODE (default: bookmarked node named at point).
With a prefix argument, show the internal definition of the bookmark.

If there is no bookmarked node named at point then you are prompted
for the name of one.

When called from Lisp, NODE is a full node name: `(MANUAL) NODE'.
That is, it corresponds to a default Info bookmark name."
    (interactive
     (list (or (Info-bookmark-name-at-point)  (Info-read-bookmarked-node-name))
           current-prefix-arg))
    (let* ((alist  (bmkp-info-alist-only))
           (bmk    (or (bmkp-get-bookmark-in-alist node t alist)
                       (bmkp-read-bookmark-for-type "Info" alist nil nil 'bmkp-info-history "Describe "))))
      (bmkp-describe-bookmark bmk show-definition-p)))

  (defun Info-read-bookmarked-node-name (&optional localp)
    "Read and return the name of a bookmarked Info node.
A bookmarked node name has the form \"(MANUAL) NODE\", referring to
NODE in MANUAL.
Optional arg LOCALP means read a node name from the current manual."
    (let* ((completion-ignore-case  t)
           (bmks                    (bmkp-remove-if-not
                                     (lambda (bmk) (string-match-p
                                               (if (and localp  Info-current-file)
                                                   (format "\\`(%s) "
                                                           (file-name-sans-extension
                                                            (file-name-nondirectory Info-current-file)))
                                                 "(\\([^)]+\\)) \\([^)]*\\)")
                                               (bmkp-bookmark-name-from-record bmk)))
                                     (bmkp-info-alist-only)))
           (bmk                     (completing-read "Bookmarked node: " bmks nil t nil 'bmkp-info-history)))
      (while (equal bmk "") (setq bmk  (completing-read "Bookmarked node: " bmks nil t nil 'bmkp-info-history)))
      bmk))

  )


;;; Support for TOC with Outline support ---------------------


;; REPLACE ORIGINAL in `outline.el':
;;
;; Changed from `defsubst' to `defun'.  See Emacs bug #28080.
;;
(defun outline-invisible-p (&optional pos)
  "Non-nil if the character after point has been made invisible by Outline."
  (eq (get-char-property (or pos (point)) 'invisible) 'outline))


(when (boundp 'Info-virtual-nodes)      ; Emacs 23.4+

  (add-to-list 'Info-virtual-nodes '("\\`\\*TOC Outline\\* (.*)\\'" (find-node . Info-toc-outline-find-node)))

  (defun Info-toc-outline (&optional arg)
    "Go to a node with a table of contents (TOC) in `outline-minor-mode'.
The TOC is created from the tree structure of Info menus.

In this node you can use the commands and menu items of
`outline-minor-mode' to navigate, hide/show, delete, or rearrange
parts of the TOC.  Start with menu-bar menu `Outline' to see what is
possible.

* With no prefix arg:

  - If buffer `*TOC Outline* (MANUAL)' exists, where MANUAL is the
    manual/file name for the current manual, then pop to it.
  - Else create that buffer for the current manual, and pop to it.

* With a plain prefix arg (`C-u'):

  Pop to a new TOC buffer, `*TOC Outline* (MANUAL)' (or `*TOC
  Outline*<N> (MANUAL)', N=1,2,3...), for the current MANUAL.

* With any other prefix arg (e.g. `M--'):

  Reuse the current Info buffer, going to a node `*TOC Outline*'
  for the current manual."
    (interactive (list (if (consp current-prefix-arg) 'clone current-prefix-arg)))
    (unless (derived-mode-p 'Info-mode)   (info-user-error "You must be in Info to use this command"))
    (unless Info-current-node             (info-user-error "No current Info node"))
    (unless Info-current-file             (info-user-error "No Info file"))
    (when (equal Info-current-file "dir") (info-user-error "No TOC for Info Directory - choose a manual"))
    (if (and (not arg)  (get-buffer (format "*TOC Outline* (%s)" (file-name-nondirectory Info-current-file))))
        (pop-to-buffer (format "*TOC Outline* (%s)" (file-name-nondirectory Info-current-file)))
      (when (or (not arg)  (eq arg 'clone))
        (clone-buffer (format "*TOC Outline* (%s)" (file-name-nondirectory Info-current-file)) t))
      (Info-breadcrumbs-in-mode-line-mode -1)
      (setq mark-ring  ()) ;`clone-buffer' causes this to be needed, if `*info*' buffer has no mark.
      (Info-find-node Info-current-file (format "*TOC Outline* (%s)" (file-name-nondirectory Info-current-file)))
      (let ((prev-node  (nth 1 (car Info-history)))
            prev-posn)
        (goto-char (point-min))
        (when (setq prev-posn  (search-forward (concat "*Note " prev-node ":") nil t))
          (setq prev-posn  (- prev-posn (length prev-node) 2)))
        (goto-char (or prev-posn  (point-min))))))

  (defun Info-toc-outline-find-node (filename nodename &optional _no-going-back)
    "TOC-specific implementation of `Info-find-node-2'."
    (let* ((curr-file  (substring-no-properties (or filename Info-current-file)))
           (curr-node  (substring-no-properties (or nodename Info-current-node)))
           (node-list  (Info-toc-nodes curr-file)))
      (insert (format "\n\^_\nFile: %s,  Node: %s,  Up: Top\n\n" curr-file curr-node))
      (let ((title  (format "Contents (%s)" (file-name-nondirectory curr-file))))
        (insert title "\n" (make-string (length title) ?*) "\n\n"))
      (insert "*Note Top::\n")
      (Info-toc-insert (nth 3 (assoc "Top" node-list)) node-list 0 curr-file) ; `Top' nodes
      (unless (bobp)
        (let ((Info-hide-note-references   'hide)
              (Info-fontify-visited-nodes  nil))
          (setq Info-current-file  filename
                Info-current-node  (format "*TOC Outline* (%s)" (file-name-nondirectory curr-file)))
          (goto-char (point-min))
          (narrow-to-region (or (re-search-forward "\n[\^_\f]\n" nil t)  (point-min)) (point-max))
          (Info-fontify-node)
          (widen))))
    (set (make-local-variable 'inhibit-read-only) t)
    (goto-char (point-min))
    (search-forward "Contents" nil t)
    (forward-line 3)
    ;; If `Info-toc-outline-no-redundancy-flag' is non-nil then remove redundancies.
    ;; Else give non-links face `info-title-4'.
    (save-excursion
      (let ((note-re  "^[\t]*[*]Note "))
        (while (not (eobp))
          (if (re-search-forward note-re (line-end-position) t)
              (if (and Info-toc-outline-no-redundancy-flag ; Remove node line if already listed.
                       (let ((node  (buffer-substring-no-properties (point) (line-end-position))))
                         (save-excursion (beginning-of-line) (re-search-backward (concat note-re node) nil t))))
                  (delete-region (line-beginning-position) (line-beginning-position 2))
                (save-excursion ; Indent previous line to same column, if it was a heading.
                  (let ((col  (current-column)))
                    (forward-line -1)
                    (unless (re-search-forward note-re (line-end-position) t)
                      (insert (propertize " " 'display `(space :align-to ,col))))))
                (forward-line 1))
            (if Info-toc-outline-no-redundancy-flag
                (delete-region (line-beginning-position) (line-beginning-position 2))
              ;; The line is a section heading.  Put face `info-title-4' on it.
              (put-text-property (line-beginning-position 1) (line-end-position 1) 'face 'info-title-4)
              (unless (looking-at "\\s-") (insert "\n"))
              (forward-line 1))))))
    (outline-minor-mode 1)
    (define-key Info-toc-outline-map [remap outline-promote] 'Info-outline-promote)
    (define-key Info-toc-outline-map [remap outline-demote]  'Info-outline-demote)
    (define-key Info-toc-outline-map "\C-x\M-l" 'Info-toc-outline-refontify-region)
    (use-local-map Info-toc-outline-map)
    (setq outline-regexp  "[\t]*[*]Note ") ; Include no "^" here.
    (set (make-local-variable 'Info-hide-note-references) 'hide)
    (add-hook 'post-command-hook 'Info-toc-outline-refontify-region nil 'LOCAL)
    (buffer-enable-undo))

  (defun Info-toc-outline-refontify-region (&optional start end forcep)
    "In Info `*TOC Outline*' buffer, refontify region.
Interactively, if region is not active or is empty, refontify buffer.
From Lisp:
 * Do nothing if not in a `*TOC Outline* buffer or if buffer has not
   been modified.
 * START defaults to `point-min', END defaults to `point-max'."
    (interactive (let* ((regionp  (use-region-p))
                        (st       (if regionp (region-beginning) (point-min)))
                        (en       (if regionp (region-end) (point-max))))
                   (list st en t)))
    (setq start  (or start  (point-min))
          end    (or end    (point-max)))
    (let ((buff  (buffer-name)))
      (when (or forcep  (and buff
                             (buffer-modified-p)
                             (string-match-p "\\`\\*TOC Outline\\* ([^)]+)" buff)
                             (derived-mode-p 'Info-mode)))
        (Info-toc-outline-refontify-links start end))))

  (defun Info-outline-promote (&optional which)
    "Promote headings higher up the tree.
If `transient-mark-mode' is on and the mark is active, promote
headings in the region (from a Lisp program, pass the symbol `region'
for WHICH).
Otherwise:
 * With no prefix arg, promote the current heading and all headings in
   the subtree (from a Lisp program, pass symbol `subtree' for WHICH);
 * with a prefix arg, promote just the current heading (from a Lisp
   program, pass nil for WHICH, or do not pass any argument).

This is a version of `outline-promote' for use with Info.  It
refontifies the buffer to hide the link prefix `*Note'."
    (interactive (list (if (and transient-mark-mode  mark-active)
                           'region
                         (outline-back-to-heading)
                         (and (not current-prefix-arg)  'subtree))))
    (let (start end)
      (cond ((eq which 'region)
             (outline-map-region 'Info-outline-promote
                                 (setq start  (copy-marker (region-beginning)))
                                 (prog1 (region-end)
                                   (setq end  (save-excursion
                                                (goto-char (region-end))
                                                (copy-marker (line-end-position)))))))
            (which
             (outline-map-region 'Info-outline-promote
                                 (setq start  (copy-marker (point)))
                                 (save-excursion (outline-get-next-sibling) (setq end  (copy-marker (point))))))
            (t
             (outline-back-to-heading t)
             (setq start  (copy-marker (point))
                   end    (copy-marker (line-end-position)))
             (let* ((head     (match-string-no-properties 0))
                    (level    (save-match-data (funcall outline-level)))
                    (up-head  (or (outline-head-from-level (1- level) head)
                                  (save-excursion ; Use the parent heading, if it is really one level less.
                                    (save-match-data
                                      (outline-up-heading 1 t)
                                      (and (= (1- level)  (funcall outline-level))
                                           (match-string-no-properties 0))))
                                  (error "Cannot promote - already at highest level"))))
               (unless (rassoc level outline-heading-alist) (push (cons head level) outline-heading-alist))
               (replace-match up-head nil t))))
      (Info-toc-outline-refontify-links start end)))

  (defun Info-outline-demote (&optional which)
    "Demote headings lower down the tree.
If `transient-mark-mode' is on and the mark isactive, demote headings
in the region (from a Lisp program, pass symbol `region' for WHICH).
Otherwise:
 * Without a prefix arg, demote current heading and all headings in the
 subtree (from a Lisp program, pass symbol `subtree' for WHICH).
 * With a prefix arg, demote just the current heading (from a Lisp
 program, pass nil for WHICH, or do not pass any argument).

This is a version of `outline-demote' for use with Info.  It
refontifies the buffer to hide link prefix `*Note'."
    (interactive (list (if (and transient-mark-mode  mark-active)
                           'region
                         (outline-back-to-heading)
                         (and (not current-prefix-arg)  'subtree))))
    (let (start end)
      (cond ((eq which 'region)
             (outline-map-region 'Info-outline-demote
                                 (setq start  (copy-marker (region-beginning)))
                                 (prog1 (region-end)
                                   (setq end  (save-excursion
                                                (goto-char (region-end))
                                                (copy-marker (line-end-position)))))))
            (which
             (outline-map-region 'Info-outline-demote
                                 (setq start  (copy-marker (point)))
                                 (save-excursion (outline-get-next-sibling) (setq end  (copy-marker (point))))))
            (t
             (setq start  (copy-marker (point))
                   end    (copy-marker (line-end-position)))
             (let* ((head       (match-string-no-properties 0))
                    (level      (save-match-data (funcall outline-level)))
                    (down-head  (or (outline-head-from-level (1+ level) head)
                                    (save-excursion
                                      (save-match-data
                                        (while (and (progn (outline-next-heading) (not (eobp)))
                                                    (<= (funcall outline-level) level)))
                                        (when (eobp) ; Try again from beginning of buffer.
                                          (goto-char (point-min))
                                          (while (and (progn (outline-next-heading) (not (eobp)))
                                                      (<= (funcall outline-level) level))))
                                        (unless (eobp)
                                          (looking-at outline-regexp)
                                          (match-string-no-properties 0))))
                                    (error "Cannot demote - already at lowest level"))))
               (unless (rassoc level outline-heading-alist) (push (cons head level) outline-heading-alist))
               (replace-match down-head nil t))))
      (Info-toc-outline-refontify-links start end)))

  (defun Info-toc-outline-refontify-links (begin end)
    "Refontify TOC cross references between buffer positions BEGIN and END."
    ;; (interactive "r") ; Not really intended as a command, but might be handy sometimes (?).
    (save-excursion
      (let* ((inhibit-read-only     t)
             (case-fold-search      t)
             (fontify-bookmarked-p  (and (boundp 'Info-fontify-bookmarked-xrefs-flag)
                                         Info-fontify-bookmarked-xrefs-flag))
             (node-not-too-large    (and (or fontify-bookmarked-p  Info-fontify-visited-nodes)
                                         Info-fontify-maximum-menu-size
                                         (or (eq t Info-fontify-maximum-menu-size)
                                             (< (- (point-max) (point-min)) Info-fontify-maximum-menu-size))))
             (fontify-bookmarked-p  (and node-not-too-large  fontify-bookmarked-p))
             (fontify-visited-p     (and node-not-too-large  Info-fontify-visited-nodes))
             paragraph-markers rbeg rend)
        (goto-char begin)
        (while (re-search-forward "\\(\\*Note[ \n\t]+\\)\\([^:]*\\)\\(:[ \t]*\\([^.,:(]*\\)\\(\\(([^)]\
*)\\)[^.,:]*\\)?[,:]?\n?\\)" end t)
          (let ((start  (match-beginning 0))
                (next   (point))
                other-tag)
            (when Info-hide-note-references
              (when (not (eq Info-hide-note-references 'hide)) ; *Note is often used where *note should have been.
                (goto-char start)
                (skip-syntax-backward " ")
                (when (memq (char-before) '(?\( ?\[ ?\{))
                  (skip-syntax-backward " (")) ; Check whether the paren is preceded by an end of sentence.
                (setq other-tag  (cond ((save-match-data (looking-back "\\<see" nil)) "")
                                       ((save-match-data (looking-back "\\<in" nil)) "")
                                       ((memq (char-before) '(nil ?\. ?! ??)) "See ")
                                       ((save-match-data (save-excursion (search-forward "\n\n" start t))) "See ")
                                       (t "see "))))
              (goto-char next)
              (add-text-properties
               (match-beginning 1)
               (or (save-match-data (let ((start1  (match-beginning 1))) ; Don't hide \n after *Note
                                      (and (string-match "\n" (match-string 1))  (+ start1 (match-beginning 0)))))
                   (match-end 1))
               (if other-tag
                   `(display ,other-tag front-sticky nil rear-nonsticky t)
                 '(invisible t front-sticky nil rear-nonsticky t))))
            (add-text-properties
             (match-beginning 2) (match-end 2)
             (list 'help-echo (if (or (match-end 5)  (not (equal (match-string 4) "")))
                                  (concat "mouse-2: go to " (or (match-string 5)  (match-string 4)))
                                "mouse-2: go to this node")
                   'mouse-face 'highlight))
            (setq rbeg  (match-beginning 2)
                  rend  (match-end 2))
            (let (node)
              (put-text-property
               rbeg
               rend
               'font-lock-face
               (if (and (or Info-fontify-visited-nodes  fontify-bookmarked-p)
                        (save-match-data
                          (setq node  (replace-regexp-in-string
                                       "^[ \t]+" ""
                                       (replace-regexp-in-string
                                        "[ \t\n]+" " "
                                        (or (match-string-no-properties 5)
                                            (and (not (equal (match-string 4) ""))
                                                 (match-string-no-properties 4))
                                            (match-string-no-properties 2)))))
                          (let* ((hl               Info-history-list)
                                 (external-link-p  (string-match "(\\([^)]+\\))\\([^)]*\\)" node))
                                 (file             (if external-link-p
                                                       (file-name-nondirectory
                                                        (match-string-no-properties 1 node))
                                                     Info-current-file))
                                 res)
                            (when external-link-p
                              (setq node  (if (equal (match-string 2 node) "")
                                              "Top"
                                            (match-string-no-properties 2 node))))
                            (or (and fontify-bookmarked-p  (Info-bookmark-name-for-node node))
                                (progn
                                  (while hl
                                    (if (and (string-equal node (nth 1 (car hl)))
                                             (equal file (if (and external-link-p  (stringp (caar hl)))
                                                             (file-name-nondirectory (caar hl))
                                                           (caar hl))))
                                        (setq res  (car hl)
                                              hl   nil)
                                      (setq hl  (cdr hl))))
                                  res)))))
                   (let ((bmk  (and fontify-bookmarked-p  (Info-bookmark-for-node node 'LOCALP))))
                     (if bmk
                         (or (bmkp-get-tag-value bmk "bmkp-info-face")  'info-xref-bookmarked)
                       'info-xref-visited))
                 'info-xref)))
            (save-excursion ; For multiline ref, unfontify newline and surrounding whitespace
              (goto-char rbeg)
              (save-match-data (while (re-search-forward "\\s-*\n\\s-*" rend t nil)
                                 (remove-text-properties (match-beginning 0) (match-end 0) '(font-lock-face t)))))
            (when (memq Info-hide-note-references '(t hide))
              (add-text-properties (match-beginning 3) (match-end 3)
                                   '(invisible t front-sticky nil rear-nonsticky t))
              ;; Unhide the file name of the external reference in parens
              (when (and (match-string 6)  (not (eq Info-hide-note-references 'hide)))
                (remove-text-properties (match-beginning 6) (match-end 6)
                                        '(invisible t front-sticky nil rear-nonsticky t)))
              (save-match-data ; Unhide newline because hidden newlines cause too long lines
                (let ((beg3  (match-beginning 3))
                      (end3  (match-end 3)))
                  (when (and (string-match "\n[ \t]*" (match-string 3))
                             (not (save-match-data (save-excursion (goto-char (1+ end3)) (looking-at "[.)]*$")))))
                    (remove-text-properties (+ beg3 (match-beginning 0)) (+ beg3 (match-end 0))
                                            '(invisible t front-sticky nil rear-nonsticky t))))))
            (when (and Info-refill-paragraphs  Info-hide-note-references)
              (push (set-marker (make-marker) start) paragraph-markers)))))
      (goto-char (point-max))
      (skip-chars-backward "\n") ; Hide any empty lines at the end of the node.
      (when (< (1+ (point)) (point-max)) (put-text-property (1+ (point)) (point-max) 'invisible t))
      (set-buffer-modified-p nil)))

  )




;; Note: This is not super-clean code (it's kind of a hack job).
;;;###autoload (autoload 'Info-merge-subnodes "info+")
(defun Info-merge-subnodes (&optional recursive-display-p recursive-call-p)
  "Integrate current node with nodes referred to in its Menu.

Displays the current Info node, together with the nodes in its Menu.
Buffer `*Info: NODE*' is used for the display, where NODE is the name
of the current node.  The contents of this node's subnodes (the nodes
named in this node's Menu) are included in the buffer, following the
contents of the current node.

Optional arg RECURSIVE-DISPLAY-P (prefix arg if interactive) governs
the way menus of subnodes are treated:

  If nil, nothing additional happens.  Subnode menus are not explored.
  Only the current node and its immediate subnodes are documented, in
  the single display buffer `*Info: NODE*'.

  If non-nil, then the subnodes of a node are treated in the same way
  as the parent node, recursively: If any of them has, itself, a Menu,
  then that menu's subnodes are also explored, and so on.

    If RECURSIVE-DISPLAY-P is zero, then a single display buffer is
    used for all of the nodes explored.  Otherwise, a separate display
    buffer is used for each subnode that has a Menu (see next).

      Use this when you want a single, flat compilation of the current
      node and all of its subnodes.  It is less appropriate when the
      current node has several levels of subnodes: The flattened
      result can be difficult to read.

    If RECURSIVE-DISPLAY-P is positive, then the contents of each
    subnode are displayed twice: once in the parent node's display,
    and once in the subnode's own display.

      Use this when the current node has several levels of subnodes
      and you want each display buffer to be self-contained.

    If RECURSIVE-DISPLAY-P is negative, then there is no redundancy: A
    subnode's contents are only displayed in its parent's buffer.  The
    subnode's own display buffer only contains the contents of its own
    subnodes.

      Use this when the current node has several levels of subnodes
      and you want no redundancy between the display buffers.

The user option (variable) `Info-subtree-separator' is a string to be
inserted by `Info-merge-subnodes' just before the title of each
node (preceding its description).  By default it is \"\\n* \", producing
a node title resembling a menu item.  Setting this to \"\\f\\n* \" will
cause a page break before each node description.  For more on setting
this variable, type \\<Info-mode-map>`\\[describe-variable] Info-subtree-separator'.

------

Optional second arg RECURSIVE-CALL-P is only for internal use.  It is
used to indicate whether (non-nil) or not (nil) this is a recursive
\(i.e. not a top-level) call to `Info-merge-subnodes'.  Non-nil
means that this is a subnode, and that its contents should only be
included in the present display if RECURSIVE-DISPLAY-P is also
non-nil.  For proper operation when RECURSIVE-DISPLAY-P is zero, the
non-nil value of RECURSIVE-CALL-P should be the node name of the
top-level call to `Info-merge-subnodes'."
  (interactive "P")
  (when (interactive-p)
    (unless (y-or-n-p "Do you really want to integrate this node with its \
subnodes (outside Info)? ")
      (info-user-error (substitute-command-keys
                        "OK.  If you are not sure what this command is about, type \
`\\[describe-function] Info-merge-subnodes'.")))) ; Defined in `help.el'.
  (garbage-collect)
  (setq recursive-display-p  (and recursive-display-p  (prefix-numeric-value recursive-display-p)))
  (let* ((buf                        (current-buffer)) ; Info buffer
         (single-buf-p               (and recursive-display-p  (zerop recursive-display-p)))
         (infop-node-name            (or (and single-buf-p  recursive-call-p)  Info-current-node))
         (rep-buf                    (get-buffer-create (concat "*Info: " infop-node-name "*"))) ; Merge buffer.
         (more                       t)
         (inhibit-field-text-motion  t) ; Just to be sure, for `end-of-line'.
         token oldpt strg menu-item-line ind)

    (when (interactive-p)
      (message "Processing node `%s' and %ssubnodes..." infop-node-name
               (if recursive-display-p "all of its " "its immediate ")))
    (save-window-excursion
      (goto-char (point-min))
      (forward-line 1)
      (setq strg  (buffer-substring (point) (point-max))) ; Node contents.
      (goto-char (point-min))
      (setq more  (search-forward "* menu" nil t))
      (forward-line 1)

      ;; Merge buffer: Insert buffer header and main node's contents, if not recursive or
      ;;                                                              do want redundancy.
      ;;               Then insert each subnode (unless this is an Index).
      (switch-to-buffer-other-window rep-buf)
      (unless (and recursive-call-p  single-buf-p)
        (erase-buffer)
        (funcall Info-display-node-header-fn) ; Insert header.
        (insert (concat "\n\n" (and (or (not recursive-call-p) ; Top-level call.
                                        (and recursive-display-p ; Redundancy desired.
                                             (> recursive-display-p 0)))
                                    strg)))) ; Insert main node's contents.

      (unless  (string-match "\\s-*Index$" infop-node-name) ; Don't recurse down Index menus.

        ;; Insert menu items and possibly their subnodes.
        (save-excursion
          (while more

            ;; Info buffer: Get menu item token.
            (set-buffer buf)
            (end-of-line)
            (setq oldpt  (point)
                  more   (search-forward "\n* " nil t)) ; Possible next menu item.
            (unless more (goto-char (point-max)))
            (while (and (not (eobp))    ; Search for a real menu item.
                        (not (setq token  (Info-get-token ; File menu item.
                                           (point) "\\* " "\\* \\([^:]*\\)::")))
                        (not (setq token  (Info-get-token ; Nonfile menu item.
                                           (point) "\\* "
                                           "\\* [^:]*:[ \t]+\\([^\t,.\n]+\\)[\t,.\n]"))))
              (setq more  (search-forward "\n* " nil t)))
            (unless token (setq more  nil)) ; No menu item. Done.

            ;; Treat subnode (menu item).
            (when more

              ;; Merge buffer: Insert separator line.
              (set-buffer rep-buf)
              (goto-char (point-max))
              (insert Info-subtree-separator) ; Ready for next menu item.

              ;; Info buffer: Go to subnode.
              (set-buffer buf)
              (Info-goto-node token)
              (goto-char (point-min))
              (forward-line 1)
              (setq strg  (buffer-substring (point) (point-max))) ; Pick up subnode contents.

              ;; Go back to parent node and get menu-item line.
              (Info-history-back)
              (let ((inhibit-read-only  t)) ; Get untabified menu-item line, so can count
                (buffer-enable-undo) (undo-start) ; chars to underline.
                (untabify (point) (save-excursion (forward-line 1) (point)))
                (setq menu-item-line  (buffer-substring-no-properties
                                       (save-excursion (beginning-of-line) (forward-char 2) (point))
                                       (save-excursion (forward-line 1) (point))))
                (when pending-undo-list (undo-more 1)) ; Only if did something.
                (buffer-disable-undo))
              ;; Merge buffer: Insert menu-item line, underline it, and insert subnode contents.
              (set-buffer rep-buf)
              (insert menu-item-line)
              (setq ind  (1+ (length menu-item-line)))
              (while (> ind 0) (insert "=") (setq ind  (1- ind))) ; Underline menu item.
              (insert "\n")
              (put-text-property (save-excursion (forward-line -2) (point))
                                 (save-excursion (forward-line 1) (point))
                                 'font-lock-face 'info-file)
              (setq oldpt  (point))
              (insert strg)             ; Insert subnode contents.
              (indent-rigidly oldpt (point) 2)
              ;; Recursive call: Insert subnode's subnodes, if there are any.
              ;; Again, though, don't recurse down Index menus.
              (when (and recursive-display-p  (not (string-match "\\s-*Index$" token)))
                ;; Info buffer: Go back to subnode.
                ;; If it has a menu, then treat its subnodes, recursively.
                (with-current-buffer buf
                  (Info-goto-node token)
                  (when (search-forward "* menu" nil t)
                    (forward-line 1) (end-of-line)
                    (when (and (search-forward "\n* " nil t)
                               (or (Info-get-token (point) "\\* " "\\* \\([^:]*\\)::") ; file menu item
                                   (Info-get-token (point) "\\* " ; nonfile menu item
                                                   "\\* [^:]*:[ \t]+\\([^\t,.\n]+\\)[\t,.\n]")))
                      (Info-merge-subnodes recursive-display-p infop-node-name)))
                  (set-buffer buf) ; Info buffer: Go back to parent node.
                  (Info-history-back)))
              (set-buffer buf))))))     ; Info buffer
    ;; Merge buffer
    (switch-to-buffer-other-window rep-buf)
    (when (and (one-window-p t)  (not (window-minibuffer-p)) (fboundp 'fit-frame) ; Defined in `fit-frame.el'.
               Info-fit-frame-flag)
      (fit-frame))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (use-local-map Info-merged-map)
    (when (interactive-p)
      (message "Processing node `%s' and %ssubnodes... done" infop-node-name
               (if recursive-display-p "all of its " "its immediate ")))))

;;;###autoload (autoload 'Info-virtual-book "info+")
(defun Info-virtual-book (book nodes &optional include-bookmark-nodes-p)
  "Open a virtual Info BOOK, with a menu of Info NODES.
BOOK is a string naming the virtual book.
NODES is a list of strings naming Info nodes.
  Each node name is normally absolute, that is, a
  filename-plus-nodename string such as \"(emacs)Basic\".  But if you
  call this command from an Info buffer, then a node name can be
  relative, such as \"Basic\".
Non-nil optional arg INCLUDE-BOOKMARK-NODES-P means that all Info
nodes recorded as bookmarks are included in the virtual book.

Interactively, you are prompted for the name of the virtual book, and
the nodes are those in `Info-saved-nodes'.

Interactively, a prefix argument says to include Info nodes recorded
as bookmarks.

In addition to creating a virtual book by customizing option
`Info-saved-nodes', you can add nodes to that option (and thus to the
virtual book) using \\<Info-mode-map>`\\[Info-save-current-node]' (`Info-save-current-node')."
  (interactive (list (read-from-minibuffer "Virtual book name: " nil nil nil nil "Virtual Book")
                     Info-saved-nodes
                     current-prefix-arg))
  (unless nodes (setq nodes  Info-saved-nodes))
  (when include-bookmark-nodes-p
    (unless (require 'bookmark+ nil t) (require 'bookmark nil t))
    (bookmark-maybe-load-default-file)
    (let ((bm-nodes  ())
          node file)
      (dolist (bm  bookmark-alist)
        (when (setq node  (cdr (assq 'info-node bm)))
          (setq file  (bookmark-get-filename bm))
          (push (concat "(" (file-name-nondirectory (cdr (assq 'filename bm))) ")" node) bm-nodes)))
      (setq nodes  (append nodes bm-nodes))))
  (unless (and nodes  (stringp (car nodes))) ; Minimal sanity check.
    (info-user-error (if (interactive-p) "No saved Info nodes" "No Info nodes")))
  (unless (stringp book) (setq book  "Virtual Book")) ; Non-interactive - NODESET is a list.
  (let ((file  (and (stringp Info-current-file)  (concat "(" (file-name-nondirectory Info-current-file) ")"))))
    (with-current-buffer (get-buffer-create " *info-toc*")
      (let ((inhibit-read-only  t)
            sans-file filep)
        (erase-buffer)
        (goto-char (point-min))
        (insert "\n\^_\nFile: toc,  Node: Top,  Up: (dir)\n\n")
        (insert book "\n" (make-string (length book) ?*) "\n\n")
        (insert "* Menu:\n\n")
        (while nodes
          (if (setq filep  (string-match "^([^)]+)" (car nodes)))
              (setq sans-file  (substring (car nodes) (match-end 0)))
            (setq sans-file  (car nodes)))
          ;; (insert "* " sans-file ": " (car nodes) ".\n")
          (insert "* " sans-file ": "
                  (concat (and (not filep)  (or file  "(**INFO FILE UNKNOWN**)")) (car nodes))
                  ".\n")
          (setq nodes  (cdr nodes))))
      (unless (bobp)
        (let ((Info-hide-note-references   'hide)
              (Info-fontify-visited-nodes  nil))
          (Info-mode)
          (setq Info-current-file  'toc Info-current-node "Top")
          (goto-char (point-min))
          (narrow-to-region (or (re-search-forward "\n[\^_\f]\n" nil t)  (point-min)) (point-max))
          (Info-fontify-node)
          (widen)))))
  (info)
  (Info-find-node 'toc "Top"))

;; Vanilla Emacs added this from Info+ on 2021-11-11, in response to bug #44895.
;;
;;;###autoload (autoload 'Info-goto-node-web "info+")
(defun Info-goto-node-web (node &optional flip-new-win)
  "Use `browse-url' to go to Info node NODE using a Web browser.
With a prefix arg, reverse the effect of option
`browse-url-new-window-flag'.

NODE is the name of a node in the GNU Emacs or Elisp manual.
Alternatively, NODE can have the form (MANUAL)NODE, where MANUAL is
\"emacs\" or \"elisp\" and NODE is the name of the node in that
manual.  Empty NODE in (MANUAL) defaults to the `Top' node."
  (interactive (list (Info-read-node-name "Go to node: " Info-current-node) current-prefix-arg))
  (require 'browse-url)
  (unless Info-current-file (error "This command must be invoked from Info"))
  (browse-url (Info-url-for-node node) (list (if flip-new-win
                                                 (not browse-url-new-window-flag)
                                               browse-url-new-window-flag))))

;; See https://www.gnu.org/software/texinfo/manual/texinfo/html_node/
;;             HTML-Xref-Node-Name-Expansion.html
;;
;; 1. The standard ASCII letters (a-z and A-Z) are not modified. All
;;    other characters may be changed as specified below.
;;
;; 2. The standard ASCII numbers (0-9) are not modified except when a
;;    number is the first character of the node name. In that case, see
;;    below.
;;
;; 3. Multiple consecutive space, tab and newline characters are
;;    transformed into just one space. (It’s not possible to have
;;    newlines in node names with the current implementation, but we
;;    specify it anyway, just in case.)
;;
;; 4. Leading and trailing spaces are removed.
;;
;; 5. After the above has been applied, each remaining space character is
;;    converted into a ‘-’ character.
;;
;; 6. Other ASCII 7-bit characters are transformed into ‘_00xx’, where xx
;;    is the ASCII character code in (lowercase) hexadecimal. This includes
;;    ‘_’, which is mapped to ‘_005f’.
;;
;; 7. If the node name does not begin with a letter, the literal string
;;    ‘g_t’ is prefixed to the result. (Due to the rules above, that
;;    string can never occur otherwise; it is an arbitrary choice,
;;    standing for “GNU Texinfo”.) This is necessary because XHTML
;;    requires that identifiers begin with a letter.
;;
;;
;; Vanilla Emacs added this from Info+ on 2021-11-11, in response to bug #44895.
;;
;;;###autoload (autoload 'Info-url-for-node "info+")
(defun Info-url-for-node (node)
  "Return a URL for NODE, a node in the GNU Emacs or Elisp manual.
Alternatively, NODE can have the form (MANUAL)NODE, where MANUAL is
\"emacs\" or \"elisp\" and NODE is the name of the node in that
manual.  Empty NODE in (MANUAL) defaults to the `Top' node."
  (interactive (list (Info-read-node-name "Node: " Info-current-node)))
  (unless Info-current-file (error "This command must be invoked from Info"))
  (let (file url)
    (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)" node)
    (setq file  (if (= (match-beginning 1) (match-end 1)) "" (match-string 2 node))
          node  (match-string 3 node))
    (when (equal node "") (setq node  "index")) ; `Top' node.
    (let ((trim  (string-match "\\s +\\'" file)))
      (when trim (setq file (substring file 0 trim))))
    (let ((trim  (string-match "\\s +\\'" node)))
      (when trim (setq node (substring node 0 trim))))
    (when (equal file "") (setq file  Info-current-file))
    (setq file  (file-name-sans-extension (file-name-nondirectory file)))
    (unless (member file '("emacs" "elisp"))
      (error "Manual cannot be `%s'; it can only be `emacs' or `elisp'" file))
    (setq node  (mapconcat (lambda (ch)
                             (if (or (< ch 32) ; ^@^A-^Z^[^\^]^^^-
                                     (and (<= 33 ch)   (<= ch 47)) ; !"#$%&'()*+,-./
                                     (and (<= 58 ch)   (<= ch 64)) ; :;<=>?@
                                     (and (<= 91 ch)   (<= ch 96)) ; [\]_`
                                     (and (<= 123 ch)  (<= ch 127))) ; {|}~ DEL
                                 (format "_00%x" ch)
                               (char-to-string ch)))
                           node
                           ""))
    (setq node  (replace-regexp-in-string "[ \t]+" "-" node t t))
    (unless (string-match-p "[[:alpha:]]" node) (setq node  (concat "g_t" node)))
    (setq url  (concat "https://www.gnu.org/software/emacs/manual/html_node/"
                       file "/" node ".html"))
    (message "URL: %s" url)
    url))

;;;###autoload (autoload 'info-manual "info+")
(defun info-manual (arg) ; Bound to `C-h r' globally (replaces `info-emacs-manual').
  "Display a manual in Info mode - by default, the Emacs manual.
With a prefix arg, prompt for the manual name.
With a numeric prefix arg, only currently visited manuals are
candidates."
  (interactive "P")
  (if arg
      (let ((current-prefix-arg  (numberp arg))) (call-interactively #'info-display-manual))
    (info "emacs")))

(global-set-key [remap info-emacs-manual] 'info-manual) ; `C-h r'
    


(easy-menu-define
    Info-merged-menu Info-merged-map
  "Menu for merged `info' buffers."
  '("Info"
    ["Next Link" Info-next-reference t]
    ["Previous Link" Info-prev-reference t]
    ["Search (regexp)" Info-search t]
    ["Quit" quit-window t]))

(easy-menu-define
  Info-mode-menu Info-mode-map
  "Menu for Info files."
  '("Info"
    ("Index"
     ["Find with Index..." Info-index :help "Look for a string in the index"]
     ["Find Again with Index" Info-index-next
      :active Info-index-alternatives :help "Look for string again in index"]
     ["Find In All Indexes..." info-apropos :help "Look for a string in the indexes of all manuals"]
     ["Virtual Index..." Info-virtual-index
      :visible (fboundp 'Info-virtual-index) :help "Open virtual index matching input pattern"])
    ("Search"
     ["Incrementally..." isearch-forward :help "Search incrementally (`C-r': backward)"]
     ["Incrementally...(Regexp)" isearch-forward-regexp
      :help "Search for regular expression incrementally (`C-M-r': backward)"]
     ["Non-incrementally...(Regexp)" Info-search :help "Search nonincrementally for regular expression"]
     ["  Again" Info-search-next :help "Search nonincrementally for another occurrence"]
     ["Non-incrementally Case-Sensitively..." Info-search-case-sensitively
      :help "Search nonincrementally and case sensitively for regular expression"]
     ["  Again" Info-search-case-sensitively-next
      :help "Search nonincrementally and case sensitively for another occurrence"])
    ("History"
     ["Back (History)" Info-history-back
      :active Info-history :help "Go back in history to the last node you were at"]
     ["Forward (History)" Info-history-forward :active Info-history-forward :help "Go forward in history"]
     ["History List" Info-history :active Info-history-list :help "Go to menu of visited nodes"])
    ("Navigation"
     ("Menu Item" ["You should never see this" report-emacs-bug t])
     ("Reference" ["You should never see this" report-emacs-bug t])
     ["Top" Info-directory :help "Go to the list of manuals (Info top level)"]
     ["Up" Info-up :active (Info-check-pointer "up") :help "Go up in the Info tree"]
     ["Next" Info-next :active (Info-check-pointer "next") :help "Go to the next node"]
     ["Previous" Info-prev :active (Info-check-pointer "prev[ious]*") :help "Go to the previous node"]
     ["Go to Node..." Info-goto-node :help "Go to a named node"]
     ["Go to Node on Web..." Info-goto-node-web :help "Go to a named node on the Web (HTML doc)"]
   
     "--"
     ["Forward" Info-forward-node :help "Go forward one node, considering all as a sequence"]
     ["Backward" Info-backward-node :help "Go backward one node, considering all as a sequence"]
     ["First in File" Info-top-node :help "Go to top node of file"]
     ["Last in File" Info-final-node :help "Go to final node in this file"]
     ["Beginning of This Node" beginning-of-buffer :help "Go to beginning of this node"])
    ("Toggle/Cycle"
     ["Highlighting All" Info-toggle-fontify-all
      :help "Toggle all Info+ fontify options"]
     ["Highlighting ‘...’ or `...', and \"...\"" Info-cycle-fontify-quotations
      :help "Cycle option `Info-fontify-quotations'"]
     ["Highlighting <...>" Info-toggle-fontify-angle-bracketed
      :style toggle :selected Info-fontify-angle-bracketed-flag
      :help "Toggle option `Info-fontify-angle-bracketed-flag'"]
     ["Highlighting Isolated ' and `" Info-toggle-fontify-isolated-quote
      :style toggle :selected Info-fontify-isolated-quote-flag
      :help "Toggle option `Info-fontify-isolated-quote-flag'"]
     ["Highlighting Glossary Words" Info-toggle-fontify-glossary-words
      :style toggle :selected Info-fontify-glossary-words
      :help "Toggle option `Info-fontify-glossary-words'"]
     ["Highlighting Reference Items" Info-toggle-fontify-reference-items
      :style toggle :selected Info-fontify-reference-items-flag
      :help "Toggle option `Info-fontify-reference-items-flag'"]
     ["Highlighting _..._ (emphasis)" Info-toggle-fontify-emphasis
      :visible info-fontify-emphasis :style toggle :selected Info-fontify-emphasis-flag
      :help "Toggle option `Info-fontify-emphasis-flag'"]
     ["Highlighting Custom Delimited" Info-toggle-fontify-custom-delimited
      :style toggle :selected (car Info-fontify-custom-delimited)
      :help "Toggle option `Info-fontify-custom-delimited'"]
     ["     Set Custom Delimiters" Info-define-custom-delimiting
      ;; Doesn't work for Emacs < 27 if menu is used with mouse, because first read is swallowed by mouse event.
      :visible (fboundp 'read-char-from-minibuffer)
      :help "Read custom delimiter chars and set fontifying regexp from them."]
     ["Highlighting Extra" Info-toggle-fontify-extra
      :style toggle :selected Info-fontify-extra-function
      :help "Toggle option `Info-fontify-extra-function'"]
     ["Highlighting Visited Nodes" Info-toggle-fontify-visited-nodes
      :style toggle :selected Info-fontify-visited-nodes
      :help "Toggle option `Info-fontify-visited-nodes'"]
     ["Highlighting Bookmarked Links" Info-toggle-fontify-bookmarked-xrefs
      :style toggle :selected (and (boundp 'Info-fontify-bookmarked-xrefs-flag)
                                   Info-fontify-bookmarked-xrefs-flag)
      :visible (fboundp 'Info-toggle-fontify-bookmarked-xrefs)
      :help "Toggle option `Info-fontify-bookmarked-xrefs-flag'"]
     ["Bookmark Access On Visit" Info-toggle-node-access-invokes-bookmark
      :style toggle :selected (and (boundp 'Info-node-access-invokes-bookmark-flag)
                                   Info-node-access-invokes-bookmark-flag)
      :visible (fboundp 'Info-toggle-node-access-invokes-bookmark)
      :help "Toggle option `Info-node-access-invokes-bookmark-flag'"]
     ["Breadcrumbs in Mode Line" Info-breadcrumbs-in-mode-line-mode
      :style toggle :selected Info-breadcrumbs-in-mode-line-mode
      :help "Toggle showing breadcrumbs in the mode line"]
     ["Breadcrumbs in Node Header" Info-toggle-breadcrumbs-in-header
      :style toggle :selected Info-breadcrumbs-in-header-flag
      :help "Toggle showing breadcrumbs in the node header"])
    ["Table of Contents (TOC)" Info-toc :help "Go to table of contents"]
    ["Editable Outline TOC" Info-toc-outline :help "Go to editable table of contents with outline support"]
    ["Virtual Book" Info-virtual-book :help "Open table of contents of a virtual book" :active Info-saved-nodes]
    ["Save Current Node" Info-save-current-node :help "Save current node name for virtual book"]
    "--"
    ["Clone Info Buffer" clone-buffer :help "Create a twin copy of the current Info buffer."]
    ["Copy Node Name" Info-copy-current-node-name :help "Copy the name of the current node into the kill ring"]
    ["Merge Subnodes" Info-merge-subnodes :help "Integrate current node with nodes referred to in its Menu"]
    "--"
    ["Quit Info" Info-exit :help "Exit from Info"]))
 
;;(@* "Replacements for Existing Functions")
;;; Replacements for Existing Functions -------------------------------


;; REPLACE ORIGINAL in `info.el':
;;
;; Added prefix arg.
;;
(defadvice Info-history (around clear-info-hist-with-prefix-arg first (&optional clearp) activate)
  "With a prefix arg, clear the history instead, upon confirmation."
  (interactive "P")
  (if (not (ad-get-arg 0))
      ad-do-it
    (when (equal "*History*" Info-current-file) (Info-up))
    (call-interactively #'Info-history-clear)))


;; REPLACE ORIGINAL in `info.el':
;;
;; Respect option `Info-node-access-invokes-bookmark-flag'.
;;
(when (boundp 'Info-node-access-invokes-bookmark-flag) ; Emacs 24.4+

  (defadvice Info-goto-node (around bmkp-invoke-Info-bookmark activate)
    "Respect option `Info-node-access-invokes-bookmark-flag'.
If the option is non-nil then a bookmark for the node is invoked when
the node is visited, provided that the bookmark name has the default
form: `(MANUAL) NODE' (e.g.,`(emacs) Modes')."
    (if Info-node-access-invokes-bookmark-flag
        (let ((node  (ad-get-arg 0)))
          (if (member node (Info-index-nodes))
              ad-do-it
            (let ((bmk  (and Info-node-access-invokes-bookmark-flag  (Info-bookmark-for-node node))))
              (if bmk
                  (let ((Info-node-access-invokes-bookmark-flag  nil)) (bookmark--jump-via bmk 'ignore))
                ad-do-it))))
      ad-do-it))

  )


;; REPLACE ORIGINAL in `info.el':
;;
;; Added optional arg DEFAULT.
;;
(defun Info-read-node-name (prompt &optional default)
  (let* ((completion-ignore-case           t)
         (Info-read-node-completion-table  (Info-build-node-completions))
         (nodename                         (completing-read
                                            prompt 'Info-read-node-name-1 nil t nil
                                            'Info-minibuf-history default)))
    (if (equal nodename "")
        (or default  (Info-read-node-name prompt))
      nodename)))


;; REPLACE ORIGINAL in `info.el':
;;
;; 1. Added optional arg LITERALP.
;;    Use apropos matching, not literal-string matching, by default.
;;    This is the _opposite_ of the vanilla behavior after the botched fix of bug #31807.
;; 2. Added optional args ARG and NARG.
;; 3. Handle prefix arg: can match literally and can choose the manuals to search.
;; 4. Use other window, unless already in Info.
;; 5. Updated doc string to reflect enhancements.
;;
;; $$$$$$ FIXME: When LITERALP, the `apropos-read-pattern' prompt still says "word list or regexp".
;;
(defun info-apropos (pattern &optional literalp)
  "Search indexes of known Info files on your system for apropos PATTERN.
Present a menu of the possible matches.
The manuals to search are defined by option `Info-apropos-manuals'.

Just as for commands such as `apropos', PATTERN can be a word, a list
of words (separated by spaces), or a regexp (using some regexp special
characters).  If it is a word, search for matches for that word as a
substring.  If it is a list of words, search for matches for any
two (or more) of those words.

With a prefix arg, match PATTERN as a literal string, not as a regexp
or keywords.  (Vanilla Emacs version of this command has it backward.)"
  (interactive (list (apropos-read-pattern "index entries") current-prefix-arg))
  (apropos-parse-pattern pattern)
  (if (equal apropos-regexp "")
      (Info-find-node Info-apropos-file "Top")
    (let ((nodes  Info-apropos-nodes)
          nodename)
      (while (and nodes  (not (string-match apropos-regexp (nth 1 (car nodes)))))
        (setq nodes  (cdr nodes)))
      ;; Use another window, if not already in Info.
      (unless (derived-mode-p 'Info-mode) (pop-to-buffer "*info*"))
      (if nodes
          (Info-find-node Info-apropos-file (caar nodes))
        (setq nodename  (format "Index for ‘%s’" apropos-regexp))
        (push (list nodename
                    apropos-regexp
                    (Info-apropos-matches apropos-regexp (and (not literalp) 'REGEXP)))
              Info-apropos-nodes)
        (Info-find-node Info-apropos-file nodename)))))


;; REPLACE ORIGINAL in `info.el':
;;
;; 1. Added optional arg REGEXP-P.
;; 2. Search manuals defined by `Info-apropos-manuals'.
;;
(defun Info-apropos-matches (string &optional regexp-p)
  "Collect STRING matches from known Info files on your system.
Return a list of matches where each element is in the format
\((FILENAME INDEXTEXT NODENAME LINENUMBER)).

The manuals to search are defined by option `Info-apropos-manuals'.

Non-nil optional REGEXP-P means interpret STRING as a regexp, instead
of trying to match it literally."
  ;; Emacs 23 has an `interactive' spec here, for no reason.
  (unless (string= string "")
    (let ((pattern       (format "\n\\* +\\([^\n]*\\(%s\\)[^\n]*\\):[ \t]+\
\\([^\n]+\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?"
                                 (if regexp-p string (regexp-quote string))))
          (ohist         Info-history)
          (ohist-list    Info-history-list)
          (current-node  Info-current-node)
          (current-file  Info-current-file)
          manuals matches node nodes)
      (let ((Info-fontify-maximum-menu-size  nil))
        (Info-directory)
        ;; `current-node' and `current-file' are nil if you invoke `info-apropos' as the first Info command.
        ;; (`info-apropos' loads `info.el'.)
        ;; In that case, use `(DIR)Top', to avoid an error after search is complete.
        (unless current-node (setq current-file  Info-current-file
                                   current-node  Info-current-node))
        (message "Searching indices...")
        (goto-char (point-min))
        (re-search-forward "\\* Menu: *\n" nil t)
        (while (re-search-forward "\\*.*: *(\\([^)]+\\))" nil t)
          (when (or (eq Info-apropos-manuals 'all)  (member (match-string 1) Info-apropos-manuals))
            (add-to-list 'manuals (match-string 1)))) ; Ensure no duplicates in MANUALS, so `dolist' runs faster.
        (dolist (manual  (nreverse manuals))
          (message "Searching %s" manual)
          (condition-case err
              (if (setq nodes  (Info-index-nodes (Info-find-file manual)))
                  (save-excursion
                    (Info-find-node manual (car nodes))
                    (while
                        (progn
                          (goto-char (point-min))
                          (while (re-search-forward pattern nil t)
                            (let ((entry     (match-string-no-properties 1))
                                  (nodename  (match-string-no-properties 3))
                                  (line      (match-string-no-properties 4)))
                              (add-text-properties
                               (- (match-beginning 2) (match-beginning 1))
                               (- (match-end 2) (match-beginning 1))
                               '(face info-index-match) entry)
                              (setq matches  (cons (list manual entry nodename line)
                                                   matches))))
                          (setq nodes  (cdr nodes)
                                node   (car nodes)))
                      (Info-goto-node node))))
            (error (message "%s" (if (eq (car-safe err) 'error) (nth 1 err) err))
                   (sit-for 1 t)))))
      (Info-find-node current-file current-node)
      (setq Info-history       ohist
            Info-history-list  ohist-list)
      (message "Searching indices...done")
      (or (nreverse matches)  t))))


;; REPLACE ORIGINAL in `info.el':
;;
;; Added final clause to `cond', to handle virtual books.  (Emacs 23.2+)
;;
(when (or (> emacs-major-version 23)  (and (= emacs-major-version 23)  (> emacs-minor-version 1)))

  (defun Info-find-file (filename &optional noerror)
    "Return expanded FILENAME, or t if FILENAME is \"dir\".
Optional second argument NOERROR, if t, means if file is not found
just return nil (no error)."
    ;; Convert filename to lower case if not found as specified.
    ;; Expand it.
    (cond
      ((Info-virtual-call (Info-virtual-fun 'find-file filename nil) filename noerror))
      ((stringp filename)
       (let (temp temp-downcase found)
         (setq filename  (substitute-in-file-name filename))
         (let ((dirs  (if (string-match "^\\./" filename)
                          '("./") ; If specified name starts with `./' then just try current dir.
                        (if (file-name-absolute-p filename)
                            '(nil) ; No point in searching for an absolute file name
                          (if Info-additional-directory-list
                              (append Info-directory-list Info-additional-directory-list)
                            Info-directory-list)))))
           ;; Fall back on the installation directory if we can't find the info node anywhere else.
           (when installation-directory
             (setq dirs  (append dirs (list (expand-file-name "info" installation-directory)))))
           ;; Search the directory list for file FILENAME.
           (while (and dirs  (not found))
             (setq temp           (expand-file-name filename (car dirs))
                   temp-downcase  (expand-file-name (downcase filename) (car dirs)))
             ;; Try several variants of specified name.
             (let ((suffix-list  Info-suffix-list)
                   (lfn          (if (fboundp 'msdos-long-file-names) (msdos-long-file-names) t)))
               (while (and suffix-list  (not found))
                 (cond ((info-file-exists-p (info-insert-file-contents-1 temp (car (car suffix-list)) lfn))
                        (setq found  temp))
                       ((info-file-exists-p (info-insert-file-contents-1 temp-downcase (car (car suffix-list))
                                                                         lfn))
                        (setq found  temp-downcase))
                       ((and (fboundp 'msdos-long-file-names)
                             lfn
                             (info-file-exists-p (info-insert-file-contents-1 temp (car (car suffix-list))
                                                                              nil)))
                        (setq found  temp)))
                 (setq suffix-list  (cdr suffix-list))))
             (setq dirs  (cdr dirs))))
         (if found
             (setq filename  found)
           (if noerror
               (setq filename  nil)
             (unless Info-current-file (Info-directory)) ; If no previous Info file, go to directory.
             (info-user-error "Info file `%s' does not exist" filename)))
         filename))
      ((member filename '(apropos history toc))  filename))) ; Handle virtual books - `toc'.
  )


;; REPLACE ORIGINAL in `info.el':
;;
;; Added optional arg NOMSG.
;;
(defun Info-find-node (filename nodename &optional no-going-back strict-case nomsg)
  "Go to an Info node specified as separate FILENAME and NODENAME.
NO-GOING-BACK is non-nil if recovering from an error in this function;
it says do not attempt further (recursive) error recovery.

Look first for a case-sensitive match for NODENAME.  If none is found
then try a case-insensitive match, unless STRICT-CASE is non-nil.

Non-nil NOMSG means do not show a status message."
  (info-initialize)
  (setq filename  (Info-find-file filename))
  ;; Go into Info buffer.
  (unless (derived-mode-p 'Info-mode)  (Info--pop-to-buffer-same-window "*info*"))
  ;; Record the node we are leaving, if we were in one.
  (and (not no-going-back)
       Info-current-file
       (push (list Info-current-file Info-current-node (point)) Info-history))
  (Info-find-node-2 filename nodename no-going-back strict-case nomsg))


;; REPLACE ORIGINAL in `info.el':
;;
;; 1. Added optional arg NOMSG.
;; 2. Call `fit-frame' if `Info-fit-frame-flag'.
;; 3. If non-nil `Info-fontify-glossary-words' and NODENAME is `Glossary' then create glossary-words hash table.
;;
(defun Info-find-node-2 (filename nodename &optional no-going-back strict-case nomsg)
  "Helper for `Info-find-node'.
\(Same arguments.)"
  (buffer-disable-undo (current-buffer))
  (unless (derived-mode-p 'Info-mode)  (Info-mode))
  (widen)
  (setq Info-current-node  nil)
  (unwind-protect
      (let ((case-fold-search  t)
            (virtual-fun       (and (fboundp 'Info-virtual-fun) ; Emacs 23.2.
                                    (Info-virtual-fun 'find-node (or filename  Info-current-file) nodename)))
            anchorpos)
        (cond ((functionp virtual-fun)
               (let ((filename  (or filename  Info-current-file)))
                 (setq buffer-read-only               nil
                       Info-current-file              filename
                       Info-current-subfile           nil
                       Info-current-file-completions  ()
                       buffer-file-name               nil)
                 (erase-buffer)
                 (Info-virtual-call virtual-fun filename nodename no-going-back)
                 (set-marker Info-tag-table-marker nil)
                 (setq buffer-read-only  t)
                 (set-buffer-modified-p nil)
                 (if (> emacs-major-version 24)
                     (setq Info-current-node-virtual  t)
                   (set (make-local-variable 'Info-current-node-virtual) t))))
              ((not (and (or (not (boundp 'Info-current-node-virtual))  (not Info-current-node-virtual))
                         (or (null filename)  (equal Info-current-file filename))))
               ;; Switch files if necessary
               (let ((inhibit-read-only  t))
                 (when (and (boundp 'Info-current-node-virtual)  Info-current-node-virtual)
                   ;; When moving from a virtual node.
                   (if (> emacs-major-version 24)
                       (setq Info-current-node-virtual  nil)
                     (set (make-local-variable 'Info-current-node-virtual) nil))
                   (unless filename (setq filename  Info-current-file)))
                 (setq Info-current-file              nil
                       Info-current-subfile           nil
                       Info-current-file-completions  ()
                       buffer-file-name               nil)
                 (erase-buffer)
                 ;; Erase any memory of the previous coding-system, so that `info-insert-file-contents' sets the
                 ;; buffer's encoding to what the Info file specifies.
                 (set-buffer-file-coding-system 'undecided t)
                 (cond ((eq filename t)        (Info-insert-dir nomsg))
                       ((eq filename 'apropos) (insert-buffer-substring " *info-apropos*"))
                       ((eq filename 'history) (insert-buffer-substring " *info-history*"))
                       ((eq filename 'toc)     (insert-buffer-substring " *info-toc*"))
                       (t (info-insert-file-contents filename nil)
                          (setq default-directory  (file-name-directory filename))))
                 (set-buffer-modified-p nil)
                 (if (> emacs-major-version 24)
                     (setq Info-file-supports-index-cookies  (Info-file-supports-index-cookies filename))
                   (set (make-local-variable 'Info-file-supports-index-cookies)
                        (Info-file-supports-index-cookies filename)))
                 ;; See whether file has a tag table.  Record the location if yes.
                 (goto-char (point-max))
                 (forward-line -8)
                 ;; Use string-equal, not equal, to ignore text props.
                 (if (not (or (string-equal nodename "*")
                              (not (search-forward "\^_\nEnd tag table\n" nil t))))
                     (let (pos)
                       ;; We have a tag table.  Find its beginning.  Is this an indirect file?
                       (search-backward "\nTag table:\n")
                       (setq pos  (point))
                       (if (save-excursion (forward-line 2) (looking-at "(Indirect)\n"))
                           ;; It is indirect.
                           ;; Copy it to another buffer and record that the tag table is in that buffer.
                           (let ((buf     (current-buffer))
                                 (tagbuf  (or Info-tag-table-buffer
                                              (generate-new-buffer " *info tag table*"))))
                             (setq Info-tag-table-buffer  tagbuf)
                             (with-current-buffer tagbuf
                               (buffer-disable-undo (current-buffer))
                               (setq case-fold-search  t)
                               (erase-buffer)
                               (insert-buffer-substring buf))
                             (set-marker Info-tag-table-marker (match-end 0) tagbuf))
                         (set-marker Info-tag-table-marker pos)))
                   (set-marker Info-tag-table-marker nil))
                 (setq Info-current-file  filename))))
        (if (string-equal nodename "*") ; Use `string-equal', not `equal', to ignore text props.
            (progn (setq Info-current-node  nodename) (Info-set-mode-line))
          ;; Possibilities:
          ;;
          ;; 1. Anchor found in tag table
          ;; 2. Anchor *not* in tag table
          ;;
          ;; 3. Node found in tag table
          ;; 4. Node *not* found in tag table, but found in file
          ;; 5. Node *not* in tag table, and *not* in file
          ;;
          ;; *Or* the same, but in an indirect subfile.
          ;;
          ;;
          ;; Search file for a suitable node.
          (let ((guesspos  (point-min))
                (regexp    (concat "\\(Node:\\|Ref:\\) *\\("  (if (stringp nodename) (regexp-quote nodename) "")
                                   "\\) *[,\t\n\177]")))
            (catch 'foo
              ;; First, search a tag table, if any
              (when (marker-position Info-tag-table-marker)
                (let* ((m      Info-tag-table-marker)
                       (found  (if (or (> emacs-major-version 24) ; Emacs 24.4+
                                       (and (= emacs-major-version 24)  (> emacs-minor-version 3)))
                                   (Info-find-in-tag-table m regexp strict-case)
                                 (Info-find-in-tag-table m regexp))))
                  (when found           ; FOUND is (ANCHOR POS MODE)
                    (let ((filepos  (nth 1 found))) ; File position in bytes.
                      ;; If this is an indirect file, determine which file really holds this node and read it in.
                      (unless (eq (nth 2 found) 'Info-mode)
                        ;; The current buffer must be the *info* buffer on entry to `Info-read-subfile'.
                        ;; Thus the hackery above.
                        (setq filepos  (Info-read-subfile filepos)))
                      (setq guesspos  (if (fboundp 'filepos-to-bufferpos)
                                          (filepos-to-bufferpos filepos 'approximate)
                                        filepos)))
                    (when (nth 0 found) ; Handle anchor
                      (goto-char (setq anchorpos  guesspos)) (throw 'foo t)))))
              ;; Else we may have a node, which we search for.
              (goto-char (max (point-min) (- (if (> emacs-major-version 24) guesspos (byte-to-position guesspos))
                                             1000)))
              ;; Now search from our advised position (or from beg of buffer) to find the actual node.
              ;; First, check whether the node is right where we are, in case the buffer begins with a node.
              (when (> emacs-major-version 24)
                (let ((pos  (Info-find-node-in-buffer regexp strict-case)))
                  (when pos (goto-char pos) (throw 'foo t)))
                ;; If the Texinfo source had an @ifnottex block of text before the Top node,
                ;; makeinfo 5.0 and 5.1 mistakenly omitted that block's size from the starting
                ;; position of the 1st subfile, which makes GUESSPOS overshoot the correct
                ;; position by the length of that text.  So we try again with a larger slop.
                (goto-char (max (point-min) (- guesspos 10000))))
              (let ((pos  (if (<= emacs-major-version 23)
                              (Info-find-node-in-buffer regexp)
                            (Info-find-node-in-buffer regexp strict-case))))
                (when pos (goto-char pos) (throw 'foo t)))
              (when (string-match "\\([^.]+\\)\\." nodename)
                (let (Info-point-loc)
                  (Info-find-node-2 filename (match-string 1 nodename) no-going-back strict-case nomsg))
                (widen)
                (throw 'foo t))
              ;; No such anchor in tag table or node in tag table or file
              (info-user-error "No such node or anchor: `%s'" nodename))
            (Info-select-node)
            (goto-char (point-min))
            (forward-line 1)            ; skip header line
            (when (and (not (fboundp 'Info-breadcrumbs)) ; Before Emacs 23.2
                       Info-breadcrumbs-in-header-flag
                       (> Info-breadcrumbs-depth 0))
              (forward-line 1))         ; skip breadcrumbs line
            (cond (anchorpos
                   (let ((new-history  (list Info-current-file (substring-no-properties nodename))))
                     ;; Add anchors to the history too
                     (setq Info-history-list  (cons new-history (delete new-history Info-history-list))))
                   (goto-char anchorpos))
                  ((numberp Info-point-loc)
                   (forward-line (- Info-point-loc 2))
                   (setq Info-point-loc  nil))
                  ((stringp Info-point-loc)
                   (Info-find-index-name Info-point-loc)
                   (setq Info-point-loc  nil)))))
        (when (and (one-window-p t)
                   (not (window-minibuffer-p))
                   (fboundp 'fit-frame) ; Defined in `fit-frame.el'.
                   Info-fit-frame-flag)
          (fit-frame))
        (when (and Info-fontify-glossary-words  (not (equal nodename "Glossary")))
          (let* ((manual  (file-name-sans-extension (file-name-nondirectory Info-current-file)))
                 (ht-var  (intern (concat manual "-glossary-hash-table"))))
            ;; If no glossary hash table, try to get one, possibly using fallback manual.
            (unless (and (boundp ht-var)  (hash-table-p (symbol-value ht-var)))
              (Info-get-glossary-hash-table-create (info-fallback-manual-for-glossary manual))))))
    ;; If we did not finish finding the specified node, go to the previous one or to `Top' node.
    (unless (or Info-current-node  no-going-back)
      (if Info-history
          (let ((hist  (car Info-history)))
            (setq Info-history  (cdr Info-history))
            (Info-find-node (nth 0 hist) (nth 1 hist) t t nomsg) ; Pass STRICT-CASE?  Emacs bug #31137.
            (goto-char (nth 2 hist)))
        (Info-find-node Info-current-file "Top" t t nomsg)))) ; Pass STRICT-CASE?  Emacs bug #31137.
  (Info-set-mode-line))

(defun Info-get-glossary-hash-table-create (&optional manual resetp)
  "Get glossary hash table for MANUAL, creating it if it doesn't exist.
MANUAL defaults to \"emacs\".
Non-nil RESETP means re-create an existing hash table."
  (unless (equal "dir" Info-current-file)
    (unless manual (setq manual  "emacs"))
    (let ((ht-var               (intern (concat manual "-glossary-hash-table")))
          (omanual              Info-current-file)
          (onode                Info-current-node)
          (Info-fit-frame-flag  nil))
      (if (and (boundp ht-var)  (hash-table-p (symbol-value ht-var)))
          (symbol-value ht-var)
        (when (and (or (not (boundp ht-var))  resetp)
                   (not (memq (intern manual) Info-no-glossary-manuals)))
          (if (not (ignore-errors (progn (Info-find-node manual "Glossary" 'NO-GOING-BACK) t)))
              (progn (add-to-list 'Info-no-glossary-manuals (intern manual))
                     (Info-find-node manual onode 'NO-GOING-BACK nil 'NOMSG))
            (let (def dbeg dend term)
              (re-search-forward "Glossary\n\\*+\n")
              (eval `(defvar ,ht-var nil ,(format "Glossary hash table for manual %s." (capitalize manual))))
              (set ht-var (make-hash-table :test 'Info-case-insensitive-string=))
              (while (not (eobp))
                (setq def  "")
                (re-search-forward "^[^[:space:]].*")
                (setq term  (match-string 0)
                      dbeg  (match-end 0))
                (forward-char)
                ;; Skip over any blank lines between term and its definition.
                ;; This is the case for glossary entry `Text', for instance.
                (while (looking-at-p "^$") (forward-line))
                (setq dbeg  (point))
                (while (looking-at-p "^[[:space:]].*") ; Gather lines of the definition.
                  (setq def  (concat def (buffer-substring dbeg (progn (forward-line)
                                                                       (setq dbeg  (point)))))))
                (setq def  (replace-regexp-in-string "\\(     \\)" "" def nil nil 1)
                      def  (replace-regexp-in-string "\\`\n+" "" def)
                      def  (replace-regexp-in-string "\n\n\\'" "" def))
                (puthash term def (symbol-value ht-var))))
            (Info-find-node omanual onode nil nil 'NOMSG)))))))

(defun info-fallback-manual-for-glossary (manual)
  "Name of manual whose glossary is to be used for MANUAL, or nil if none."
  (catch 'info-fallback-manual-for-glossary
    (dolist (entry  Info-glossary-fallbacks-alist)
      (when (or (and (listp (cdr entry))  (memq (intern manual) (cdr entry))) ; Do this first, before catch-all.
                (eq t (cdr entry)))
        (throw 'info-fallback-manual-for-glossary (symbol-name (car entry)))))
    nil))

(define-hash-table-test
  'Info-case-insensitive-string=
  'Info-case-insensitive-string=
  'Info-case-insensitive-string-hash)

(defun Info-case-insensitive-string= (string1 string2)
  "Return non-nil if STRING1 and STRING2 are the same, ignoring case."
  (eq t (compare-strings string1 nil nil string2 nil nil 'IGNORE-CASE)))

(defun Info-case-insensitive-string-hash (string)
  "Return hash code for `upcase' of STRING."
  (sxhash (upcase string)))


;; REPLACE ORIGINAL in `info.el':
;;
;; Pass non-nil NOMSG arg to `Info-insert-dir'.
;;
(defun Info-directory-find-node (filename nodename &optional no-going-back nomsg)
  "Directory-specific implementation of Info-find-node-2."
  (Info-insert-dir 'NOMSG))


;; REPLACE ORIGINAL in `info.el':
;;
;; Added optional arg NOMSG.  If non-nil then do not show progress messages.
;;
(defun Info-insert-dir (&optional nomsg)
  (if (and Info-dir-contents
           Info-dir-file-attributes
           (eval (cons 'and ; Verify that none of the files we used has changed since we used it.
                       (mapcar (lambda (elt)
                                 (let ((curr (file-attributes (file-truename (car elt))))) ; Handle symlinks
                                   (when curr (setcar (nthcdr 4 curr) 0)) ; Don't compare the access time.
                                   (setcar (nthcdr 4 (cdr elt)) 0)
                                   (equal (cdr elt) curr)))
                               Info-dir-file-attributes))))
      (progn (insert Info-dir-contents) (goto-char (point-min)))
    (let ((dirs              (if Info-additional-directory-list
                                 (append Info-directory-list Info-additional-directory-list)
                               Info-directory-list))
          (dir-file-attrs    ())
          (case-fold-search  t) ; Bind this in case the user sets it to nil.
          problems ; This is set non-nil if we find a problem in some input files.
          buffers buffer others nodes dirs-done)
      (while dirs  ; Search the directory list for the directory file.
        (let ((truename (file-truename (expand-file-name (car dirs)))))
          (or (member truename dirs-done)
              (member (directory-file-name truename) dirs-done)
              ;; Try several variants of specified name.  Try upcasing, appending `.info', or both.
              (let* (file
                     (attrs  (or (progn (setq file  (expand-file-name "dir" truename)) (file-attributes file))
                                 (progn (setq file  (expand-file-name "DIR" truename)) (file-attributes file))
                                 (progn (setq file  (expand-file-name "dir.info" truename))
                                        (file-attributes file))
                                 (progn (setq file  (expand-file-name "DIR.INFO" truename))
                                        (file-attributes file))
                                 ;; Shouldn't really happen, but sometimes does, eg on Debian systems with
                                 ;; buggy packages; so may as well try it.
                                 ;; https://lists.gnu.org/archive/html/emacs-devel/2012-03/msg00005.html
                                 (progn (setq file  (expand-file-name "dir.gz" truename))
                                        (file-attributes file)))))
                (setq dirs-done  (cons truename (cons (directory-file-name truename) dirs-done)))
                (when attrs
                  (with-current-buffer (generate-new-buffer " info dir")
                    (unless (or buffers  nomsg) (message "Composing main Info directory..."))
                    (condition-case nil
                        ;; Index nodes include null bytes.  DIR files should not have indices, but who knows...
                        (let ((inhibit-null-byte-detection  t))
                          (insert-file-contents file)
                          (set (make-local-variable 'Info-dir-file-name) file)
                          (push (current-buffer) buffers)
                          (push (cons file attrs) dir-file-attrs))
                      (error (kill-buffer (current-buffer))))))))
          (unless (cdr dirs)
            (set (make-local-variable 'Info-dir-contents-directory) (file-name-as-directory (car dirs))))
          (setq dirs  (cdr dirs))))
      (unless buffers  (info-user-error "Can't find the Info directory node"))
      ;; Distinguish the dir file that comes with Emacs from all the others.  Yes, that is really what this
      ;; is supposed to do.  The definition of `Info-directory-list' puts it first on that list and so last in
      ;; `buffers' at this point.
      (setq buffer  (car (last buffers))
            others  (delq buffer buffers))
      ;; Insert the entire original dir file as a start; note that we've already saved its default directory
      ;; to use as the default directory for the whole concatenation.
      (save-excursion (insert-buffer-substring buffer))
      (dolist (other  others) ; Look at each of the other buffers, one by one.
        (let (this-buffer-nodes)
          (with-current-buffer other    ; In each, find all the menus.
            (goto-char (point-min))
            (while (re-search-forward "^\\* Menu:" nil t) ; Find each menu, and add an elt to NODES for it.
              (while (and (zerop (forward-line 1))  (eolp)))
              (let ((beg  (point))
                    nodename end)
                (re-search-backward "^\^_")
                (search-forward "Node: ")
                (setq nodename  (and (looking-at (Info-following-node-name-re))  (match-string-no-properties 1)))
                (search-forward "\n\^_" nil 'move)
                (beginning-of-line)
                (setq end  (point))
                (push (list nodename other beg end) this-buffer-nodes)))
            (if (assoc-string "top" this-buffer-nodes t)
                (setq nodes  (nconc this-buffer-nodes nodes))
              (setq problems  t)
              (unless nomsg (message "No `top' node in %s" Info-dir-file-name))))))
      (re-search-forward "^\\* Menu:") ; Add to the main menu a menu item for each other node.
      (forward-line 1)
      (let ((menu-items  '("top"))
            (end         (save-excursion (search-forward "\^_" nil t) (point))))
        (dolist (node  nodes)
          (let ((nodename  (car node)))
            (save-excursion
              (or (member (downcase nodename)  menu-items)
                  (re-search-forward (concat "^\\* +" (regexp-quote nodename) "::") end t)
                  (progn (insert "* " nodename "::" "\n") (push nodename menu-items)))))))
      (dolist (node  nodes) ; Now take each node of each of the other buffers and merge it into the main buffer.
        (let ((case-fold-search  t)
              (nodename          (car node)))
          (goto-char (point-min))
          ;; Find the like-named node in the main buffer.
          (if (re-search-forward (concat "^\^_.*\n.*Node: " (regexp-quote nodename) "[,\n\t]") nil t)
              (progn (search-forward "\n\^_" nil 'move) (beginning-of-line) (insert "\n"))
            (goto-char (point-max))     ; If none exists, add one.
            (insert "\^_\nFile: dir\tNode: " nodename "\n\n* Menu:\n\n"))
          ;; Merge text from the other buffer's menu into the menu in the like-named node in the main buffer.
          (apply 'insert-buffer-substring (cdr node))))
      (Info-dir-remove-duplicates)
      ;; Kill all the buffers we just made, including the special one excised.
      (mapc 'kill-buffer (cons buffer buffers))
      (goto-char (point-min))
      (unless nomsg (message "Composing main Info directory...%s"
                             (if problems "problems encountered, see `*Messages*'" "done")))
      (set (make-local-variable 'Info-dir-contents) (buffer-string))
      (set (make-local-variable 'Info-dir-file-attributes) dir-file-attrs)))
  (setq default-directory  Info-dir-contents-directory))


;; REPLACE ORIGINAL in `info.el':
;;
;; Handle `Info-breadcrumbs-in-mode-line-mode'.
;;
(defun Info-set-mode-line ()
  "Set the Info mode line.
If `Info-breadcrumbs-in-mode-line-mode' is non-nil, insert breadcrumbs."
  (if Info-breadcrumbs-in-mode-line-mode
      (Info-insert-breadcrumbs-in-mode-line)
    (setq mode-line-buffer-identification
	  (nconc (propertized-buffer-identification "%b")
	         (list (concat " ("
                               (propertize ; Remove trailing ".info", ".info.gz", etc.
                                (if (stringp Info-current-file)
                                    (if (fboundp 'string-replace) ; Emacs 28.1+
                                        (string-replace
                                         "%" "%%" (replace-regexp-in-string
                                                   "\\..*\\'" "" (file-name-nondirectory Info-current-file)))
                                      (replace-regexp-in-string
                                       "%" "%%" (file-name-nondirectory Info-current-file)))
                                  (format "*%S*" Info-current-file))
                                'help-echo "Manual name")
                               ") ")
		       (if Info-current-node
		           (propertize
                            (if (fboundp 'string-replace) ; Emacs 28.1+
                                (string-replace "%" "%%" Info-current-node)
                              (replace-regexp-in-string "%" "%%" Info-current-node))
			    'face 'mode-line-buffer-id
			    'help-echo "mouse-1: scroll forward, mouse-3: scroll back"
			    'mouse-face 'mode-line-highlight
			    'local-map Info-mode-line-node-keymap)
		         ""))))))

(defun Info-insert-breadcrumbs-in-mode-line ()
  (let ((nodes   (Info-toc-nodes Info-current-file))
        (node    Info-current-node)
        (crumbs  ())
        (depth   Info-breadcrumbs-depth-internal)
        (text    ""))
    ;; Get ancestors from the cached parent-children node info
    (while (and (not (equal "Top" node))  (> depth 0))
      (setq node  (nth 1 (assoc node nodes)))
      (when node (push node crumbs))
      (setq depth  (1- depth)))
    ;; Add bottom node.
    (setq crumbs  (nconc crumbs (list Info-current-node)))
    (when crumbs
      ;; Add top node (and continuation if needed).
      (setq crumbs  (cons "Top" (if (member (pop crumbs) '(nil "Top"))
                                    crumbs
                                  (cons nil crumbs))))
      (dolist (node  crumbs)
        (let ((crumbs-map  (make-sparse-keymap))
              (menu-map    (make-sparse-keymap "Breadcrumbs in Mode Line")))
          (define-key crumbs-map [mode-line mouse-3] menu-map)
          (when node
            (define-key menu-map [Info-prev]
              `(menu-item "Previous Node" Info-prev
                          :visible ,(Info-check-pointer "prev[ious]*") :help "Go to the previous node"))
            (define-key menu-map [Info-next]
              `(menu-item "Next Node" Info-next
                          :visible ,(Info-check-pointer "next") :help "Go to the next node"))
            (define-key menu-map [separator] '("--"))
            (define-key menu-map [Info-breadcrumbs-in-mode-line-mode]
              `(menu-item "Toggle Breadcrumbs" Info-breadcrumbs-in-mode-line-mode
                          :help "Toggle displaying breadcrumbs in the Info mode-line"
                          :button (:toggle . Info-breadcrumbs-in-mode-line-mode)))
            (define-key menu-map [Info-set-breadcrumbs-depth]
              `(menu-item "Set Breadcrumbs Depth" Info-set-breadcrumbs-depth
                          :help "Set depth of breadcrumbs to show in the mode-line"))
            (setq node  (if (equal node Info-current-node)
                            (propertize
                             (replace-regexp-in-string "%" "%%" Info-current-node)
                             'face 'mode-line-buffer-id
                             'help-echo "mouse-1: Scroll back, mouse-2: Scroll forward, mouse-3: Menu"
                             'mouse-face 'mode-line-highlight
                             'local-map
                             (progn
                               (define-key crumbs-map [mode-line mouse-1] 'Info-mouse-scroll-down)
                               (define-key crumbs-map [mode-line mouse-2] 'Info-mouse-scroll-up)
                               crumbs-map))
                          (propertize
                           node
                           'local-map (progn (define-key crumbs-map [mode-line mouse-1]
                                               `(lambda () (interactive) (Info-goto-node ,node)))
                                             (define-key crumbs-map [mode-line mouse-2]
                                               `(lambda () (interactive) (Info-goto-node ,node)))
                                             crumbs-map)
                           'mouse-face 'mode-line-highlight
                           'help-echo "mouse-1, mouse-2: Go to this node; mouse-3: Menu")))))
        (let ((nodetext  (if (not (equal node "Top"))
                             node
                           (concat (format "(%s)" (if (stringp Info-current-file)
                                                      (file-name-nondirectory Info-current-file)
                                                    ;; Some legacy code can still use a symbol.
                                                    Info-current-file))
                                   node))))
          (setq text  (concat text (if (equal node "Top") "" " > ") (if node nodetext "...")))))
      (make-local-variable 'mode-line-format) ; Needed for Emacs 21+.
      (setq mode-line-format  text))))



;;; ;; REPLACE ORIGINAL in `info.el':
;;; ;;
;;; ;; BUG FIX (bug #1085, reported 2008-10-04).
;;; ;; 1. Match closing paren, if present.
;;; ;; 2. If only opening paren and CODE = t, then wrap each file name in ().
;;; ;;
;;;   (defun Info-read-node-name-1 (string predicate code)
;;;   "Internal function used by `Info-read-node-name'.
;;; See `completing-read' for a description of arguments and usage."
;;;     (cond ((string-match "\\`(\\([^)]*\\))\\'" string) ; e.g. (emacs) or (emacs-mime)
;;;            (cond ((eq code nil) string)
;;;                  ((eq code t) (list string))
;;;                  (t t)))
;;;           ((string-match "\\`(\\([^)]*\\)\\'" string) ; e.g. (emacs
;;;            (let ((ctwc  (completion-table-with-context
;;;                          "("
;;;                          (apply-partially
;;;                           'completion-table-with-terminator ")"
;;;                           (apply-partially 'Info-read-node-name-2
;;;                                            Info-directory-list
;;;                                            (mapcar 'car Info-suffix-list)))
;;;                          (match-string 1 string)
;;;                          predicate
;;;                          code)))
;;;              (cond ((eq code nil) ctwc)
;;;                    ((eq code t) (mapcar (lambda (file) (concat "(" file ")")) ctwc))
;;;                    (t t))))
;;;           ((string-match "\\`(" string) ; e.g. (emacs)Mac OS or (jlkj - just punt.
;;;            (cond ((eq code nil) string)
;;;                  ((eq code t) nil)
;;;                  (t t)))
;;;           ;; Otherwise use Info-read-node-completion-table - e.g. Mac OS
;;;           (t (complete-with-action code Info-read-node-completion-table string predicate))))



;; REPLACE ORIGINAL in `info.el':
;;
;; Menus `Menu Item' and `References' are in submenu `Info' > `Navigation', not in main menu `Info'.
;;
(defun Info-menu-update ()
  "Update the Info menu for the current node."
  (condition-case nil
      (if (or (not (derived-mode-p 'Info-mode))
              (equal (list Info-current-file Info-current-node)
                     Info-menu-last-node))
          ()
        ;; Update `Menu Item' menu.
        (let* ((Info-complete-menu-buffer  (current-buffer))
               (items                      (nreverse (condition-case nil
                                                         (Info-complete-menu-item "" nil t)
                                                       (error nil))))
               (number                     0)
               entries current)
          (while (and items  (< number 9))
            (setq current  (car items)
                  items    (cdr items)
                  number   (1+ number)
                  entries  (cons `[,current (Info-menu ,current) :keys ,(format "%d" number)] entries)))
          (when items (setq entries  (cons ["Other..." Info-menu t] entries)))
          (unless entries (setq entries  (list ["No menu" nil nil] nil :active)))
          (easy-menu-change '("Info" "Navigation") "Menu Item" (nreverse entries)))
        ;; Update `Reference' menu.  Vanilla code taken from `Info-follow-reference'.
        (let ((items             ())
              (number            0)
              (case-fold-search  t)
              str ii entries current)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "\\*note[ \n\t]+\\([^:]*\\):" nil t)
              (setq str  (match-string 1)
                    ii   0)
              (while (setq ii  (string-match "[ \n\t]+" str ii))
                (setq str  (concat (substring str 0 ii) " " (substring str (match-end 0)))
                      ii   (1+ ii)))
              (setq items  (cons str items))))
          (while (and items  (< number 9))
            (setq current  (car items)
                  items    (cdr items)
                  number   (1+ number)
                  entries  (cons `[,current (Info-follow-reference ,current) t] entries)))
          (when items (setq entries  (cons ["Other..." Info-follow-reference t] entries)))
          (unless entries (setq entries  (list ["No references" nil nil] nil :active)))
          ;; (easy-menu-change '("Info") "Reference" (nreverse entries)))
          (easy-menu-change '("Info" "Navigation") "Reference" (nreverse entries)))
        ;; Update last seen node.
        (setq Info-menu-last-node  (list Info-current-file Info-current-node)))
    (error (ding))))


;; REPLACE ORIGINAL in `info.el':
;;
;; 1. Added optional arg MSGP.
;; 2. Added in-progress message ("Looking...")
;; 3, Return nil if not found.
;;
(defun Info-find-emacs-command-nodes (command &optional msgp)
  "Return a list of locations documenting COMMAND.
The `info-file' property of COMMAND says which Info manual to search.
If COMMAND has no property, the variable `Info-file-list-for-emacs'
defines heuristics for which Info manual to try.

The locations are of the format used in variable `Info-history', that
is, (FILENAME NODENAME BUFFERPOS), where BUFFERPOS is the line number
of the first element of the returned list (which is treated specially
in `Info-goto-emacs-command-node'), and 0 for the other elements of
the list."
  (let ((where      ())
        (cmd-desc   (concat "^\\* +" (regexp-quote (symbol-name command))
                            "\\( <[0-9]+>\\)?:\\s *\\(.*\\)\\."
                            "\\(?:[ \t\n]+(line +\\([0-9]+\\))\\)?"))
        (info-file  "emacs")            ;default
        line-number)
    ;; Determine which info file this command is documented in.
    (if (get command 'info-file)
        (setq info-file  (get command 'info-file))
      ;; If it doesn't say explicitly, test its name against
      ;; various prefixes that we know.
      (let ((file-list  Info-file-list-for-emacs))
        (while file-list
          (let* ((elt               (car file-list))
                 (name              (if (consp elt) (car elt) elt))
                 (file              (if (consp elt) (cdr elt) elt))
                 (case-fold-search  nil)
                 (regexp            (concat "\\`" (regexp-quote name) "\\(\\'\\|-\\)")))
            (if (string-match regexp (symbol-name command))
                (setq info-file  file
                      file-list  ()))
            (setq file-list  (cdr file-list))))))
    (when msgp (message "Looking for command `%s' in Info manual `%s'..."
                        command (file-name-nondirectory info-file)))
    (save-excursion
      (condition-case nil
          (progn (Info-find-node info-file "Top" (not msgp))
                 (or (and (search-forward "\n* menu:" nil t)
                          (re-search-forward "\n\\* \\(.*\\<Index\\>\\)" nil t))
                     (info-user-error "Info file `%s' appears to lack an index" info-file)))
        (error nil))                    ; Return nil: not found.
      (goto-char (match-beginning 1))
      ;; Bind Info-history to nil, to prevent the index nodes from
      ;; getting into the node history.
      (let ((Info-history       ())
            (Info-history-list  ())
            node
            (nodes              (Info-index-nodes)))
        (Info-goto-node (car nodes))
        (while (progn (goto-char (point-min))
                      (while (re-search-forward cmd-desc nil t)
                        (setq where        (cons (list Info-current-file (match-string-no-properties 2) 0)
                                                 where)
                              line-number  (and (match-beginning 3)  (string-to-number (match-string 3)))))
                      (and (setq nodes  (cdr nodes)
                                 node   (car nodes))))
          (Info-goto-node node)))
      (if (and line-number  where)
          (cons (list (nth 0 (car where)) (nth 1 (car where)) line-number) (cdr where))
        where))))


;; REPLACES ORIGINAL in `info.el':
;;
;; 1. Uses `completing-read' in interactive spec, with `symbol-nearest-point'
;;    (defined in `thingatpt+.el') or `symbol-at-point' (defined in `thingatpt.el').
;; 2. Added optional arg MSGP (interactive-p).
;; 3. Message if single node found.
;; 4. Returns `num-matches' if found; nil if not.
;; 4. Pass MSGP to `Info-find-emacs-command-nodes'.
;;
;;;###autoload
(defun Info-goto-emacs-command-node (command &optional msgp)
  "Go to the Info node in the Emacs manual for command COMMAND.
The command is found by looking it up in Emacs manual's indexes,
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'.
COMMAND must be a symbol or string."
  (interactive
   (let ((symb  (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                      ((fboundp 'symbol-at-point)      (symbol-at-point))
                      (t nil)))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Find documentation for command: "
                                    obarray 'commandp t nil nil (symbol-name symb) t))
           t)))
  (unless (commandp command)
    (signal 'wrong-type-argument (list 'commandp command)))
  (let ((where  (Info-find-emacs-command-nodes command msgp)))
    (if where
        (let ((num-matches  (length where)))
          ;; Get Info running, and pop to it in another window.
          (save-window-excursion (info))
          (unless (derived-mode-p 'Info-mode) (pop-to-buffer "*info*"))
          ;; Bind Info-history to nil, to prevent the last Index node visited by
          ;; `Info-find-emacs-command-nodes' from being pushed onto the history.
          (let ((Info-history       ())
                (Info-history-list  ()))
            (Info-find-node (car (car where)) (car (cdr (car where)))))
          (if (<= num-matches 1)
              (when msgp (message "This info node documents command `%s'." command))

            ;; (car where) will be pushed onto Info-history
            ;; when/if they go to another node.  Put the other
            ;; nodes that were found on the history.
            (setq Info-history  (nconc (cdr where) Info-history))
            (when msgp
              (message "Found %d other entr%s.  Use %s to see %s."
                       (1- num-matches) (if (> num-matches 2) "ies" "y")
                       (substitute-command-keys "\\<Info-mode-map>\\[Info-history-back]")
                       (if (> num-matches 2) "them" "it"))))
          num-matches)                  ; Return num-matches found.
      (and (interactive-p)              ; Return nil for unfound.
           (info-user-error "No documentation found for command `%s'" command)))))


;; REPLACES ORIGINAL in `info.el':

;; 1. If key's command is not found, then `Info-search' for key sequence in text.
;; 2. Added optional arg MSGP (interactive-p).
;; 3. Message for repeating.
;; 4. Return non-nil for found, nil for not found.
;;
;;;###autoload
(defun Info-goto-emacs-key-command-node (key &optional msgp)
  "Go to the node in the Emacs manual describing command bound to KEY.
KEY is a string.

Interactively, if the binding is `execute-extended-command', then a
command is read.

The command is found by looking it up in Emacs manual's indexes,
or in another manual's index found via COMMAND's `info-file' property
or the variable `Info-file-list-for-emacs'.

If key's command cannot be found by looking in indexes, then
`Info-search' is used to search for the key sequence in the info text."
  (interactive "kFind documentation for key: \np")
  (let ((command  (lookup-key global-map key))
        (pp-key   (key-description key)))
    (when (natnump command) (setq command  (key-binding key))) ; E.g. menu item.
    (cond ((null command)
           (when msgp (message "No doc found for key sequence `%s'." pp-key))
           nil)                         ; RETURN nil: not found.
          ((and (interactive-p)  (eq command 'execute-extended-command)) ; Read a new command name.
           (Info-goto-emacs-command-node (read-command "Find documentation for command: ") msgp))
          (t
           (let ((this-file        Info-current-file)
                 (this-node        Info-current-node)
                 (num-cmd-matches  (Info-goto-emacs-command-node command msgp)))
             (cond (num-cmd-matches
                    ;; Found key's command via a manual index.
                    (when msgp
                      (if (<= num-cmd-matches 1)
                          (message "This info node documents key `%s'." pp-key)
                        (message
                         (substitute-command-keys
                          (concat "Found %d other entr%s.  Use "
                                  "\\<Info-mode-map>`\\[Info-history-back]' to see %s."))
                         (1- num-cmd-matches) (if (> num-cmd-matches 2) "ies" "y")
                         (if (> num-cmd-matches 2) "them" "it"))))
                    num-cmd-matches)  ; RETURN num-cmd-matches: found.
                   (this-file
                    ;; Couldn't find key's command via a manual index.
                    ;; Get back to where we were, if in Info.
                    ;; Would be better if there were a save-xxx-excursion-xxx
                    ;; that would work.
                    (Info-goto-node (concat "(" this-file ")" this-node))
                    ;; Would be better to now try looking for the key in indexes (e.g. Key
                    ;; Index). Instead, just look for the key sequence in the text.
                    (when msgp
                      (message "Not found using Index. Searching for \"%s\" in text..." pp-key)
                      (sit-for 3))
                    (condition-case err
                        (progn
                          (Info-search (regexp-quote pp-key))
                          (when msgp
                            (message (substitute-command-keys
                                      "Use \\<Info-mode-map>`\\[Info-search] RET' to search again for `%s'.")
                                     pp-key))
                          t)            ; RETURN t: found.
                      (search-failed (when msgp (message "No documentation found for key `%s'." pp-key))
                                     nil))) ; RETURN nil: not found.
                   (t nil))))))) ; RETURN nil: not found.


;; REPLACES ORIGINAL in `info.el':
;;
;; 1. File name in face `info-file'.
;; 2. If `Info-fontify-emphasis-flag', fontify _..._.
;; 3. If `Info-fontify-indented-text-chars' then fontify text indented at least that many chars.
;; 4. If `Info-fontify-quotations', fontify ‘...’ and `...' in face `info-quoted-name',
;;    “...” in face `info-double-quoted-name', and "..." in face `info-string'.
;; 5. If `Info-fontify-quotations' and `Info-fontify-angle-bracketed-flag' then
;;    fontify <...> in face `info-quoted-name'.
;; 6. If `Info-fontify-quotations' and `Info-fontify-isolated-quote-flag' then fontify
;;    isolated ' and ` in faces `Info-isolated-quote' and `Info-isolated-backquote', respectively.
;;
(when (not (fboundp 'Info-breadcrumbs)) ; Emacs 23.1, not 23.2+

  (defun Info-fontify-node ()
    "Fontify the node."
    (save-excursion
      (let* ((inhibit-read-only  t)
             (case-fold-search   t)
             paragraph-markers
             (not-fontified-p ; the node hasn't already been fontified
              (not (let ((where  (next-single-property-change (point-min) 'font-lock-face)))
                     (and where  (not (= where (point-max)))))))
             (fontify-visited-p ; visited nodes need to be re-fontified
              (and Info-fontify-visited-nodes
                   ;; Don't take time to refontify visited nodes in huge nodes
                   Info-fontify-maximum-menu-size
                   (< (- (point-max) (point-min)) Info-fontify-maximum-menu-size)))
             rbeg rend)

        ;; Fontify EMPHASIS: _..._
        ;;
        ;; Do this first because it can remove existing highlighting.
        ;; Wrap stuff with `ignore-errors' in case the regexp is faulty.  This is a special case, because we use
        ;; text property `invisible', not just font-lock.
        (when info-fontify-emphasis
          (goto-char (point-min))
          (when (and font-lock-mode  not-fontified-p)
            (let ((regexp  (Info-emphasis-regexp)))
              (unless (eq info-nomatch regexp)
                (while (ignore-errors (re-search-forward regexp nil t))
                  (ignore-errors
                    (let ((fn  (if Info-fontify-emphasis-flag #'add-text-properties #'remove-text-properties)))
                      (funcall fn (match-beginning 0) (1+ (match-beginning 0))
                               '(invisible t front-sticky nil rear-nonsticky t))
                      (funcall fn (1- (match-end 0)) (match-end 0)
                               '(invisible t front-sticky nil rear-nonsticky t))
                      (funcall fn (match-beginning 1) (match-end 1) '(font-lock-face info-emphasis)))))))))

        ;; Fontify HEADER LINE
        (goto-char (point-min))
        (when (and not-fontified-p  (looking-at "^File: \\([^,: \t]+\\),?[ \t]+"))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'info-file))
        (goto-char (point-min))
        (when (and not-fontified-p  (looking-at "^\\(File: [^,: \t]+,?[ \t]+\\)?"))
          (while (looking-at "[ \t]*\\([^:, \t\n]+\\):[ \t]+\\([^:,\t\n]+\\),?")
            (goto-char (match-end 0))
            (let* ((nbeg  (match-beginning 2))
                   (nend  (match-end 2))
                   (tbeg  (match-beginning 1))
                   (tag   (match-string 1)))
              (if (string-equal (downcase tag) "node")
                  (put-text-property nbeg nend 'font-lock-face 'info-header-node)
                (put-text-property nbeg nend 'font-lock-face 'info-header-xref)
                (put-text-property tbeg nend 'mouse-face 'highlight)
                (put-text-property tbeg nend
                                   'help-echo (concat "mouse-2: Go to node " (buffer-substring nbeg nend)))
                ;; Always set up the text property keymap.
                ;; It will either be used in the buffer
                ;; or copied in the header line.
                (put-text-property tbeg nend 'keymap
                                   (cond
                                    ((string-equal (downcase tag) "prev") Info-prev-link-keymap)
                                    ((string-equal (downcase tag) "next") Info-next-link-keymap)
                                    ((string-equal (downcase tag) "up"  ) Info-up-link-keymap))))))
          (when (and Info-breadcrumbs-in-header-flag  (> Info-breadcrumbs-depth 0))
            (Info-insert-breadcrumbs))

          ;; Treat header line.
          (when Info-use-header-line
            (goto-char (point-min))
            (let* ((header-end  (line-end-position))
                   (header
                    ;; If we find neither Next: nor Prev: link, show the entire
                    ;; node header.  Otherwise, don't show the File: and Node:
                    ;; parts, to avoid wasting precious space on information that
                    ;; is available in the mode line.
                    (if (re-search-forward "\\(next\\|up\\|prev[ious]*\\): " header-end t)
                        (progn (goto-char (match-beginning 1))
                               (buffer-substring (point) header-end))
                      (if (re-search-forward "node:[ \t]*[^ \t]+[ \t]*" header-end t)
                          (concat "No next, prev or up links  --  "
                                  (buffer-substring (point) header-end))
                        (buffer-substring (point) header-end)))))
              (put-text-property (point-min) (1+ (point-min))
                                 'header-line (replace-regexp-in-string
                                               "%"
                                               ;; Preserve text properties on duplicated `%'.
                                               (lambda (s) (concat s s)) header))
              ;; Hide the part of the first line that is in the header, if it is just part.
              (cond ((and Info-breadcrumbs-in-header-flag  (> Info-breadcrumbs-depth 0))
                     (put-text-property (point-min) (1+ header-end) 'invisible t))
                    ((not (bobp))
                     ;; Hide the punctuation at the end, too.
                     (skip-chars-backward " \t,")
                     (put-text-property (point) header-end 'invisible t))))))

        ;; Fontify INDENTED TEXT (e.g. code blocks, ASCII diagrams).
        (goto-char (point-min))
        (when (and Info-fontify-indented-text-chars  (not (equal "Top" Info-current-node)))
          (Info-fontify-indented-text))

        ;; Fontify QUOTATIONS: ‘...’, `...', “...”, and "..."
        (goto-char (point-min))
        (when Info-fontify-quotations (Info-fontify-quotations))

        ;; Fontify CUSTOM-DELIMITED: text between custom delimiters
        (goto-char (point-min))
        (when (car Info-fontify-custom-delimited) (Info-fontify-custom-delimited))

        ;; Fontify EXTRA: something else
        (when Info-fontify-extra-function
          (goto-char (point-min))
          (funcall Info-fontify-extra-function))

        ;; Fontify REFERENCE ITEMS: `-- Function:', `-- Variable:', etc.
        (goto-char (point-min))
        (when Info-fontify-reference-items-flag (Info-fontify-reference-items))

        ;; Fontify TITLES
        (goto-char (point-min))
        (when (and font-lock-mode not-fontified-p)
          (while (and (re-search-forward "\n\\([^ \t\n].+\\)\n\\(\\*\\*+\\|==+\\|--+\\|\\.\\.+\\)$" nil t)
                      ;; Only consider it as an underlined title if the ASCII
                      ;; underline has the same size as the text.  A typical
                      ;; counter example is when a continuation "..." is alone
                      ;; on a line.
                      (= (string-width (match-string 1))
                         (string-width (match-string 2))))
            (let* ((c     (preceding-char))
                   (face  (cond ((= c ?*) 'info-title-1)
                                ((= c ?=) 'info-title-2)
                                ((= c ?-) 'info-title-3)
                                (t        'info-title-4))))
              (put-text-property (match-beginning 1) (match-end 1)
                                 'font-lock-face face))
            ;; This is a serious problem for trying to handle multiple
            ;; frame types at once.  We want this text to be invisible
            ;; on frames that can display the font above.
            (when (memq (framep (selected-frame)) '(x pc w32 ns))
              (add-text-properties (1- (match-beginning 2)) (match-end 2)
                                   '(invisible t front-sticky nil rear-nonsticky t)))))

        ;; Fontify CROSS REFERENCES
        (goto-char (point-min))
        (when (or not-fontified-p  fontify-visited-p)
          (while (re-search-forward
                  "\\(\\*Note[ \n\t]+\\)\\([^:]*\\)\\(:[ \t]*\\([^.,:(]*\\)\\(\\(([^)]\
*)\\)[^.,:]*\\)?[,:]?\n?\\)"
                  nil t)
            (let ((start  (match-beginning 0))
                  (next   (point))
                  other-tag)
              (when not-fontified-p
                (when Info-hide-note-references
                  (when (and (not (eq Info-hide-note-references 'hide))
                             (> (line-number-at-pos) 4)) ; Skip breadcrumbs
                    ;; *Note is often used where *note should have been
                    (goto-char start)
                    (skip-syntax-backward " ")
                    (when (memq (char-before) '(?\( ?\[ ?\{))
                      ;; Check whether the paren is preceded by
                      ;; an end of sentence
                      (skip-syntax-backward " ("))
                    (setq other-tag  (cond ((save-match-data (looking-back "\\<see" nil))
                                            "")
                                           ((save-match-data (looking-back "\\<in" nil))
                                            "")
                                           ((memq (char-before) '(nil ?\. ?! ??))
                                            "See ")
                                           ((save-match-data
                                              (save-excursion (search-forward "\n\n" start t)))
                                            "See ")
                                           (t "see "))))
                  (goto-char next)
                  (add-text-properties (match-beginning 1)
                                       (or (save-match-data
                                             (let ((start1  (match-beginning 1))) ; Don't hide \n after *Note
                                               (and (string-match "\n" (match-string 1))
                                                    (+ start1 (match-beginning 0)))))
                                           (match-end 1))
                                       (if other-tag
                                           `(display ,other-tag front-sticky nil rear-nonsticky t)
                                         '(invisible t front-sticky nil rear-nonsticky t))))
                (add-text-properties (match-beginning 2) (match-end 2)
                                     (list 'help-echo (if (or (match-end 5)  (not (equal (match-string 4) "")))
                                                          (concat "mouse-2: go to " (or (match-string 5)
                                                                                        (match-string 4)))
                                                        "mouse-2: go to this node")
                                           'mouse-face 'highlight)))
              (when (or not-fontified-p  fontify-visited-p)
                (setq rbeg  (match-beginning 2)
                      rend  (match-end 2))
                (put-text-property
                 rbeg
                 rend
                 'font-lock-face
                 ;; Display visited nodes in a different face
                 (if (and Info-fontify-visited-nodes
                          (save-match-data
                            (let* ((node
                                    (replace-regexp-in-string
                                     "^[ \t]+" ""
                                     (replace-regexp-in-string
                                      "[ \t\n]+" " "
                                      (or (match-string-no-properties 5)
                                          (and (not (equal (match-string 4) ""))
                                               (match-string-no-properties 4))
                                          (match-string-no-properties 2)))))
                                   (external-link-p  (string-match "(\\([^)]+\\))\\([^)]*\\)" node))
                                   (file             (if external-link-p
                                                         (file-name-nondirectory
                                                          (match-string-no-properties 1 node))
                                                       Info-current-file))
                                   (hl               Info-history-list)
                                   res)
                              (when external-link-p
                                (setq node  (if (equal (match-string 2 node) "")
                                                "Top"
                                              (match-string-no-properties 2 node))))
                              (while hl
                                (if (and (string-equal node (nth 1 (car hl)))
                                         (equal file (if (and external-link-p (stringp (caar hl)))
                                                         (file-name-nondirectory (caar hl))
                                                       (caar hl))))
                                    (setq res  (car hl)
                                          hl   nil)
                                  (setq hl  (cdr hl))))
                              res)))
                     'info-xref-visited
                   'info-xref))
                ;; For multiline ref, unfontify newline and surrounding whitespace
                (save-excursion
                  (goto-char rbeg)
                  (save-match-data (while (re-search-forward "\\s-*\n\\s-*" rend t nil)
                                     (remove-text-properties (match-beginning 0) (match-end 0)
                                                             '(font-lock-face t))))))
              (when not-fontified-p
                (when (memq Info-hide-note-references '(t hide))
                  (add-text-properties (match-beginning 3) (match-end 3)
                                       '(invisible t front-sticky nil rear-nonsticky t))
                  ;; Unhide the file name of the external reference in parens
                  (if (and (match-string 6)  (not (eq Info-hide-note-references 'hide)))
                      (remove-text-properties (match-beginning 6) (match-end 6) '(invisible t front-sticky nil
                                                                                            rear-nonsticky t)))
                  ;; Unhide newline because hidden newlines cause too long lines
                  (save-match-data (let ((beg3  (match-beginning 3))
                                         (end3  (match-end 3)))
                                     (if (and (string-match "\n[ \t]*" (match-string 3))
                                              (not (save-match-data (save-excursion (goto-char (1+ end3))
                                                                                    (looking-at "[.)]*$")))))
                                         (remove-text-properties (+ beg3 (match-beginning 0))
                                                                 (+ beg3 (match-end 0))
                                                                 '(invisible t front-sticky nil
                                                                             rear-nonsticky t))))))
                (when (and Info-refill-paragraphs  Info-hide-note-references)
                  (push (set-marker (make-marker) start) paragraph-markers))))))

        ;; REFILL PARAGRAPHS (experimental feature)
        (when (and not-fontified-p  Info-refill-paragraphs  paragraph-markers)
          (let ((fill-nobreak-invisible          t)
                (fill-individual-varying-indent  nil)
                (paragraph-start                 "\f\\|[ \t]*[-*]\\|[ \t]*$")
                (paragraph-separate              ".*\\.[ \t]*\n[ \t]\\|[ \t]*[-*]\\|[ \t\f]*$")
                (adaptive-fill-mode              nil))
            (goto-char (point-max))
            (dolist (m  paragraph-markers)
              (when (< m (point))
                (goto-char m)
                (beginning-of-line)
                (let ((beg  (point)))
                  (when (zerop (forward-paragraph))
                    (fill-individual-paragraphs beg (point) nil nil)
                    (goto-char beg))))
              (set-marker m nil))))

        ;; Fontify MENU ITEMS
        (goto-char (point-min))
        (when (and (or not-fontified-p  fontify-visited-p)
                   (search-forward "\n* Menu:" nil t)
                   Info-fontify-maximum-menu-size ; Don't take time to annotate huge menus
                   (< (- (point-max) (point)) Info-fontify-maximum-menu-size))
          (let ((n  0)
                cont)
            (while (re-search-forward (concat "^\\* Menu:\\|\\(?:^\\* +\\(" Info-menu-entry-name-re "\\)\\(:"
                                              Info-node-spec-re "\\([ \t]*\\)\\)\\)")
                                      nil t)
              (when (match-beginning 1)
                (when not-fontified-p
                  (setq n  (1+ n))
                  (if (and (<= n 9)  (zerop (% n 3))) ; visual aids to help with 1-9 keys
                      (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                                         'font-lock-face 'info-menu-star)))
                (when not-fontified-p
                  (add-text-properties
                   (match-beginning 1) (match-end 1)
                   (list 'help-echo (if (and (match-end 3)  (not (equal (match-string 3) "")))
                                        (concat "mouse-2: go to " (match-string 3))
                                      "mouse-2: go to this node")
                         'mouse-face 'highlight)))
                (when (or not-fontified-p  fontify-visited-p)
                  (put-text-property
                   (match-beginning 1) (match-end 1)
                   'font-lock-face
                   ;; Display visited menu items in a different face
                   (if (and Info-fontify-visited-nodes
                            (save-match-data
                              (let* ((node             (if (equal (match-string 3) "")
                                                           (match-string-no-properties 1)
                                                         (match-string-no-properties 3)))
                                     (external-link-p  (string-match "(\\([^)]+\\))\\([^)]*\\)" node))
                                     (file             (if external-link-p
                                                           (file-name-nondirectory
                                                            (match-string-no-properties 1 node))
                                                         Info-current-file))
                                     (hl               Info-history-list)
                                     res)
                                (when external-link-p
                                  (setq node  (if (equal (match-string 2 node) "")
                                                  "Top"
                                                (match-string-no-properties 2 node))))
                                (while hl
                                  (if (and (string-equal node (nth 1 (car hl)))
                                           (equal file (if (and external-link-p (stringp (caar hl)))
                                                           (file-name-nondirectory (caar hl))
                                                         (caar hl))))
                                      (setq res  (car hl)
                                            hl   nil)
                                    (setq hl  (cdr hl))))
                                res)))
                       'info-xref-visited
                     'info-xref)))
                (when (and not-fontified-p
                           (memq Info-hide-note-references '(t hide))
                           (not (Info-index-node)))
                  (put-text-property (match-beginning 2) (1- (match-end 6)) 'invisible t)
                  ;; Unhide the file name in parens
                  (if (and (match-end 4)  (not (eq (char-after (match-end 4)) ?.)))
                      (remove-text-properties (match-beginning 4) (match-end 4) '(invisible t)))
                  ;; We need a stretchable space like :align-to but with a minimum value.
                  (put-text-property (1- (match-end 6)) (match-end 6) 'display (if (>= 22
                                                                                       (- (match-end 1)
                                                                                          (match-beginning 0)))
                                                                                   '(space :align-to 24)
                                                                                 '(space :width 2)))
                  (setq cont  (looking-at "."))
                  (while (and (= (forward-line 1) 0)  (looking-at "\\([ \t]+\\)[^*\n]"))
                    (put-text-property (match-beginning 1) (1- (match-end 1)) 'invisible t)
                    (put-text-property (1- (match-end 1)) (match-end 1) 'display (if cont
                                                                                     '(space :align-to 26)
                                                                                   '(space :align-to 24)))
                    (setq cont  t)))))))

        ;; Fontify GLOSSARY WORDS
        ;;
        ;; Do this AFTER fontifying menu items and references.
        (goto-char (point-min))
        (forward-line 4)
        (when Info-fontify-glossary-words (Info-fontify-glossary-words))

        ;; Fontify MENU HEADERS
        (goto-char (point-min))
        (when (and not-fontified-p ; Add face `info-menu-header' to any header before a menu entry
                   (re-search-forward "^\\* Menu:" nil t))
          (put-text-property (match-beginning 0) (match-end 0)
                             'font-lock-face 'info-menu-header)
          (while (re-search-forward "\n\n\\([^*\n ].*\\)\n\n?[*]" nil t)
            (put-text-property (match-beginning 1) (match-end 1)
                               'font-lock-face 'info-menu-header)))

        ;; Hide index line numbers
        (goto-char (point-min))
        (when (and not-fontified-p  (Info-index-node))
          (while (re-search-forward "[ \t\n]*(line +[0-9]+)" nil t)
            (put-text-property (match-beginning 0) (match-end 0)
                               'invisible t)))

        ;; Fontify HTTP AND FTP REFERENCES
        (goto-char (point-min))
        (when not-fontified-p
          (while (re-search-forward "\\(https?\\|ftp\\)://[^ \t\n\"`({<>})']+" nil t)
            (add-text-properties (match-beginning 0) (match-end 0)
                                 '(font-lock-face info-xref
                                                  mouse-face highlight
                                                  help-echo "mouse-2: go to this URL"))))

        ;; Hide any empty lines at the end of the node.
        (goto-char (point-max))
        (skip-chars-backward "\n")
        (when (< (1+ (point)) (point-max)) (put-text-property (1+ (point)) (point-max) 'invisible t))
        (set-buffer-modified-p nil))))

  )


;; REPLACES ORIGINAL in `info.el':
;;
;; 1. File name in face `info-file'.
;; 2. If `Info-fontify-emphasis-flag', fontify _..._.
;; 3. If `Info-fontify-indented-text-chars' then fontify text indented at least that many chars.
;; 4. If `Info-fontify-quotations', fontify ‘...’ and `...' in face `info-quoted-name',
;;    “...” in face `info-double-quoted-name', and "..." in face `info-string'.
;; 5. If `Info-fontify-quotations' and `Info-fontify-angle-bracketed-flag' then
;;    fontify <...> in face `info-quoted-name'.
;; 6. If `Info-fontify-quotations' and `Info-fontify-isolated-quote-flag' then fontify
;;    isolated ' and ` in faces `Info-isolated-quote' and `Info-isolated-backquote', respectively.
;;
(when (and (fboundp 'Info-breadcrumbs)  ; Emacs 23.2 through 24.1
           (or (= emacs-major-version 23)
               (and (= emacs-major-version 24)  (= emacs-minor-version 1))))

  (defun Info-fontify-node ()
    "Fontify the node."
    (save-excursion
      (let* ((inhibit-read-only  t)
             (case-fold-search   t)
             paragraph-markers
             (not-fontified-p ; the node hasn't already been fontified
              (not (let ((where  (next-single-property-change (point-min) 'font-lock-face)))
                     (and where  (not (= where (point-max)))))))
             (fontify-visited-p ; visited nodes need to be re-fontified
              (and Info-fontify-visited-nodes
                   Info-fontify-maximum-menu-size ; Don't take time to refontify visited nodes in huge nodes
                   (< (- (point-max) (point-min)) Info-fontify-maximum-menu-size)))
             rbeg rend)

        ;; Fontify EMPHASIS: _..._
        ;;
        ;; Do this first because it can remove existing highlighting.
        ;; Wrap stuff with `ignore-errors' in case the regexp is faulty.  This is a special case, because we use
        ;; text property `invisible', not just font-lock.
        (when info-fontify-emphasis
          (goto-char (point-min))
          (when (and font-lock-mode  not-fontified-p)
            (let ((regexp  (Info-emphasis-regexp)))
              (unless (eq info-nomatch regexp)
                (while (ignore-errors (re-search-forward regexp nil t))
                  (ignore-errors
                    (let ((fn  (if Info-fontify-emphasis-flag #'add-text-properties #'remove-text-properties)))
                      (funcall fn (match-beginning 0) (1+ (match-beginning 0))
                               '(invisible t front-sticky nil rear-nonsticky t))
                      (funcall fn (1- (match-end 0)) (match-end 0)
                               '(invisible t front-sticky nil rear-nonsticky t))
                      (funcall fn (match-beginning 1) (match-end 1) '(font-lock-face info-emphasis)))))))))

        ;; Fontify HEADER LINE
        (goto-char (point-min))
        (when (and not-fontified-p  (looking-at "^File: \\([^,: \t]+\\),?[ \t]+"))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'info-file))
        (goto-char (point-min))
        (when (and not-fontified-p  (looking-at "^\\(File: [^,: \t]+,?[ \t]+\\)?"))
          (while (looking-at "[ \t]*\\([^:, \t\n]+\\):[ \t]+\\([^:,\t\n]+\\),?")
            (goto-char (match-end 0))
            (let* ((nbeg  (match-beginning 2))
                   (nend  (match-end 2))
                   (tbeg  (match-beginning 1))
                   (tag   (match-string 1)))
              (if (string-equal (downcase tag) "node")
                  (put-text-property nbeg nend 'font-lock-face 'info-header-node)
                (put-text-property nbeg nend 'font-lock-face 'info-header-xref)
                (put-text-property tbeg nend 'mouse-face 'highlight)
                (put-text-property tbeg nend
                                   'help-echo (concat "mouse-2: Go to node " (buffer-substring nbeg nend)))
                ;; Always set up the text property keymap.
                ;; It will either be used in the buffer
                ;; or copied in the header line.
                (put-text-property tbeg nend 'keymap
                                   (cond
                                    ((string-equal (downcase tag) "prev") Info-prev-link-keymap)
                                    ((string-equal (downcase tag) "next") Info-next-link-keymap)
                                    ((string-equal (downcase tag) "up"  ) Info-up-link-keymap))))))

          ;; Treat header line.
          (when Info-use-header-line
            (goto-char (point-min))
            (let* ((header-end  (line-end-position))
                   (header
                    ;; If we find neither Next: nor Prev: link, show the entire
                    ;; node header.  Otherwise, don't show the File: and Node:
                    ;; parts, to avoid wasting precious space on information that
                    ;; is available in the mode line.
                    (if (re-search-forward "\\(next\\|up\\|prev[ious]*\\): " header-end t)
                        (progn (goto-char (match-beginning 1))
                               (buffer-substring (point) header-end))
                      (if (re-search-forward "node:[ \t]*[^ \t]+[ \t]*" header-end t)
                          (concat "No next, prev or up links  --  "
                                  (buffer-substring (point) header-end))
                        (buffer-substring (point) header-end)))))
              (put-text-property (point-min) (1+ (point-min))
                                 'header-line (replace-regexp-in-string
                                               "%"
                                               ;; Preserve text properties on duplicated `%'.
                                               (lambda (s) (concat s s)) header))
              ;; Hide the part of the first line that is in the header, if it is just part.
              (cond ((and Info-breadcrumbs-in-header-flag  (> Info-breadcrumbs-depth 0))
                     (let ((ov (make-overlay (point-min) (1+ header-end))))
                       (overlay-put ov 'display (Info-breadcrumbs))
                       (overlay-put ov 'evaporate t)))
                    ((not (bobp))
                     ;; Hide the punctuation at the end, too.
                     (skip-chars-backward " \t,")
                     (put-text-property (point) header-end 'invisible t))))))

        ;; Fontify INDENTED TEXT (e.g. code blocks, ASCII diagrams).
        (goto-char (point-min))
        (when (and Info-fontify-indented-text-chars  (not (equal "Top" Info-current-node)))
          (Info-fontify-indented-text))

        ;; Fontify QUOTATIONS: ‘...’, `...', “...”, and "..."
        (goto-char (point-min))
        (when Info-fontify-quotations (Info-fontify-quotations))

        ;; Fontify CUSTOM-DELIMITED: text between custom delimiters
        (goto-char (point-min))
        (when (car Info-fontify-custom-delimited) (Info-fontify-custom-delimited))

        ;; Fontify EXTRA: something else
        (when Info-fontify-extra-function
          (goto-char (point-min))
          (funcall Info-fontify-extra-function))

        ;; Fontify REFERENCE ITEMS: `-- Function:', `-- Variable:', etc.
        (goto-char (point-min))
        (when Info-fontify-reference-items-flag (Info-fontify-reference-items))

        ;; Fontify TITLES
        (goto-char (point-min))
        (when (and font-lock-mode  not-fontified-p)
          (while (and (re-search-forward "\n\\([^ \t\n].+\\)\n\\(\\*\\*+\\|==+\\|--+\\|\\.\\.+\\)$" nil t)
                      ;; Only consider it as an underlined title if the ASCII underline has the same size as the
                      ;; text.  A typical counter example is when a continuation "..." is alone on a line.
                      (= (string-width (match-string 1)) (string-width (match-string 2))))
            (let* ((c     (preceding-char))
                   (face  (cond ((= c ?*) 'info-title-1)
                                ((= c ?=) 'info-title-2)
                                ((= c ?-) 'info-title-3)
                                (t        'info-title-4))))
              (put-text-property (match-beginning 1) (match-end 1)
                                 'font-lock-face face))
            ;; This is a serious problem for trying to handle multiple
            ;; frame types at once.  We want this text to be invisible
            ;; on frames that can display the font above.
            (when (memq (framep (selected-frame)) '(x pc w32 ns))
              (add-text-properties (1- (match-beginning 2)) (match-end 2)
                                   '(invisible t front-sticky nil rear-nonsticky t)))))

        ;; Fontify CROSS REFERENCES
        (goto-char (point-min))
        (when (or not-fontified-p  fontify-visited-p)
          (while (re-search-forward
                  "\\(\\*Note[ \n\t]+\\)\\([^:]*\\)\\(:[ \t]*\\([^.,:(]*\\)\\(\\(([^)]\
*)\\)[^.,:]*\\)?[,:]?\n?\\)"
                  nil t)
            (let ((start  (match-beginning 0))
                  (next   (point))
                  other-tag)
              (when not-fontified-p
                (when Info-hide-note-references
                  (when (and (not (eq Info-hide-note-references 'hide))
                             (> (line-number-at-pos) 4)) ; Skip breadcrumbs
                    ;; *Note is often used where *note should have been
                    (goto-char start)
                    (skip-syntax-backward " ")
                    (when (memq (char-before) '(?\( ?\[ ?\{))
                      (skip-syntax-backward " (")) ; Check whether the paren is preceded by an end of sentence
                    (setq other-tag  (cond ((save-match-data (looking-back "\\<see" nil)) "")
                                           ((save-match-data (looking-back "\\<in" nil)) "")
                                           ((memq (char-before) '(nil ?\. ?! ??)) "See ")
                                           ((save-match-data (save-excursion (search-forward "\n\n" start t)))
                                            "See ")
                                           (t "see "))))
                  (goto-char next)
                  (add-text-properties
                   (match-beginning 1)
                   (or (save-match-data
                         (let ((start1  (match-beginning 1))) ; Don't hide \n after *Note
                           (and (string-match "\n" (match-string 1))  (+ start1 (match-beginning 0)))))
                       (match-end 1))
                   (if other-tag
                       `(display ,other-tag front-sticky nil rear-nonsticky t)
                     '(invisible t front-sticky nil rear-nonsticky t))))
                (add-text-properties
                 (match-beginning 2) (match-end 2)
                 (list 'help-echo (if (or (match-end 5)  (not (equal (match-string 4) "")))
                                      (concat "mouse-2: go to " (or (match-string 5)  (match-string 4)))
                                    "mouse-2: go to this node")
                       'mouse-face 'highlight)))
              (when (or not-fontified-p  fontify-visited-p)
                (setq rbeg  (match-beginning 2)
                      rend  (match-end 2))
                (put-text-property
                 rbeg
                 rend
                 'font-lock-face
                 (if (and Info-fontify-visited-nodes ; Display visited nodes in a different face
                          (save-match-data
                            (let* ((node             (replace-regexp-in-string
                                                      "^[ \t]+" ""
                                                      (replace-regexp-in-string
                                                       "[ \t\n]+" " "
                                                       (or (match-string-no-properties 5)
                                                           (and (not (equal (match-string 4) ""))
                                                                (match-string-no-properties 4))
                                                           (match-string-no-properties 2)))))
                                   (external-link-p  (string-match "(\\([^)]+\\))\\([^)]*\\)" node))
                                   (file             (if external-link-p
                                                         (file-name-nondirectory
                                                          (match-string-no-properties 1 node))
                                                       Info-current-file))
                                   (hl               Info-history-list)
                                   res)
                              (when external-link-p
                                (setq node  (if (equal (match-string 2 node) "")
                                                "Top"
                                              (match-string-no-properties 2 node))))
                              (while hl
                                (if (and (string-equal node (nth 1 (car hl)))
                                         (equal file (if (and external-link-p  (stringp (caar hl)))
                                                         (file-name-nondirectory (caar hl))
                                                       (caar hl))))
                                    (setq res  (car hl)
                                          hl   nil)
                                  (setq hl  (cdr hl))))
                              res)))
                     'info-xref-visited
                   'info-xref))
                (save-excursion ; For multiline ref, unfontify newline and surrounding whitespace
                  (goto-char rbeg)
                  (save-match-data
                    (while (re-search-forward "\\s-*\n\\s-*" rend t nil)
                      (remove-text-properties (match-beginning 0) (match-end 0) '(font-lock-face t))))))
              (when not-fontified-p
                (when (memq Info-hide-note-references '(t hide))
                  (add-text-properties (match-beginning 3) (match-end 3)
                                       '(invisible t front-sticky nil rear-nonsticky t))
                  ;; Unhide the file name of the external reference in parens
                  (when (and (match-string 6)  (not (eq Info-hide-note-references 'hide)))
                    (remove-text-properties (match-beginning 6) (match-end 6)
                                            '(invisible t front-sticky nil rear-nonsticky t)))
                  (save-match-data ; Unhide newline because hidden newlines cause too long lines
                    (let ((beg3  (match-beginning 3))
                          (end3  (match-end 3)))
                      (when (and (string-match "\n[ \t]*" (match-string 3))
                                 (not (save-match-data (save-excursion (goto-char (1+ end3))
                                                                       (looking-at "[.)]*$")))))
                        (remove-text-properties (+ beg3 (match-beginning 0))
                                                (+ beg3 (match-end 0))
                                                '(invisible t front-sticky nil rear-nonsticky t))))))
                (when (and Info-refill-paragraphs  Info-hide-note-references)
                  (push (set-marker (make-marker) start) paragraph-markers))))))

        ;; REFILL PARAGRAPHS (experimental feature)
        (when (and not-fontified-p  Info-refill-paragraphs  paragraph-markers)
          (let ((fill-nobreak-invisible          t)
                (fill-individual-varying-indent  nil)
                (paragraph-start                 "\f\\|[ \t]*[-*]\\|[ \t]*$")
                (paragraph-separate              ".*\\.[ \t]*\n[ \t]\\|[ \t]*[-*]\\|[ \t\f]*$")
                (adaptive-fill-mode              nil))
            (goto-char (point-max))
            (dolist (m  paragraph-markers)
              (when (< m (point))
                (goto-char m)
                (beginning-of-line)
                (let ((beg  (point)))
                  (when (zerop (forward-paragraph))
                    (fill-individual-paragraphs beg (point) nil nil)
                    (goto-char beg))))
              (set-marker m nil))))

        ;; Fontify MENU ITEMS
        (goto-char (point-min))
        (when (and (or not-fontified-p  fontify-visited-p)
                   (search-forward "\n* Menu:" nil t)
                   Info-fontify-maximum-menu-size ; Don't take time to annotate huge menus
                   (< (- (point-max) (point)) Info-fontify-maximum-menu-size))
          (let ((n  0)
                cont)
            (while (re-search-forward
                    (concat "^\\* Menu:\\|\\(?:^\\* +\\(" Info-menu-entry-name-re "\\)\\(:"
                            Info-node-spec-re "\\([ \t]*\\)\\)\\)")
                    nil t)
              (when (match-beginning 1)
                (when not-fontified-p
                  (setq n  (1+ n))
                  (if (and (<= n 9)  (zerop (% n 3))) ; visual aids to help with 1-9 keys
                      (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                                         'font-lock-face 'info-menu-star)))
                (when not-fontified-p
                  (add-text-properties
                   (match-beginning 1) (match-end 1)
                   (list 'help-echo (if (and (match-end 3)  (not (equal (match-string 3) "")))
                                        (concat "mouse-2: go to " (match-string 3))
                                      "mouse-2: go to this node")
                         'mouse-face 'highlight)))
                (when (or not-fontified-p  fontify-visited-p)
                  (put-text-property
                   (match-beginning 1) (match-end 1)
                   'font-lock-face
                   (if (and Info-fontify-visited-nodes ; Display visited menu items in a different face
                            (save-match-data
                              (let* ((node             (if (equal (match-string 3) "")
                                                           (match-string-no-properties 1)
                                                         (match-string-no-properties 3)))
                                     (external-link-p  (string-match "(\\([^)]+\\))\\([^)]*\\)" node))
                                     (file             (if external-link-p
                                                           (file-name-nondirectory
                                                            (match-string-no-properties 1 node))
                                                         Info-current-file))
                                     (hl               Info-history-list)
                                     res)
                                (when external-link-p
                                  (setq node  (if (equal (match-string 2 node) "")
                                                  "Top"
                                                (match-string-no-properties 2 node))))
                                (while hl
                                  (if (and (string-equal node (nth 1 (car hl)))
                                           (equal file (if (and external-link-p  (stringp (caar hl)))
                                                           (file-name-nondirectory (caar hl))
                                                         (caar hl))))
                                      (setq res  (car hl)
                                            hl   nil)
                                    (setq hl  (cdr hl))))
                                res)))
                       'info-xref-visited
                     'info-xref)))
                (when (and not-fontified-p
                           (memq Info-hide-note-references '(t hide))
                           (not (Info-index-node)))
                  (put-text-property (match-beginning 2) (1- (match-end 6)) 'invisible t)
                  ;; Unhide the file name in parens
                  (if (and (match-end 4)  (not (eq (char-after (match-end 4)) ?.)))
                      (remove-text-properties (match-beginning 4) (match-end 4) '(invisible t)))
                  ;; We need a stretchable space like :align-to but with a minimum value.
                  (put-text-property (1- (match-end 6)) (match-end 6) 'display
                                     (if (>= 22 (- (match-end 1) (match-beginning 0)))
                                         '(space :align-to 24)
                                       '(space :width 2)))
                  (setq cont  (looking-at "."))
                  (while (and (= (forward-line 1) 0)  (looking-at "\\([ \t]+\\)[^*\n]"))
                    (put-text-property (match-beginning 1) (1- (match-end 1)) 'invisible t)
                    (put-text-property (1- (match-end 1)) (match-end 1) 'display (if cont
                                                                                     '(space :align-to 26)
                                                                                   '(space :align-to 24)))
                    (setq cont  t)))))))

        ;; Fontify GLOSSARY WORDS
        ;;
        ;; Do this AFTER fontifying menu items and references.
        (goto-char (point-min))
        (forward-line 4)
        (when Info-fontify-glossary-words (Info-fontify-glossary-words))

        ;; Fontify MENU HEADERS
        (goto-char (point-min))
        ;; Add face `info-menu-header' to any header before a menu entry
        (when (and not-fontified-p  (re-search-forward "^\\* Menu:" nil t))
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'info-menu-header)
          (while (re-search-forward "\n\n\\([^*\n ].*\\)\n\n?[*]" nil t)
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'info-menu-header)))

        ;; Hide index line numbers
        (goto-char (point-min))
        (when (and not-fontified-p  (Info-index-node))
          (while (re-search-forward "[ \t\n]*(line +[0-9]+)" nil t)
            (put-text-property (match-beginning 0) (match-end 0)
                               'invisible t)))

       ; ; Fontify HTTP AND FTP REFERENCES
        (goto-char (point-min))
        (when not-fontified-p
          (while (re-search-forward "\\(https?\\|ftp\\)://[^ \t\n\"`({<>})']+" nil t)
            (add-text-properties (match-beginning 0) (match-end 0)
                                 '(font-lock-face info-xref
                                                  mouse-face highlight
                                                  help-echo "mouse-2: go to this URL"))))

        ;; Hide any empty lines at the end of the node.
        (goto-char (point-max))
        (skip-chars-backward "\n")
        (when (< (1+ (point)) (point-max)) (put-text-property (1+ (point)) (point-max) 'invisible t))
        (set-buffer-modified-p nil))))

  )


;; REPLACES ORIGINAL in `info.el':
;;
;; 1. File name in face `info-file'.
;; 2. If `Info-fontify-emphasis-flag', fontify _..._.
;; 3. If `Info-fontify-indented-text-chars' then fontify text indented at least that many chars.
;; 4. If `Info-fontify-quotations', fontify ‘...’ and `...' in face `info-quoted-name',
;;    “...” in face `info-double-quoted-name', and "..." in face `info-string'.
;; 5. If `Info-fontify-quotations' and `Info-fontify-angle-bracketed-flag' then
;;    fontify <...> in face `info-quoted-name'.
;; 6. If `Info-fontify-quotations' and `Info-fontify-isolated-quote-flag' then fontify
;;    isolated ' and ` in faces `Info-isolated-quote' and `Info-isolated-backquote', respectively.
;; 7. If non-nil `Info-fontify-glossary-words' then fontify glossary words.
;;
(when (or (> emacs-major-version 24)    ; Emacs 24.2+
          (and (= emacs-major-version 24)  (> emacs-minor-version 1)))

  (defun Info-fontify-node ()
    "Fontify the node."
    (save-excursion
      (let* ((inhibit-read-only     t)
             (case-fold-search      t)
             (fontify-bookmarked-p  (and (boundp 'Info-fontify-bookmarked-xrefs-flag)
                                         Info-fontify-bookmarked-xrefs-flag))
             (node-not-too-large    (and (or fontify-bookmarked-p  Info-fontify-visited-nodes)
                                         ;; Don't take time to refontify xrefs in huge nodes
                                         Info-fontify-maximum-menu-size
                                         (or (eq t Info-fontify-maximum-menu-size)
                                             (< (- (point-max) (point-min)) Info-fontify-maximum-menu-size))))
             (fontify-bookmarked-p  (and node-not-too-large  fontify-bookmarked-p))
             (fontify-visited-p     (and node-not-too-large  Info-fontify-visited-nodes))
             (not-fontified-p       (not (let ((where  (next-single-property-change (point-min) 'font-lock-face)))
                                           (and where  (not (= where (point-max)))))))
             paragraph-markers rbeg rend)

        ;; Fontify EMPHASIS: _..._
        ;;
        ;; Do this first because it can remove existing highlighting.
        ;; Wrap stuff with `ignore-errors' in case the regexp is faulty.  This is a special case, because we use
        ;; text property `invisible', not just font-lock.
        (when info-fontify-emphasis
          (goto-char (point-min))
          (when (and font-lock-mode  not-fontified-p)
            (let ((regexp  (Info-emphasis-regexp)))
              (unless (eq info-nomatch regexp)
                (while (ignore-errors (re-search-forward regexp nil t))
                  (ignore-errors
                    (let ((fn  (if Info-fontify-emphasis-flag #'add-text-properties #'remove-text-properties)))
                      (funcall fn (match-beginning 0) (1+ (match-beginning 0))
                               '(invisible t front-sticky nil rear-nonsticky t))
                      (funcall fn (1- (match-end 0)) (match-end 0)
                               '(invisible t front-sticky nil rear-nonsticky t))
                      (funcall fn (match-beginning 1) (match-end 1) '(font-lock-face info-emphasis)))))))))

        ;; Fontify HEADER LINE
        (goto-char (point-min))
        (when (and not-fontified-p  (looking-at "^File: \\([^,: \t]+\\),?[ \t]+"))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'info-file))
        (goto-char (point-min))
        (when (and not-fontified-p  (looking-at "^\\(File: [^,: \t]+,?[ \t]+\\)?"))
          (while (looking-at "[ \t]*\\([^:, \t\n]+\\):[ \t]+\\([^:,\t\n]+\\),?")
            (goto-char (match-end 0))
            (let* ((nbeg  (match-beginning 2))
                   (nend  (match-end 2))
                   (tbeg  (match-beginning 1))
                   (tag   (match-string 1)))
              (if (string-equal (downcase tag) "node")
                  (put-text-property nbeg nend 'font-lock-face 'info-header-node)
                (put-text-property nbeg nend 'font-lock-face 'info-header-xref)
                (put-text-property tbeg nend 'mouse-face (if (facep 'header-line-highlight) ; Emacs 26+
                                                             'header-line-highlight
                                                           'highlight))
                (put-text-property tbeg nend
                                   'help-echo (concat "mouse-2: Go to node " (buffer-substring nbeg nend)))
                ;; Set up the text property keymap.  Depending on
                ;; `Info-use-header-line', it is either used in the
                ;; buffer, or copied to the header line.  A symbol value
                ;; of the `link-args' property is handled specially by
                ;; `Info-mouse-follow-link'.
                (put-text-property tbeg nend 'keymap Info-link-keymap)
                (put-text-property tbeg nend 'link-args (intern (downcase tag))))))
          ;; Treat header line.
          (when Info-use-header-line
            (goto-char (point-min))
            (let* ((header-end  (line-end-position))
                   (header
                    ;; If we find neither Next: nor Prev: link, show the entire
                    ;; node header.  Otherwise, don't show the File: and Node:
                    ;; parts, to avoid wasting precious space on information that
                    ;; is available in the mode line.
                    (if (re-search-forward "\\(next\\|up\\|prev[ious]*\\): " header-end t)
                        (progn (goto-char (match-beginning 1))
                               (buffer-substring (point) header-end))
                      (if (re-search-forward "node:[ \t]*[^ \t]+[ \t]*" header-end t)
                          (concat "No next, prev or up links  --  "
                                  (buffer-substring (point) header-end))
                        (buffer-substring (point) header-end)))))
              (put-text-property (point-min) (1+ (point-min))
                                 'header-line (replace-regexp-in-string
                                               "%"
                                               ;; Preserve text properties on duplicated `%'.
                                               (lambda (s) (concat s s)) header))
              ;; Hide the part of the first line that is in the header, if it is just part.
              (cond ((and Info-breadcrumbs-in-header-flag  (> Info-breadcrumbs-depth 0))
                     (let ((ov  (make-overlay (point-min) (1+ header-end))))
                       (overlay-put ov 'display (Info-breadcrumbs))
                       (overlay-put ov 'evaporate t)))
                    ((not (bobp))
                     ;; Hide the punctuation at the end, too.
                     (skip-chars-backward " \t,")
                     (put-text-property (point) header-end 'invisible t)
                     ;; Hide the suffix (`.info') of the Info file name.
                     (beginning-of-line)
                     (if (re-search-forward (format "File: %s\\([^,\n\t]+\\),"
                                                    (if (stringp Info-current-file)
                                                        (file-name-nondirectory Info-current-file)
                                                      Info-current-file))
                                            header-end t)
                         (put-text-property (match-beginning 1) (match-end 1) 'invisible t)))))))

        ;; Fontify INDENTED TEXT (e.g. code blocks, ASCII diagrams).
        (goto-char (point-min))
        (when (and Info-fontify-indented-text-chars  (not (equal "Top" Info-current-node)))
          (Info-fontify-indented-text))

        ;; Fontify QUOTATIONS: ‘...’, `...', “...”, and "..."
        (goto-char (point-min))
        (when Info-fontify-quotations (Info-fontify-quotations))

        ;; Fontify CUSTOM-DELIMITED: text between custom delimiters
        (goto-char (point-min))
        (when (car Info-fontify-custom-delimited) (Info-fontify-custom-delimited))

        ;; Fontify EXTRA: something else
        (when Info-fontify-extra-function
          (goto-char (point-min))
          (funcall Info-fontify-extra-function))

        ;; Fontify REFERENCE ITEMS: `-- Function:', `-- Variable:', etc.
        (goto-char (point-min))
        (when Info-fontify-reference-items-flag (Info-fontify-reference-items))

        ;; Fontify TITLES
        (goto-char (point-min))
        (when (and font-lock-mode  not-fontified-p)
          (while (and (re-search-forward "\n\\([^ \t\n].+\\)\n\\(\\*\\*+\\|==+\\|--+\\|\\.\\.+\\)$" nil t)
                      ;; Only consider it as an underlined title if the ASCII underline has the same size as
                      ;; the text.  A typical counter example is when a continuation "..." is alone on a line.
                      (= (string-width (match-string 1)) (string-width (match-string 2))))
            (let* ((c     (preceding-char))
                   (face  (cond ((= c ?*) 'info-title-1)
                                ((= c ?=) 'info-title-2)
                                ((= c ?-) 'info-title-3)
                                (t        'info-title-4))))
              (put-text-property (match-beginning 1) (match-end 1)
                                 'font-lock-face face))
            ;; This is a serious problem for trying to handle multiple
            ;; frame types at once.  We want this text to be invisible
            ;; on frames that can display the font above.
            (when (memq (framep (selected-frame)) '(x pc w32 ns))
              (add-text-properties (1- (match-beginning 2)) (match-end 2)
                                   '(invisible t front-sticky nil rear-nonsticky t)))))

        ;; Fontify CROSS REFERENCES
        (goto-char (point-min))
        (when (or not-fontified-p  fontify-bookmarked-p  fontify-visited-p)
          (while (re-search-forward
                  "\\(\\*Note[ \n\t]+\\)\\([^:]*\\)\\(:[ \t]*\\([^.,:(]*\\)\\(\\(([^)]\
*)\\)[^.,:]*\\)?[,:]?\n?\\)"
                  nil t)
            (let ((start  (match-beginning 0))
                  (next   (point))
                  other-tag)
              (when not-fontified-p
                (when Info-hide-note-references
                  (when (and (not (eq Info-hide-note-references 'hide))
                             (> (line-number-at-pos) 4)) ; Skip breadcrumbs
                    ;; *Note is often used where *note should have been
                    (goto-char start)
                    (skip-syntax-backward " ")
                    (when (memq (char-before) '(?\( ?\[ ?\{))
                      (skip-syntax-backward " (")) ; Check whether the paren is preceded by an end of sentence
                    (setq other-tag  (cond ((save-match-data (looking-back "\\<see" nil)) "")
                                           ((save-match-data (looking-back "\\<in" nil)) "")
                                           ((memq (char-before) '(nil ?\. ?! ??)) "See ")
                                           ((save-match-data (save-excursion (search-forward "\n\n" start t)))
                                            "See ")
                                           (t "see "))))
                  (goto-char next)
                  (add-text-properties
                   (match-beginning 1)
                   (or (save-match-data
                         (let ((start1  (match-beginning 1))) ; Don't hide \n after *Note
                           (and (string-match "\n" (match-string 1))  (+ start1 (match-beginning 0)))))
                       (match-end 1))
                   (if other-tag
                       `(display ,other-tag front-sticky nil rear-nonsticky t)
                     '(invisible t front-sticky nil rear-nonsticky t))))
                (add-text-properties
                 (match-beginning 2) (match-end 2)
                 (list 'help-echo (if (or (match-end 5)  (not (equal (match-string 4) "")))
                                      (concat "mouse-2: go to " (or (match-string 5)  (match-string 4)))
                                    "mouse-2: go to this node")
                       'mouse-face 'highlight)))
              (when (or not-fontified-p  fontify-bookmarked-p  fontify-visited-p)
                (setq rbeg  (match-beginning 2)
                      rend  (match-end 2))
                (let (node)
                  (put-text-property
                   rbeg
                   rend
                   'font-lock-face
                   (if (and (or Info-fontify-visited-nodes  fontify-bookmarked-p)
                            (save-match-data
                              (setq node  (replace-regexp-in-string
                                           "^[ \t]+" ""
                                           (replace-regexp-in-string
                                            "[ \t\n]+" " "
                                            (or (match-string-no-properties 5)
                                                (and (not (equal (match-string 4) ""))
                                                     (match-string-no-properties 4))
                                                (match-string-no-properties 2)))))
                              (let* ((external-link-p  (string-match "(\\([^)]+\\))\\([^)]*\\)" node))
                                     (file             (if external-link-p
                                                           (file-name-nondirectory
                                                            (match-string-no-properties 1 node))
                                                         Info-current-file))
                                     (hl               Info-history-list)
                                     res)
                                (when external-link-p
                                  (setq node  (if (equal (match-string 2 node) "")
                                                  "Top"
                                                (match-string-no-properties 2 node))))
                                (or (and fontify-bookmarked-p  (Info-bookmark-name-for-node node))
                                    (progn
                                      (while hl
                                        (if (and (string-equal node (nth 1 (car hl)))
                                                 (equal file (if (and external-link-p  (stringp (caar hl)))
                                                                 (file-name-nondirectory (caar hl))
                                                               (caar hl))))
                                            (setq res  (car hl)
                                                  hl   nil)
                                          (setq hl  (cdr hl))))
                                      res)))))
                       (let ((bmk  (and fontify-bookmarked-p  (Info-bookmark-for-node node 'LOCALP))))
                         (if bmk
                             (or (bmkp-get-tag-value bmk "bmkp-info-face")  'info-xref-bookmarked)
                           'info-xref-visited))
                     'info-xref)))
                (save-excursion ; For multiline ref, unfontify newline and surrounding whitespace
                  (goto-char rbeg)
                  (save-match-data (while (re-search-forward "\\s-*\n\\s-*" rend t nil)
                                     (remove-text-properties (match-beginning 0) (match-end 0)
                                                             '(font-lock-face t))))))
              (when not-fontified-p
                (when (memq Info-hide-note-references '(t hide))
                  (add-text-properties (match-beginning 3) (match-end 3)
                                       '(invisible t front-sticky nil rear-nonsticky t))
                  ;; Unhide the file name of the external reference in parens
                  (when (and (match-string 6)  (not (eq Info-hide-note-references 'hide)))
                    (remove-text-properties (match-beginning 6) (match-end 6)
                                            '(invisible t front-sticky nil rear-nonsticky t)))
                  (save-match-data ; Unhide newline because hidden newlines cause too long lines
                    (let ((beg3  (match-beginning 3))
                          (end3  (match-end 3)))
                      (when (and (string-match "\n[ \t]*" (match-string 3))
                                 (not (save-match-data (save-excursion (goto-char (1+ end3))
                                                                       (looking-at "[.)]*$")))))
                        (remove-text-properties (+ beg3 (match-beginning 0)) (+ beg3 (match-end 0))
                                                '(invisible t front-sticky nil rear-nonsticky t))))))
                (when (and Info-refill-paragraphs  Info-hide-note-references)
                  (push (set-marker (make-marker) start) paragraph-markers))))))

        ;; REFILL PARAGRAPHS (experimental feature)
        (when (and not-fontified-p  Info-refill-paragraphs  paragraph-markers)
          (let ((fill-nobreak-invisible          t)
                (fill-individual-varying-indent  nil)
                (paragraph-start                 "\f\\|[ \t]*[-*]\\|[ \t]*$")
                (paragraph-separate              ".*\\.[ \t]*\n[ \t]\\|[ \t]*[-*]\\|[ \t\f]*$")
                (adaptive-fill-mode              nil))
            (goto-char (point-max))
            (dolist (m  paragraph-markers)
              (when (< m (point))
                (goto-char m)
                (beginning-of-line)
                (let ((beg  (point)))
                  (when (zerop (forward-paragraph))
                    (fill-individual-paragraphs beg (point) nil nil)
                    (goto-char beg))))
              (set-marker m nil))))

        ;; Fontify MENU ITEMS
        (goto-char (point-min))
        (when (and (or not-fontified-p  fontify-bookmarked-p  fontify-visited-p)
                   (search-forward "\n* Menu:" nil t)
                   Info-fontify-maximum-menu-size ; Don't take time to annotate huge menus
                   (or (eq t Info-fontify-maximum-menu-size)
                       (< (- (point-max) (point)) Info-fontify-maximum-menu-size)))
          (let ((n  0)
                cont)
            (while (re-search-forward (concat "^\\* Menu:\\|\\(?:^\\* +\\(" Info-menu-entry-name-re "\\)\\(:"
                                              Info-node-spec-re "\\([ \t]*\\)\\)\\)")
                                      nil t)
              (when (match-beginning 1)
                (when not-fontified-p
                  (setq n  (1+ n))
                  (when (and (<= n 9)  (zerop (% n 3))) ; visual aids to help with 1-9 keys
                    (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                                       'font-lock-face 'info-menu-star)))
                (when not-fontified-p
                  (add-text-properties (match-beginning 1) (match-end 1)
                                       (list 'help-echo (if (and (match-end 3)
                                                                 (not (equal (match-string 3) "")))
                                                            (concat "mouse-2: go to " (match-string 3))
                                                          "mouse-2: go to this node")
                                             'mouse-face 'highlight)))
                (when (or not-fontified-p  fontify-bookmarked-p  fontify-visited-p)
                  (let (node)
                    (put-text-property
                     (match-beginning 1) (match-end 1)
                     'font-lock-face
                     (if (and (or Info-fontify-visited-nodes  fontify-bookmarked-p)
                              (save-match-data
                                (setq node  (if (equal (match-string 3) "")
                                                (match-string-no-properties 1)
                                              (match-string-no-properties 3)))
                                (let* ((external-link-p  (string-match "(\\([^)]+\\))\\([^)]*\\)" node))
                                       (file             (if external-link-p
                                                             (file-name-nondirectory
                                                              (match-string-no-properties 1 node))
                                                           Info-current-file))
                                       (hl               Info-history-list)
                                       res)
                                  (when external-link-p
                                    (setq node  (if (equal (match-string 2 node) "")
                                                    "Top"
                                                  (match-string-no-properties 2 node))))
                                  (or (and fontify-bookmarked-p  (Info-bookmark-name-for-node node))
                                      (progn
                                        (while hl
                                          (if (and (string-equal node (nth 1 (car hl)))
                                                   (equal file (if (and external-link-p  (stringp (caar hl)))
                                                                   (file-name-nondirectory (caar hl))
                                                                 (caar hl))))
                                              (setq res  (car hl)
                                                    hl   nil)
                                            (setq hl  (cdr hl))))
                                        res)))))
                         (let ((bmk  (and fontify-bookmarked-p  (Info-bookmark-for-node node 'LOCALP))))
                           (if bmk
                               (or (bmkp-get-tag-value bmk "bmkp-info-face")  'info-xref-bookmarked)
                             'info-xref-visited))
                       'info-xref))))
                (when (and not-fontified-p
                           (memq Info-hide-note-references '(t hide))
                           (not (Info-index-node)))
                  (put-text-property (match-beginning 2) (1- (match-end 6)) 'invisible t)
                  ;; Unhide the file name in parens
                  (if (and (match-end 4)  (not (eq (char-after (match-end 4)) ?.)))
                      (remove-text-properties (match-beginning 4) (match-end 4)
                                              '(invisible t)))
                  ;; We need a stretchable space like :align-to but with a minimum value.
                  (put-text-property (1- (match-end 6)) (match-end 6) 'display (if (>= 22
                                                                                       (- (match-end 1)
                                                                                          (match-beginning 0)))
                                                                                   '(space :align-to 24)
                                                                                 '(space :width 2)))
                  (setq cont  (looking-at "."))
                  (while (and (= (forward-line 1) 0)  (looking-at "\\([ \t]+\\)[^*\n]"))
                    (put-text-property (match-beginning 1) (1- (match-end 1)) 'invisible t)
                    (put-text-property (1- (match-end 1)) (match-end 1) 'display (if cont
                                                                                     '(space :align-to 26)
                                                                                   '(space :align-to 24)))
                    (setq cont  t)))))))

        ;; Fontify GLOSSARY WORDS
        ;;
        ;; Do this AFTER fontifying menu items and references.
        (goto-char (point-min))
        (forward-line 4)
        (when Info-fontify-glossary-words (Info-fontify-glossary-words))

        ;; Fontify MENU HEADERS
        (goto-char (point-min))
        (when (and not-fontified-p ; Add face `info-menu-header' to any header before a menu entry
                   (re-search-forward "^\\* Menu:" nil t))
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'info-menu-header)
          (while (re-search-forward "\n\n\\([^*\n ].*\\)\n\n?[*]" nil t)
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'info-menu-header)))

        ;; Hide index line numbers
        (goto-char (point-min))
        (when (and not-fontified-p  (Info-index-node))
          (while (re-search-forward "[ \t\n]*(line +[0-9]+)" nil t)
            (put-text-property (match-beginning 0) (match-end 0) 'invisible t)))

        ;; Fontify HTTP AND FTP REFERENCES
        (goto-char (point-min))
        (when not-fontified-p
          (while (re-search-forward "\\(https?\\|ftp\\)://[^ \t\n\"`({<>})']+" nil t)
            (add-text-properties
             (match-beginning 0) (match-end 0)
             '(font-lock-face info-xref mouse-face highlight help-echo "mouse-2: go to this URL"))))

        ;; Hide any empty lines at the end of the node.
        (goto-char (point-max))
        (skip-chars-backward "\n")
        (when (< (1+ (point)) (point-max)) (put-text-property (1+ (point)) (point-max) 'invisible t))
        (set-buffer-modified-p nil))))

  )

(defun Info-fontify-glossary-words ()
  "Fontify words in current node that are defined in the glossary.
The glossary is that of the current manual, if there is one.
Otherwise, it is the glossary specified by option
`Info-glossary-fallbacks-alist', if any.

Do nothing if the current node is `dir' or if the manual has no
`Glossary' node.

Don't fontify anything in an Index.  Don't fontify a word in a
`Glossary', except for occurrences in definitions other than its own."
  (unless (equal "dir"   Info-current-file)
    (let* ((manual        (file-name-sans-extension (file-name-nondirectory Info-current-file)))
           (ht-var        (intern (concat manual "-glossary-hash-table")))
           (words-done    ())
           (gloss-done    Info-glossary-link-history)
           (gloss-node-p  (equal Info-current-node "Glossary"))
           wbeg wend word def show-link-p)
      (unless (and (boundp ht-var)  (hash-table-p (symbol-value ht-var)))
        (setq ht-var  (intern (concat (info-fallback-manual-for-glossary manual) "-glossary-hash-table"))))
      (when (and (boundp ht-var)
                 (hash-table-p (symbol-value ht-var)) ; Just to be safe.
                 (not (string-match-p "\\bIndex\\b" Info-current-node)))
        (while (and (not (eobp))  (forward-word))
          (setq wend  (prog1 (point) (save-excursion (backward-word) (setq wbeg  (point))))
                word  (buffer-substring wbeg wend))
          (unless (or (Info--member-string-nocase word words-done) ; Fontify only the first occurrence in node.
                      ;; Don't fontify an existing link.
                      (let ((face  (get-text-property 0 'font-lock-face word)))
                        (and face  (memq face Info-link-faces))))
            (when (and (setq def  (gethash word (symbol-value ht-var))) ; WORD is defined in the glossary.
                       (or (not gloss-node-p)
                           (let ((case-fold-search  t)) ; Node is `Glossary'.  Fontify WORD only if in diff def.
                             (save-excursion
                               (forward-line 0)
                               (when (looking-at-p "[ ]+") ; Inside a definition
                                 (while (looking-at-p "[ ]+") (forward-line -1))
                                 (not (string-match-p ; Don't fontify if term being defined matches WORD.
                                       (buffer-substring (line-beginning-position) (line-end-position))
                                       word)))))))
              (setq show-link-p  (and (or (not (memq Info-fontify-glossary-words
                                                     '(face-till-visit-+-mouseover-def
                                                       face-till-visit-+-NO-mouseover-def)))
                                          (not (Info--member-string-nocase word gloss-done)))
                                      (not (memq Info-fontify-glossary-words '(NO-face-+-mouseover-def
                                                                               NO-face-+-NO-mouseover-def))))
                    words-done   (cons word words-done)
                    gloss-done   (cons word gloss-done))
              ;; If using echo area, don't bother to show the mouse-2 info.  If using tooltip then we need
              ;; to show the `link-echo' BEFORE the `def', because for the text to magically change from
              ;; `mouse-2' to `mouse-1' due to `mouse-1-click-follows-link' the text needs to start with
              ;; `mouse-2'.  This is an undocumented "feature".  It's too bad, as it makes it harder to
              ;; see the def.  For a tooltip we can at least add blank lines to separate the two.
              ;;
              ;; Unfortunately, after toggling `tooltip-mode' the change in behavior (adding/removing mouse-2
              ;; info) doesn't take effect till you refontify, e.g. you move to another node.  Not a big deal.
              (let* ((echo  "mouse-2: go to Glossary for this word")
                     (echo  (if (memq Info-fontify-glossary-words '(face-till-visit-+-NO-mouseover-def
                                                                    face-+-NO-mouseover-def
                                                                    NO-face-+-NO-mouseover-def))
                                echo
                              (if tooltip-mode (concat echo "\n\n" def) def))))
                (add-text-properties
                 wbeg wend
                 `(help-echo
                   ,echo
                   ,(and show-link-p  'font-lock-face) ; Add link face only if not yet visited.
                   ,(and show-link-p  'info-glossary-word)
                   mouse-face highlight
                   keymap ,info-glossary-link-map))))))))))

(defun Info-fontify-indented-text ()
  "Fontify text indented `Info-fontify-indented-text-chars' or more.
It is fontified using face `info-indented-text'."
  (let ((num  (abs Info-fontify-indented-text-chars)))
    (while (re-search-forward (format "^ \\{%d,\\}.*" num) nil 'NOERROR)
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'info-indented-text))))

(if (> emacs-major-version 23) ; Emacs < 24 `cl-member' doesn't accept `:test'.  Just use dumb recursion.
    (defun Info--member-string-nocase (string list)
      "`cl-member' with `:test' as `Info-case-insensitive-string='."
      (cl-member string list :test #'Info-case-insensitive-string=))      
  (defun Info--member-string-nocase (string list)
    "`cl-member' with `:test' as `Info-case-insensitive-string='.
But usable with Emacs < 24 too."
    (let ((lst  list))
      (and list  (if (Info-case-insensitive-string= string (car lst))
                     lst
                   (Info--member-string-nocase string (cdr lst)))))))

;;;###autoload (autoload 'Info-goto-glossary-definition "info+")
(defun Info-goto-glossary-definition (&optional event)
  "Go to definition of indicated glossary word.
By default, this is bound to `RET', `mouse-2', and (if
`mouse-1-click-follows-link' is non-nil) `mouse-1'."
  (interactive (list last-nonmenu-event))
  (goto-char (posn-point (event-start event)))
  (let ((word  (word-at-point)))
    (ignore-errors
      (Info-goto-node (format "(%s)Glossary" (info-fallback-manual-for-glossary
                                              (file-name-sans-extension
                                               (file-name-nondirectory Info-current-file)))))
      (goto-char (point-min))
      (forward-line 4)
      (let ((case-fold-search  t)) (re-search-forward (format "^%s$" word) nil t))
      (add-to-list 'Info-glossary-link-history word))))

;;;###autoload (autoload 'Info-glossary "info+")
(defun Info-glossary (term)
  "Look up a string TERM in glossary for this manual, and go to it.
If there are no exact matches for TERM, choose the first term that has
TERM as a case-insensitive substring.
Use an empty TERM name to go to the `Glossary' node itself."
  (interactive
   (progn
     (unless (derived-mode-p 'Info-mode) (info-user-error "You must be in Info to use this command"))
     (when (equal Info-current-file "dir")
       (info-user-error "The Info directory node has no glossary; use `m' to select a manual"))
     (list (let ((completion-ignore-case  t))
             (condition-case nil
                 (completing-read
                  "Glossary term: "
                  (eval (intern (concat (file-name-sans-extension (file-name-nondirectory Info-current-file))
                                        "-glossary-hash-table"))))
               (error (error "Manual `%s' has no glossary"
                             (file-name-sans-extension (file-name-nondirectory Info-current-file)))))))))
  (condition-case nil
      (Info-goto-node "Glossary")
    (error (error "Manual `%s' has no glossary"
                  (file-name-sans-extension (file-name-nondirectory Info-current-file)))))
  (goto-char (point-min))
  (forward-line 4)
  (let ((case-fold-search  t)) (re-search-forward (format "^%s$" term) nil t)))

;;;###autoload (autoload 'Info-set-breadcrumbs-depth "info+")
(defun Info-set-breadcrumbs-depth ()
  "Set current breadcrumbs depth.
Update breadcrumbs display in mode line accordingly.
You are prompted for the depth value."
  (interactive)
  (setq Info-breadcrumbs-depth-internal  (read-number "New breadcrumbs depth: "
                                                      Info-breadcrumbs-depth-internal))
  (when Info-breadcrumbs-in-mode-line-mode (Info-insert-breadcrumbs-in-mode-line)))

(defun Info-fontify-quotations ()
  "Fontify ‘...’, `...', “...”, \"...\", <...>, and isolated ' and `.
Fontify <...> only if `Info-fontify-angle-bracketed-flag'.
Fontify isolated ' and ` only if `Info-fontify-isolated-quote-flag'.

This respects option `Info-fontify-quotations'.

 ‘...’, `...', and <...> use face `info-quoted-name'.
 “...”\t uses face `info-double-quoted-name'.
 \"...\"\t uses face `info-string'.
 '\t uses face `info-single-quote'."
  (let ((regexp    (if (eq Info-fontify-quotations 'multiline)
                       (if Info-fontify-angle-bracketed-flag
                           (info-quoted+<>-regexp)
                         (info-quotation-regexp))
                     (if Info-fontify-angle-bracketed-flag
                         (info-quoted+<>-same-line-regexp)
                       (info-quotation-same-line-regexp))))
        (property  'font-lock-face))
    (unless (eq info-nomatch regexp)
      (while (ignore-errors (re-search-forward regexp nil t))
        (cond ((and (eq (aref (match-string 0) 0) ?`) ; Single-quote wrapped backslashes: `\', `\\', `\\\', etc.
                    (goto-char (match-beginning 0))
                    (save-match-data (looking-at "\\(`\\\\+'\\)")))
               (put-text-property (1+ (match-beginning 0)) (1- (match-end 0)) property 'info-quoted-name)
               (goto-char (match-end 0)))
              ((and (eq (aref (match-string 0) 0) ?‘) ; Single-quote wrapped backslashes:
                    (goto-char (match-beginning 0)) ; ‘\’, ‘\\’, ‘\\\’, etc. 
                    (save-match-data (looking-at "\\(‘\\\\+’\\)")))
               (put-text-property (1+ (match-beginning 0)) (1- (match-end 0)) property 'info-quoted-name)
               (goto-char (match-end 0)))
              ((and (memq (aref (match-string 0) 0) '(?` ?‘)) ; ‘...’, `...'
                    (goto-char (match-beginning 0)) ; If ` or ‘ is preceded by \, then skip it
                    (< (save-excursion (skip-chars-backward "\\\\")) 0))
               (goto-char (1+ (match-beginning 0))))
              ((and Info-fontify-angle-bracketed-flag
                    (eq ?< (aref (match-string 0) 0)) ; <...>: If < is preceded by \, then skip it
                    (goto-char (match-beginning 0))
                    (< (save-excursion (skip-chars-backward "\\\\")) 0))
               (goto-char (1+ (match-beginning 0))))
              ((memq (aref (match-string 0) 0) '(?` ?‘)) ; ‘...’, `...'
               (put-text-property (1+ (match-beginning 0)) (1- (match-end 0)) property 'info-quoted-name)
               (goto-char (match-end 0)) (forward-char 1))
              ((and Info-fontify-angle-bracketed-flag
                    (eq ?< (aref (match-string 0) 0))) ; <...>
               (put-text-property (1+ (match-beginning 0)) (1- (match-end 0)) property 'info-quoted-name)
               (goto-char (match-end 0)) (forward-char 1))
              ((eq (aref (match-string 0) 0) ?“) ; “...”
               (put-text-property (1+ (match-beginning 0)) (1- (match-end 0)) property 'info-double-quoted-name)
               (goto-char (match-end 0)) (forward-char 1))
              ;; Don't try to handle strings correctly.  Check only the first " for being escaped.
              ;; The second " ends the match, even if it is escaped (odd number of \s before it).
              ((and (goto-char (match-beginning 0)) ; "...": If " preceded by \, then skip it
                    (< (save-excursion (skip-chars-backward "\\\\")) 0))
               (goto-char (1+ (match-beginning 0))))
              ((and (not (string= "'" (buffer-substring (match-beginning 0) (match-end 0)))) ; "..."
                    (not (string= "’" (buffer-substring (match-beginning 0) (match-end 0)))))
               (put-text-property (match-beginning 0) (match-end 0) property 'info-string)
               (goto-char (match-end 0)) (forward-char 1))
              (t
               (goto-char (match-end 0)) (forward-char 1)))))
    (when Info-fontify-isolated-quote-flag
      (goto-char (point-min))
      (let ((regexp  (info-isolated-quote-regexp)))
        (unless (eq info-nomatch regexp)
          (while (ignore-errors (re-search-forward regexp nil t))
            (put-text-property (1- (match-end 0)) (match-end 0) property 'info-isolated-quote)
            (goto-char (match-end 0)) (forward-char 1))))
      (goto-char (point-min))
      (let ((regexp  (info-isolated-backquote-regexp)))
        (unless (eq info-nomatch regexp)
          (while (ignore-errors (re-search-forward regexp nil t))
            (put-text-property (match-beginning 0) (1+ (match-beginning 0)) property 'info-isolated-backquote)
            (goto-char (match-end 0)) (forward-char 1)))))))

(defun Info-fontify-custom-delimited ()
  "Fontify text between custom delimiters."
  (let ((regexp  (info-custom-delimited-same-line-regexp)))
    (unless (eq info-nomatch regexp)
      (while (ignore-errors (re-search-forward regexp nil t))
        (put-text-property (1+ (match-beginning 0)) (1- (match-end 0)) 'font-lock-face 'info-custom-delimited)
        (goto-char (match-end 0)) (forward-char 1)))))

(defun Info-fontify-reference-items ()
  "Fontify reference items such as \"Function:\" in Info buffer."
  (while (re-search-forward "^ --? \\(Command:\\|Constant:\\|Function:\\|Macro:\\|Special Form:\\|\
Syntax class:\\|User Option:\\|Variable:\\)\\(.*\\)\\(\n          \\(.*\\)\\)*"
                            nil t)
    (let ((symb  (intern (match-string 1))))
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face (case symb
                                           ('Constant:       'info-constant-ref-item)
                                           ('Command:        'info-command-ref-item)
                                           ('Function:       'info-function-ref-item)
                                           ('Macro:          'info-macro-ref-item)
                                           ('Special\ Form:  'info-special-form-ref-item)
                                           ('Syntax\ class:  'info-syntax-class-item)
                                           ('User\ Option:   'info-user-option-ref-item)
                                           ('Variable:       'info-variable-ref-item)))
      (put-text-property (match-beginning 2) (match-end 2)
                         'font-lock-face 'info-reference-item)
      (when (match-beginning 4)
        (put-text-property (match-beginning 4) (match-end 4)
                           'font-lock-face 'info-reference-item)))))


(defun Info-search-beg () ;; `isearchp-reg-beg' is defined in library `isearch+.el' for Emacs 24.3+.
  "`isearchp-reg-beg', if defined and non-nil; else `point-min'."
  (or (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg)  (point-min)))

(defun Info-search-end () ;; `isearchp-reg-end' is defined in library `isearch+.el' for Emacs 24.3+.
  "`isearchp-reg-end', if defined and non-nil; else `point-max'."
  (or (and (boundp 'isearchp-reg-end)  isearchp-reg-end)  (point-max)))

(defun Info-isearch-search-p ()
  "Return non-nil if isearch in Info searches through multiple nodes.
\(Returns nil if search is restricted to the active region.)"
  (and Info-isearch-search  (or (not (boundp 'isearchp-reg-beg))  (not isearchp-reg-beg))))


;; REPLACES ORIGINAL in `info.el':
;;
;; Use `Info-isearch-search-p', not var `Info-isearch-search'.
;;
(defun Info-isearch-wrap ()
  (if (not (Info-isearch-search-p))
      (goto-char (if isearch-forward (Info-search-beg) (Info-search-end)))
    (if (not Info-isearch-initial-node)
        (setq Info-isearch-initial-node  Info-current-node
              isearch-wrapped            nil)
      (if isearch-forward (Info-top-node) (Info-final-node))
      (goto-char (if isearch-forward (Info-search-beg) (Info-search-end))))))


;; REPLACES ORIGINAL in `info.el':
;;
;; Use `Info-isearch-search-p', not var `Info-isearch-search'.
;;
(when (= emacs-major-version 23)        ; Emacs 23 only
  (defun Info-isearch-search () ; Use `Info-isearch-search-p', not var `Info-isearch-search'.
    (if (Info-isearch-search-p)
        (lambda (string &optional bound noerror count)
          (if isearch-word
              (Info-search (concat "\\b" (replace-regexp-in-string "\\W+" "\\W+"
                                                                   (replace-regexp-in-string
                                                                    "^\\W+\\|\\W+$" "" string)
                                                                   nil t)
                                   (and (or isearch-nonincremental ; Lax version of word search
                                            (eq (length string) (length (isearch-string-state
                                                                         (car isearch-cmds)))))
                                        "\\b"))
                           bound noerror count (unless isearch-forward 'backward))
            (Info-search (if isearch-regexp string (regexp-quote string))
                         bound noerror count (unless isearch-forward 'backward)))
          (point))
      (let ((isearch-search-fun-function  nil)) (isearch-search-fun)))))


(when (> emacs-major-version 23)        ; Emacs 24+

  (unless (boundp 'isearch-lax-whitespace) ; Emacs 24.1, 24.2.
    
    (defvar isearch-lax-whitespace t
      "If non-nil, a space will match a sequence of whitespace chars.
When you enter a space or spaces in ordinary incremental search, it
will match any sequence matched by the regexp defined by the variable
`search-whitespace-regexp'.  If the value is nil, each space you type
matches literally, against one space.  You can toggle the value of this
variable by the command `isearch-toggle-lax-whitespace'.")

    (defvar isearch-regexp-lax-whitespace nil
      "If non-nil, a space will match a sequence of whitespace chars.
When you enter a space or spaces in regexp incremental search, it
will match any sequence matched by the regexp defined by the variable
`search-whitespace-regexp'.  If the value is nil, each space you type
matches literally, against one space.  You can toggle the value of this
variable by the command `isearch-toggle-lax-whitespace'.")

    )



  ;; REPLACES ORIGINAL in `info.el':
  ;;
  ;; 1. Use function `Info-isearch-search-p', not variable `Info-isearch-search'.
  ;; 2. 
  ;;
  (defun Info-isearch-search ()
    (if (Info-isearch-search-p)
        (lambda (string &optional bound noerror count)
	  (let ((isearch-symbols-fn  (if (boundp 'isearch-regexp-function)
                                         isearch-regexp-function ; Emacs 25+
                                       isearch-word)) ; Emacs 24
                (Info-search-whitespace-regexp
	         (if (if isearch-regexp
		         isearch-regexp-lax-whitespace
		       isearch-lax-whitespace)
		     search-whitespace-regexp)))
	    (Info-search
	     (cond (isearch-symbols-fn
                    ;; Lax version of word search
	            (let ((lax  (if (fboundp 'isearch--lax-regexp-function-p)
                                    (and (not bound)  (isearch--lax-regexp-function-p)) ; Emacs 25+
                                  (not (or isearch-nonincremental ; Emacs 24
                                           (eq (length string)
                                               (length (isearch--state-string (car isearch-cmds)))))))))
	              (when lax (setq isearch-adjusted  t))
	              (if (functionp isearch-symbols-fn)
		          (funcall isearch-symbols-fn string lax)
		        (word-search-regexp string lax))))
	           (isearch-regexp string)
	           (t (regexp-quote string)))
	     bound noerror count (unless isearch-forward 'backward)))
	  (point))
      (isearch-search-fun-default)))

  )

(defun info--user-search-failed ()      ; Needed for Emacs < 24, which doesn't have `user-search-failed'.
  "`user-search-failed', if `user-error' is defined.  Else `search-failed'."
  (if (fboundp 'user-error) 'user-search-failed 'search-failed))

;; REPLACES ORIGINAL in `info.el':
;;
;; 1. Fit frame if `one-window-p'.
;; 2. Highlight the found regexp if `search-highlight'.
;; 3. If `isearch+.el' is loaded, search only the active region if `isearchp-restrict-to-region-flag' is non-nil.
;; 4. Deactivate mark only if search moves to a different node or `isearchp-deactivate-region-flag' is non-nil.
;; 5. Use `info--user-search-failed' for compatibility with older Emacs.
;;
;;;###autoload (autoload 'Info-search "info+")
(defun Info-search (regexp &optional bound _noerror _count direction)
  "Search for REGEXP, starting from point, and select node of search hit.
If DIRECTION is `backward', search backward.
Fits frame if `one-window-p'.
Highlights current location of found regexp if `search-highlight'.

If called interactively then this is a non-incremental search.  In
that case, the search-hit highlighting remains, after the search is
over.  To remove the highlighting, just start an incremental search:
`\\[isearch-forward]'."
  (interactive
   (list (let ((prompt  (if Info-search-history
                            (format "Regexp search%s (default `%s'): "
                                    (if case-fold-search "" " case-sensitively")
                                    (car Info-search-history))
                          (format "Regexp search%s: "
                                  (if case-fold-search "" " case-sensitively")))))
           (if (fboundp 'icicle-read-string-completing)
               (icicle-read-string-completing prompt nil nil 'Info-search-history)
             (read-string prompt nil 'Info-search-history)))))
  ;; Deactivate mark later, and only if moved to new node or `isearchp-deactivate-region-flag' is non-nil.
  ;; Emacs bug #45839 now also applies this fix.
  ;; (deactivate-mark)
  (when (equal regexp "") (setq regexp  (car Info-search-history)))
  (when regexp
    (prog1
        (let* ((backward                          (eq direction 'backward))
               (onode                             Info-current-node)
               (ofile                             Info-current-file)
               (opoint                            (point))
               (opoint-min                        (Info-search-beg))
               (opoint-max                        (Info-search-end))
               (ostart                            (window-start))
               (osubfile                          Info-current-subfile)
               (found                             (save-excursion
                                                    (save-restriction
                                                      (widen)
                                                      (Info--search-loop regexp bound backward))))
               (isearchp-restrict-to-region-flag  (not Info-isearch-search))) ; For `isearch+.el'.

          (setq Info-search-case-fold  case-fold-search)
          (unless (or (not isearch-mode)  (not (Info-isearch-search))
                      Info-isearch-initial-node ; `Info-isearch-initial-node' is free here.
                      bound
                      (and found  (> found opoint-min)  (< found opoint-max)))
	    (signal (info--user-search-failed) (list regexp "(end of node)")))

          (unless (or found  Info-current-subfile) ; If no subfiles, give error now.
            (if isearch-mode
                (signal (info--user-search-failed) (list regexp "end of manual"))
              (let ((search-spaces-regexp  Info-search-whitespace-regexp))
                (unless (if backward
                            (re-search-backward regexp nil t)
                          (re-search-forward regexp nil t))
                  (signal (info--user-search-failed) (list regexp))))))
          (when (and bound  (not found)) (signal (info--user-search-failed) (list regexp)))
          (unless (or found  bound)
	    (unwind-protect
	        (let ((list  ()))       ; Try other subfiles.
	          (with-current-buffer (marker-buffer Info-tag-table-marker)
		    (goto-char (Info-search-beg))
		    (search-forward "\n\^_\nIndirect:")
		    (save-restriction
		      (narrow-to-region (point) (progn (search-forward "\n\^_") (1- (point))))
		      (goto-char (Info-search-beg))
		      (search-forward (concat "\n" osubfile ": ")) ; Find the subfile we just searched.
		      (forward-line (if backward 0 1)) ; Skip that one.
		      (if backward (forward-char -1))
                      ;; Make a list of all following subfiles.
                      ;; Each elt has the form (VIRT-POSITION . SUBFILENAME).
		      (while (not (if backward (bobp) (eobp)))
		        (if backward
		            (re-search-backward "\\(^.*\\): [0-9]+$")
		          (re-search-forward "\\(^.*\\): [0-9]+$"))
		        (goto-char (+ (match-end 1) 2))
                        (setq list  (cons (cons (+ (Info-search-beg) (read (current-buffer)))
                                                (match-string-no-properties 1))
                                          list))
		        (goto-char (if backward (1- (match-beginning 0)) (1+ (match-end 0)))))
		      (setq list  (nreverse list)))) ; Put in forward order.
	          (while list
		    (when (interactive-p) (message "Searching subfile %s..." (cdar list)))
		    (Info-read-subfile (caar list))
		    (when backward
                      (goto-char (Info-search-end)))
		    (setq list   (cdr list)
                          found  (Info--search-loop regexp nil backward))
		    (when found (setq list  ())))
	          (if found
		      (message "")
                    (signal (info--user-search-failed) `(,regexp ,@(and isearch-mode  '("end of manual"))))))
	      (unless found
                (Info-read-subfile osubfile)
                (goto-char opoint)
                (Info-select-node)
                (set-window-start (selected-window) ostart))))
          (if (and (string= osubfile Info-current-subfile)  (> found opoint-min)  (< found opoint-max))
              (goto-char found)      ; Search landed in the same node.
            (widen)
            (goto-char found)
            (save-match-data (Info-select-node)))
          ;; Deactivate only if moved to new node or `isearchp-deactivate-region-flag' is undefined or non-nil.
          ;; Emacs bug #45839 now also applies this fix, but without the Isearch+ possibility.
          (unless (and (boundp 'isearchp-deactivate-region-flag)
                       (not isearchp-deactivate-region-flag)
                       (string-equal onode Info-current-node))
            (deactivate-mark))
          ;; Highlight the regexp match.  If `isearch+.el' is loaded this can highlight subgroups.
          (when search-highlight (isearch-highlight (match-beginning 0) (match-end 0))) ; Highlight regexp.
          ;; Use `string-equal', not `equal', to ignore text props.
          (or (and (string-equal onode Info-current-node)  (equal ofile Info-current-file))
              (and isearch-mode  isearch-wrapped  (eq opoint (if isearch-forward opoint-min opoint-max)))
	      (setq Info-history  (cons (list ofile onode opoint) Info-history)))
          (when (and (one-window-p t)  (not (window-minibuffer-p)) ; Fit the frame, if appropriate.
                     (fboundp 'fit-frame) ; Defined in `fit-frame.el'.
                     Info-fit-frame-flag)
            (fit-frame)))
      (when (and (interactive-p)  (not isearch-mode))
        (message (substitute-command-keys
                  "Use \\<Info-mode-map>`\\[Info-search] RET' to search again for `%s'.")
                 regexp)))))


;; REPLACES ORIGINAL in `info.el', Emacs 24+:
;;
;; Use `Info-search-beg' and `Info-search-end', not `point-min' and `point-max'.
;;
(defun Info--search-loop (regexp bound backward)
  (when backward
    (narrow-to-region (save-excursion ; Hide Info file header for backward search.
                        (goto-char (Info-search-beg))
                        (search-forward "\n\^_")
                        (1- (point)))
                      (Info-search-end)))
  (let ((give-up    nil)
        (found      nil)
        (beg-found  nil))
    (while (not (or give-up  (and found  (funcall isearch-filter-predicate beg-found found))))
      (let ((search-spaces-regexp  Info-search-whitespace-regexp))
        (if (funcall (if backward #'re-search-backward #'re-search-forward) regexp bound t)
            (setq found      (point)
                  beg-found  (if backward (match-end 0) (match-beginning 0)))
          (setq give-up  t
                found    nil))))
    found))

(defun Info-search-case-sensitively-next ()
  "Search for next regexp from a previous `Info-search-case-sensitively'."
  (interactive)
  (let ((case-fold-search  Info-search-case-fold))
    (if Info-search-history
        (Info-search (car Info-search-history))
      (call-interactively #'Info-search-case-sensitively))))


;; REPLACES ORIGINAL in `info.el':
;; Added optional arg FORK.
;;
(defun Info-next (&optional fork)       ; `n'
  "Go to the next node of this node.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive "P")
  (unless (derived-mode-p 'Info-mode) (Info--pop-to-buffer-same-window "*info*"))
  (Info-goto-node (Info-extract-pointer "next") fork))


;; REPLACES ORIGINAL in `info.el':
;; Added optional arg FORK.
;;
(defun Info-prev (&optional fork)       ; `p'
  "Go to the previous node of this node.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive "P")
  (unless (derived-mode-p 'Info-mode) (Info--pop-to-buffer-same-window "*info*"))
  (Info-goto-node (Info-extract-pointer "prev[ious]*" "previous") fork))


;; REPLACES ORIGINAL in `info.el':
;; Added optional arg FORK.
;;
(defun Info-up (&optional same-file fork) ; `u', `^'
  "Go to the superior node of this node.
If SAME-FILE is non-nil, do not move to a different Info file.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive "i\nP")
  (unless (derived-mode-p 'Info-mode) (Info--pop-to-buffer-same-window "*info*"))
  (let ((old-node  Info-current-node)
        (old-file  Info-current-file)
        (node      (Info-extract-pointer "up"))
        pp)
    (when (and same-file  (string-match "^(" node)) (error "Up node is in another Info file"))
    (Info-goto-node node fork)
    (setq pp  (point))
    (goto-char (point-min))
    (cond ((and (stringp old-file)
                (search-forward "\n* Menu:" nil t)
                (re-search-forward
                 (if (string-equal old-node "Top")
                     (concat "\n\\*[^:]+: +(" (file-name-nondirectory old-file) ")")
                   (concat "\n\\* +\\(" (regexp-quote old-node)
                           ":\\|[^:]+: +" (regexp-quote old-node) "\\)"))
                 nil t))
           (beginning-of-line)
           (if (looking-at "^\\* ") (forward-char 2)))
          (t
           (goto-char pp)
           (Info-restore-point Info-history)))))


;; REPLACES ORIGINAL in `info.el':
;; Added optional arg FORK.
;;
(defun Info-history-back (&optional fork) ; `l'
  "Go back in the history to the last node visited.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive "P")
  (unless Info-history
    (funcall (if (fboundp 'user-error) #'user-error #'error)
             "This is the first Info node you have visited"))
  (let ((history-forward  (cons (list Info-current-file Info-current-node (point))
                                Info-history-forward))
        filename nodename opoint)
    (setq filename      (caar Info-history)
          nodename      (car (cdar Info-history))
          opoint        (cadr (cdar Info-history))
          Info-history  (cdr Info-history))
    (when fork
      (set-buffer (clone-buffer (concat "*info-" (if (stringp fork) fork nodename) "*") t)))
    (Info-find-node filename nodename)
    (setq Info-history          (cdr Info-history)
          Info-history-forward  history-forward)
    (goto-char opoint)))


;; REPLACES ORIGINAL in `info.el':
;; Added optional arg FORK.
;;
(defun Info-history-forward (&optional fork) ; `r'
  "Go forward in the history of visited nodes.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive "P")
  (unless Info-history-forward 
    (funcall (if (fboundp 'user-error) #'user-error #'error)
             "This is the last Info node you have visited"))
  (let ((history-forward  (cdr Info-history-forward))
        filename nodename opoint)
    (setq filename  (caar Info-history-forward)
          nodename  (car (cdar Info-history-forward))
          opoint    (cadr (cdar Info-history-forward)))
    (when fork
      (set-buffer (clone-buffer (concat "*info-" (if (stringp fork) fork nodename) "*") t)))
    (Info-find-node filename nodename)
    (setq Info-history-forward  history-forward)
    (goto-char opoint)))


;; REPLACES ORIGINAL in `info.el':
;; Added optional arg FORK.
;;
(defun Info-directory (&optional fork)  ; `d'
  "Go to the Info directory node.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive "P")
  (when fork (set-buffer (clone-buffer nil t)))
  (Info-find-node "dir" "top"))


;; REPLACES ORIGINAL in `info.el':
;; Added optional arg FORK.
;;
(defun Info-forward-node (&optional not-down not-up no-error fork) ; `]'
  "Go forward one node, considering all nodes as forming one sequence.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive "i\ni\ni\nP")
  (goto-char (point-min))
  (forward-line 1)
  (let ((case-fold-search  t))
    ;; Three possibilities, in order of priority:
    ;;     1. Next node is in a menu in this node (but not in an index)
    ;;     2. Next node is next at same level
    ;;     3. Next node is up and next
    (cond ((and (not not-down)
                (save-excursion (search-forward "\n* menu:" nil t))
                (not (Info-index-node)))
           (Info-goto-node (Info-extract-menu-counting 1) fork)
           t)
          ((save-excursion (search-backward "next:" nil t))
           (Info-next fork)
           t)
          ((and (not not-up)
                (save-excursion (search-backward "up:" nil t))
                ;; Use `string-equal', not `equal', to ignore text properties.
                (not (string-equal (downcase (Info-extract-pointer "up")) "top")))
           (let ((old-node  Info-current-node))
             (Info-up)
             (let ((old-history  Info-history)
                   success)
               (unwind-protect
                    (setq success  (Info-forward-node t nil no-error fork))
                 (or success  (Info-goto-node old-node fork)))
               (and (boundp 'Info-history-skip-intermediate-nodes) ; Emacs 24+
                    Info-history-skip-intermediate-nodes
                    (setq Info-history  old-history)))))
          (no-error nil)
          (t (user-error "No pointer forward from this node")))))


;; REPLACES ORIGINAL in `info.el':
;; Added optional arg FORK.
;;
(defun Info-backward-node (&optional fork) ; `['
  "Go backward one node, considering all nodes as forming one sequence.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive "P")
  (let ((prevnode          (Info-extract-pointer "prev[ious]*" t))
        (upnode            (Info-extract-pointer "up" t))
        (case-fold-search  t))
    (cond ((and upnode  (string-match "(" upnode)) (user-error "First node in file"))
          ((and upnode  (or (null prevnode)
                            ;; Use `string-equal', not `equal', to ignore text properties.
                            (string-equal (downcase prevnode) (downcase upnode))))
           (Info-up fork))
          (prevnode
           ;; If we move back at the same level, go down to find the last subnode*.
           (Info-prev fork)
           (let ((old-history  Info-history)
                 node)
             (while (and (not (Info-index-node))
                         (save-excursion (search-forward "\n* Menu:" nil t)))
               (Info-goto-node (setq node  (Info-extract-menu-counting nil))))
;;; (when fork (Info-goto-node node fork))
             (and (boundp 'Info-history-skip-intermediate-nodes) ; Emacs 24+
                  Info-history-skip-intermediate-nodes
                  (setq Info-history  old-history))))
          (t (user-error "No pointer backward from this node")))))


;; REPLACES ORIGINAL in `info.el':
;; Added optional arg FORK.
;;
;;;###autoload (autoload 'Info-mouse-follow-nearest-node "info+")
(defun Info-mouse-follow-nearest-node (click &optional fork)
  "\\<Info-mode-map>Follow a node reference near point.
Like \\[Info-menu], \\[Info-follow-reference], \\[Info-next], \\[Info-prev] or \\[Info-up] \
command, depending on where you click.
At end of the node's text, moves to the next node, or up if none.

With a prefix argument, open the node in a separate window."
  (interactive "e\nP")
  (mouse-set-point click)
  (and (not (Info-try-follow-nearest-node fork))
       (save-excursion (forward-line 1) (eobp))
       (Info-next-preorder)))


;; REPLACES ORIGINAL in `info.el':
;; Use `Info-mode-syntax-table' (bug #3312).
;; Doc string changed: displays all bindings.
;;
(define-derived-mode Info-mode nil "Info" ;FIXME: Derive from special-mode?
  "Provide commands for browsing through the Info documentation tree.
Documentation in Info is divided into \"nodes\", each of which discusses
one topic and contains hyperlink references to other nodes that discuss
related topics.  Info has commands to follow the references.
The most important commands to know are: \
\\<Info-mode-map>\
`\\[Info-exit]', `\\[Info-mouse-follow-nearest-node]', `\\[Info-history-back]', and `\\[Info-search]'.

Help commands
-------------
\\[describe-mode]\tDisplay this help.
\\[Info-help]\tThe Info tutorial.  Learn about Info while using it.

Selecting other nodes (basic)
-----------------------------
\\[Info-mouse-follow-nearest-node]\tFollow a node reference you click.
\tThis works with menu items, cross references, \"Next\",
\t\"Previous\" and \"Up\" references.
\tAt end of node's text, goes to \"Next\", or \"Up\" if no \"Next\".
\\[Info-follow-nearest-node]\tLike `\\[Info-mouse-follow-nearest-node]', \
except cursor location, not mouse location.
\\[Info-history-back]\tGo back to the last node you were at. (chronological)
\\[Info-history-forward]\tGo forward to where you were before using \\[Info-history-back].
\\[Info-history]\tGo to menu of visited nodes.
\\[Info-toc]\tGo to table of contents of the current Info file.
\\[Info-index]\tLook up a topic in this file's Index and move to its node.
\\[Info-index-next]\t(comma) Go to the next match from a previous \
`\\[Info-index]' command.

Structural navigation commands
------------------------------
\\[Info-menu]\tGo to a menu item's node.  Completion available for its name.
1\tGo to first menu item's node.
2, 3, 4, 5, 6, 7, 8, 9\tGo to second...ninth menu item's node.
\\[Info-next]\tGo to this node's \"Next\" node.
\\[Info-prev]\tGo to this node's \"Previous\" node.  (*not* chronological)
\\[Info-up]\tGo \"Up\" from this node to its parent node.
\\[Info-directory]\tGo to the Info directory (root) node.
\\[Info-follow-reference]\tFollow a cross reference. Prompts for name.

Moving within a node
--------------------
\\[Info-scroll-up]\tNormally, scroll forward a full screen.
\tIf node's menu appears below cursor, go to first menu item.
\tIf node's menu appears above cursor, go to parent node.
\\[Info-scroll-down]\tNormally, scroll backward.  If beginning of buffer is \
already
\tvisible, go to previous menu entry, or up if there is none.
\\[beginning-of-buffer]\tGo to beginning of node.
\\[Info-next-reference]\tMove cursor to next cross-reference or menu item in \
this node.
\\[Info-prev-reference]\tMove cursor to previous cross-reference or menu item.

Other navigation commands
-------------------------
\\[Info-exit]\tQuit Info.
\\[Info-goto-node]\tGo to a node with a given name.
\tYou may include a filename as well, as \"(FILENAME)NODENAME\".
\\[universal-argument] \\[info]\tGo to a new Info file.  (Completion \
available.)
\\[universal-argument] N \\[info]\tOpen Info with number in buffer name: *info*<N>.
\\[Info-top-node]\tGo to first node (\"Top\") of current Info file.
\\[Info-final-node]\tGo to final node of current Info file.
\\[Info-forward-node]\tGo forward a node, considering all nodes as one \
sequence.
\\[Info-backward-node]\tGo backward a node, considering all nodes as one \
sequence.

Other commands
--------------
\\[Info-save-current-node]\tSave current node name for use by `\\[Info-virtual-book]'.
\\[Info-virtual-book]\tOpen a virtual Info book of nodes saved using `\\[Info-save-current-node]'.
\\[isearch-forward]\tIsearch this Info manual for a literal string.
\\[isearch-forward-regexp]\tIsearch this Info manual for a regexp.
\\[Info-search]\tSearch this Info manual for a regexp
\\[Info-search-case-sensitively]\tLike `\\[Info-search]', but case-sensitive.
\\[info-apropos]\tLook for a string in the indexes of all manuals.
\\[Info-copy-current-node-name]\tPut name of current info node in the kill ring.
\\[clone-buffer]\tSelect a new cloned Info buffer in another window.
\\[Info-merge-subnodes]\tIntegrate current node with nodes referred to \
in its Menu.
\tDisplay the result outside of Info.  `\\[universal-argument]': Recursively.

User options you can customize
------------------------------
`Info-fontify-quotations' -
  Fontify quoted text (‘...’, `...', “...”) and strings (\"...\").
  Cycle with \\[Info-cycle-fontify-quotations].
`Info-fontify-angle-bracketed-flag' -
  Fontify angle-bracketd names (<...>).
  Toggle with \\[Info-toggle-fontify-angle-bracketed].
`Info-fontify-isolated-quote-flag' -
  Fontify isolated quote and backquote (', `).
  Toggle with \\[Info-toggle-fontify-isolated-quote].
`Info-fontify-glossary-words' -
  Fontify and link glossary words.
  Cycle values with \\[Info-cycle-fontify-glossary-words].
  Toggle last value with \\[Info-toggle-fontify-glossary-words].
`Info-fontify-bookmarked-xrefs-flag' -
  Fontify references to bookmarked nodes.
  Toggle with \\[Info-toggle-fontify-bookmarked-xrefs].
  (You need library Bookmark+ for this one.)
`Info-saved-nodes' - Node names you can visit using `\\[Info-virtual-book]'.
`Info-subtree-separator' - See `Info-merge-subnodes'.

Faces you can customize
-----------------------
`info-file'   - Face used for file heading labels
`info-string' - Face used for strings (e.g. \"toto\")
`info-double-quoted-name'
              - Face used for curly double-quoted names (e.g. “toto”)
`info-quoted-name'  - Face for quoted names (e.g. ‘toto’ or `toto')
`info-single-quote' - Face used for isolated single-quote (e.g. 'foo)

These are all of the current Info Mode bindings:

\\{Info-mode-map}"
  :syntax-table Info-mode-syntax-table
  :abbrev-table text-mode-abbrev-table
  (kill-all-local-variables)
  (setq major-mode  'Info-mode
        mode-name   "Info"
        tab-width   8)
  (use-local-map Info-mode-map)
  (add-hook 'activate-menubar-hook 'Info-menu-update nil t)
  (setq case-fold-search  t
        buffer-read-only  t)
  (make-local-variable 'Info-current-file)
  (make-local-variable 'Info-current-subfile)
  (make-local-variable 'Info-current-node)
  (make-local-variable 'Info-tag-table-marker)
  (setq Info-tag-table-marker  (make-marker))
  (make-local-variable 'Info-tag-table-buffer)
  (setq Info-tag-table-buffer  nil)
  (make-local-variable 'Info-history)
  (make-local-variable 'Info-history-forward)
  (make-local-variable 'Info-index-alternatives)
  (unless (or (display-multi-font-p)
              (and (coding-system-equal (coding-system-base (terminal-coding-system)) 'utf-8)
                   ;; The Linux console has limited character repertoire even when its encoding is UTF-8.
                   (not (equal (tty-type) "linux"))))
    (dolist (elt  info-symbols-and-replacements)
      (let ((ch    (car elt))
            (repl  (cdr elt)))
        (or (char-displayable-p ch)
            (aset (or buffer-display-table  (setq buffer-display-table  (make-display-table)))
                  ch
                  (vconcat (mapcar (lambda (cc) (make-glyph-code cc 'homoglyph)) repl)))))))
  (when Info-use-header-line     ; do not override global header lines
    (setq header-line-format  '(:eval (get-text-property (point-min) 'header-line))))
  (set (make-local-variable 'tool-bar-map) info-tool-bar-map)
  ;; This is for the sake of the invisible text we use handling titles.
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible  t)
  (make-local-variable 'desktop-save-buffer)
  (make-local-variable 'widen-automatically)
  (setq widen-automatically  nil) ; `widen-automatically' is free here.
  (setq desktop-save-buffer  'Info-desktop-buffer-misc-data)
  (add-hook 'kill-buffer-hook 'Info-kill-buffer nil t)
  (add-hook 'clone-buffer-hook 'Info-clone-buffer nil t)
  (add-hook 'change-major-mode-hook 'Info-defontify nil t)
  (add-hook 'isearch-mode-hook 'Info-isearch-start nil t)
  ;; The `Info-*' variables are free here.
  (set (make-local-variable 'isearch-search-fun-function) 'Info-isearch-search)
  (set (make-local-variable 'isearch-wrap-function) 'Info-isearch-wrap)
  (set (make-local-variable 'isearch-push-state-function) 'Info-isearch-push-state)
  (set (make-local-variable 'isearch-filter-predicate) 'Info-isearch-filter)
  (unless (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 2)))
    (set (make-local-variable 'search-whitespace-regexp) Info-search-whitespace-regexp))
  (set (make-local-variable 'revert-buffer-function) 'Info-revert-buffer-function)
  (Info-set-mode-line)
  (set (make-local-variable 'bookmark-make-record-function) 'Info-bookmark-make-record)
  (run-mode-hooks 'Info-mode-hook))


;; REPLACES ORIGINAL in `info.el':
;;
;; Respect `Info-bookmark-use-only-node-not-file-flag'.
;;
;; This code and the definition of `Info-bookmark-use-only-node-not-file-flag' are also in `bookmark+-1.el',
;; so that their feature is available if you use either `Info+' or `Bookmark+'.
;;
;; Note: This function name doesn't respect the naming convention for bookmark handler functions.
;;       This name gives the impression that this is a jump command.
;;
;;;###autoload (autoload 'Info-bookmark-jump "info+")
(defun Info-bookmark-jump (bookmark)
  "Handler function for record returned by `Info-bookmark-make-record'.
BOOKMARK is a bookmark name or a bookmark record.

If `Info-bookmark-use-only-node-not-file-flag' is nil, and the
recorded Info file is readable, then use it.  If not, then go to the
recorded Info node in the manual for the current Emacs version."
  (let* ((absfile    (bookmark-prop-get bookmark 'filename))
         (file       (if (or Info-bookmark-use-only-node-not-file-flag  (not (file-readable-p absfile)))
                         (file-name-nondirectory absfile)
                       absfile))
         (info-node  (bookmark-prop-get bookmark 'info-node))
         (buf        (save-window-excursion ; Vanilla FIXME: doesn't work with frames!
                       (Info-find-node file info-node) (current-buffer))))
    (bookmark-default-handler `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))


;; REPLACES ORIGINAL in `info.el':
;;
;; 1. Use completion for inputting the manual name, for all Emacs versions 23+.
;; 2. Use helper, `Info--manuals'.
;;
;;;###autoload (autoload 'info-display-manual "info+")
(defun info-display-manual (manual)
  "Display an Info buffer displaying MANUAL.
If there is an existing Info buffer for MANUAL, display it.
Otherwise, visit the manual in a new Info buffer.

With a prefix arg (Emacs 24.4+), completion candidates are limited to
currently visited manuals."
  (interactive (let* ((_IGNORE  (info-initialize))
                      (manuals  (Info--manuals current-prefix-arg)))
                 (when (fboundp 'info--filter-manual-names) ; Emacs 29, bug #54961.
                   (setq manuals  (info--filter-manual-names manuals)))
                 (list (completing-read "Display manual: " manuals nil t))))
  (let ((blist             (buffer-list))
        (manual-re         (concat "\\(/\\|\\`\\)" manual "\\(\\.\\|\\'\\)"))
        (case-fold-search  t)
        found)
    (dolist (buffer blist)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'Info-mode)
                   (stringp Info-current-file)
                   (string-match manual-re Info-current-file))
          (setq found  buffer
                blist  ()))))
    (if found
        (Info--pop-to-buffer-same-window found)
      (info-initialize)
      (info (Info-find-file manual)))))

;; Emacs 27+ removed `Info-edit-mode'.  Restore it.
;;
(unless (fboundp 'Info-edit-mode)

  (defvar Info-edit-mode-hook nil
    "Hook run when `Info-edit-mode' is activated.")

  (defvar Info-edit-mode-map (let ((map  (make-sparse-keymap)))
                               (set-keymap-parent map text-mode-map)
                               (define-key map "\C-c\C-c" 'Info-cease-edit)
                               map)
    "Local keymap for `Info-edit-mode'.")

  (put 'Info-edit-mode 'mode-class 'special) ; Only for specially formatted data

  (define-derived-mode Info-edit-mode text-mode "Info Edit"
    "Major mode for editing the contents of an Info node.
Like Text mode with the addition of `\\<Info-edit-mode-map>\\[Info-cease-edit]', which
returns to (read-only) Info mode for browsing."
    (setq buffer-read-only nil)
    (force-mode-line-update)
    (buffer-enable-undo (current-buffer)))

  (defun Info-edit ()
    "Edit the contents of this Info node."
    (interactive)
    (Info-edit-mode)
    (message "%s" (substitute-command-keys
		   "EDITING.  Use `\\<Info-edit-mode-map>\\[Info-cease-edit]' to end editing")))

  (put 'Info-edit 'disabled "**BE CAREFUL** if you enable this - you might mess up your Info files.")

  (defun Info-cease-edit ()
    "Finish editing Info node; switch back to (read-only) Info mode."
    (interactive)
    (and (buffer-modified-p) ; Do this first, so nothing changed if user `C-g's at query.
         (y-or-n-p "Save the file? ")
         (save-buffer))
    (Info-mode)
    (force-mode-line-update)
    (and (marker-position Info-tag-table-marker)
         (buffer-modified-p)
         (message "Tags may have changed.  Use `Info-tagify' if necessary")))

  )
 
;;(@* "Non-Interactive Functions")
;;; Non-Interactive  Functions ---------------------------------------

(defun Info-refontify-current-node ()
  "In Info mode, refontify the current node.
This first removes text properties used for fontifying from
the (entire) current Info file (`Info-current-file').
It then fontifies (only) the current node.

The properties removed are those that comprise links:
`font-lock-face', `mouse-face', `help-echo', and `keymap'."
  (when (derived-mode-p 'Info-mode)
    (let ((modp               (buffer-modified-p))
          (inhibit-read-only  t))
      (save-restriction
        (widen)
        (remove-list-of-text-properties (point-min) (point-max)
                                        '(font-lock-face mouse-face help-echo keymap)))
      (Info-fontify-node)
      (restore-buffer-modified-p modp))))

;; Currently used only in `change-major-mode-hook' and `Info-refontify-current-node'.
;; Use it in Info, instead of `font-lock-defontify'.
;;
(defun Info-defontify ()
  "Remove text properties for fontifying from current Info file.
This includes properties comprising links.  The properties removed are
`font-lock-face', `mouse-face', `help-echo', and `keymap'.  They are
removed from all text in the current Info file (`Info-current-file').

This is like `font-lock-fontify', but that removes only property
`font-lock-face'."
  (let ((modp               (buffer-modified-p))
        (inhibit-read-only  t))
    (save-restriction
      (widen)
      (remove-list-of-text-properties (point-min) (point-max)
                                      '(font-lock-face mouse-face help-echo keymap)))
    (restore-buffer-modified-p modp)))

;; Not used currently, but could be.  Same as `Info-defontify', but just for current node.
;;
(defun Info-defontify-current-node ()
  "Remove text properties for fontifying from current Info node.
The properties removed are `font-lock-face', `mouse-face',
`help-echo', and `keymap'.  They are removed from all text in the
node, not just text used in links."
  (let ((modp               (buffer-modified-p))
	(inhibit-read-only  t))
    (remove-list-of-text-properties (point-min) (point-max)
                                    '(font-lock-face mouse-face help-echo keymap))
    (restore-buffer-modified-p modp)))

(defun Info-manual-string (&optional info-file)
  "String naming Info manual corresponding to INFO-FILE.
Optional arg INFO-FILE defaults to the value of `Info-current-file',
the current Info manual (\"emacs\", \"elisp\", etc.).
Return nil if `Info-current-file' is nil."
  (setq info-file  (or info-file  Info-current-file))
  (file-name-sans-extension (file-name-nondirectory info-file)))

(defun Info-manual-symbol (&optional info-file)
  "Symbol naming the current Info manual (`emacs', `elisp', etc.).
If no current Info manual, then return nil."
  (let ((manual-strg  (Info-manual-string info-file)))
    (and manual-strg  (intern manual-strg))))

(defun info-fontifying-regexp (variable)
  "Value of VARIABLE for current manual, if non-nil, else its global value."
  (or (get (Info-manual-symbol) variable)  (symbol-value variable)))

(defun info-custom-delimited-same-line-regexp ()
  "Return the value of `info-custom-delimited-same-line-regexp'.
Value for current manual, if non-nil, else global value."
  (info-fontifying-regexp 'info-custom-delimited-same-line-regexp))

(defun info-indented-text-regexp ()
  "Set and return value of variable `info-indented-text-regexp'.
The value is for current manual.  It is based on option
`Info-fontify-indented-text-chars', and it applies only to manuals in
option `Info-fontify-indented-text-manuals'."
  (setq info-indented-text-regexp  (format "^ \\{%d,\\}.*" (abs Info-fontify-indented-text-chars)))
  (info-fontifying-regexp 'info-indented-text-regexp))

(defun info-isolated-backquote-regexp ()
  "Return the value of `info-isolated-backquote-regexp'.
Value for current manual, if non-nil, else global value."
  (info-fontifying-regexp 'info-isolated-backquote-regexp))

(defun info-isolated-quote-regexp ()
  "Return the value of `info-isolated-quote-regexp'.
Value for current manual, if non-nil, else global value."
  (info-fontifying-regexp 'info-isolated-quote-regexp))

(defun info-quotation-regexp ()
  "Return the value of `info-quotation-regexp'.
Value for current manual, if non-nil, else global value."
  (info-fontifying-regexp 'info-quotation-regexp))

(defun info-quotation-same-line-regexp ()
  "Return the value of `info-quotation-same-line-regexp'.
Value for current manual, if non-nil, else global value."
  (info-fontifying-regexp 'info-quotation-same-line-regexp))

(defun info-quoted+<>-regexp ()
  "Return the value of `info-quoted+<>-regexp'.
Value for current manual, if non-nil, else global value."
  (info-fontifying-regexp 'info-quoted+<>-regexp))

(defun info-quoted+<>-same-line-regexp ()
  "Return the value of `info-quoted+<>-same-line-regexp'.
Value for current manual, if non-nil, else global value."
  (info-fontifying-regexp 'info-quoted+<>-same-line-regexp))

(defun Info-emphasis-regexp ()
  "Return the value of `Info-emphasis-regexp'.
Value for current manual, if non-nil, else global value."
  (info-fontifying-regexp 'Info-emphasis-regexp))

(defun Info-display-node-default-header ()
  "Insert node name as header."
  ;; `infop-node-name' is free here - bound in `Info-merge-subnodes'.
  (insert (if (fboundp 'concat-w-faces)
              (concat-w-faces (list 'info-title-1 infop-node-name)) ; FREE: INFOP-NODE-NAME
            infop-node-name)
          "\n")
  (goto-char (point-min))
  (center-line 2))

(defun Info-node-name-at-point ()
  "Return the Info node named at point, or nil if none."
  (save-match-data
    (let ((name
           (cond ((Info-get-token (point) "\\*note[ \n\t]+" "\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?"))
                 ((Info-get-token (point) "\\* +" "\\* +\\([^:]*\\)::")) ; Menu item: node name
                 ((Info-get-token (point) "\\* +" "\\* +\\(.*\\): ") ; menu item: node name or index entry
                  (beginning-of-line)
                  (forward-char 2)
                  (Info-extract-menu-node-name nil (Info-index-node)))
                 ((Info-get-token (point) "Up: " "Up: \\([^,\n\t]*\\)"))
                 ((Info-get-token (point) "Next: " "Next: \\([^,\n\t]*\\)"))
                 ((Info-get-token (point) "File: " "File: \\([^,\n\t]*\\)"))
                 ((Info-get-token (point) "Prev: " "Prev: \\([^,\n\t]*\\)")))))
      (and name  (replace-regexp-in-string "\n+" " " name)))))

(when (require 'bookmark+ nil t)

  (defun Info-bookmark-for-node (&optional node localp)
    "Return Info bookmark for NODE, or nil if none.
Non-nil NODE can have the form `NODE' or `(MANUAL) NODE'.
If NODE is nil then read the node name.  If optional arg LOCALP is
non-nil then read the node name only from the current manual."
    (when (and node  (stringp Info-current-file)  (not (string-match-p "(\\([^)]+\\)) \\([^)]*\\)" node)))
      (setq node  (concat "(" (Info-manual-string) ") " node)))
    (unless node (setq node  (Info-read-bookmarked-node-name localp)))
    (bmkp-get-bookmark-in-alist node t (bmkp-info-alist-only)))

  (defun Info-bookmark-name-for-node (node)
    "Return the name of an Info bookmark for NODE, or nil if none.
Non-nil NODE can have the form `NODE' or `(MANUAL) NODE'.
The name of the bookmark must be the default name that
`Info-bookmark-make-record' would use.  This is normally the full node
name, `(MANUAL) NODE', where MANUAL is the lowercase name of the Info
manual.  For example, node `Modes' in the Emacs manual has full
name `(emacs) Modes', and the bookmark must have that same name."
    (let ((bmk  (Info-bookmark-for-node node)))
      (and bmk  (bmkp-bookmark-name-from-record bmk))))

  (defun Info-bookmark-named-at-point ()
    "Return Info bookmark for node named at point, or nil if none.
See `Info-bookmark-name-for-node' for the form of the bookmark name."
    (let ((node  (Info-node-name-at-point)))
      (and node
           (let* ((file  (and (stringp Info-current-file)  (Info-manual-string)))
                  (bname  (if file (concat "(" file ") " node) node)))
             (bmkp-get-bookmark-in-alist bname t (bmkp-info-alist-only))))))

  (defun Info-bookmark-name-at-point ()
    "Return name of Info bookmark for node named at point, or nil if none.
See `Info-bookmark-name-for-node' for the form of the bookmark name."
    (let ((bmk   (Info-bookmark-named-at-point)))
      (and bmk  (bmkp-bookmark-name-from-record bmk))))

  (define-key Info-mode-map (kbd "C-h C-b") 'Info-describe-bookmark)

  )

(when (fboundp 'advice-add)             ; Emacs 24.4+

  (defun Info-save-history-list ()
    "Save `Info-history-list' to `Info-saved-history-file'."
    (when (and Info-persist-history-mode
               (not (string= "" Info-saved-history-file))
               (file-writable-p Info-saved-history-file)
               (not (file-directory-p Info-saved-history-file)))
      (let* ((ibuf  (catch 'Info-save-history-list
                      (dolist (buf  (buffer-list))
                        (with-current-buffer buf
                          (when (derived-mode-p 'Info-mode) (throw 'Info-save-history-list buf))))
                      nil))
             (hist  (and ibuf  (with-current-buffer ibuf Info-history-list))))
        (with-temp-file Info-saved-history-file
          (print Info-history-list (current-buffer))))))

  (defun Info-restore-history-list ()
    "Restore `Info-history-list' from `Info-saved-history-file'."
    (when (and Info-persist-history-mode
               (not (string= "" Info-saved-history-file))
               (file-readable-p Info-saved-history-file))
      (let ((buf  (let ((enable-local-variables  ()))
                    (find-file-noselect Info-saved-history-file)))
            hist)
        (unwind-protect
             (with-current-buffer buf
               (goto-char (point-min))
               (setq hist  (ignore-errors (read (current-buffer)))))
          (kill-buffer buf))
        (when hist
          (setq Info-history-list  hist)
          (when Info-fontify-visited-nodes (Info-fontify-node))))))

  )

(defun Info--manuals (&optional visited-only)
  "Version of `info--manual-names' that works for Emacs 23+."
  (let ((manuals  (and (fboundp 'info--manual-names) ; Arg was added in Emacs 25.
                       (ignore-errors (info--manual-names visited-only)))))
    (unless manuals
      (ignore-errors
        (with-temp-buffer
          (Info-mode)
          (Info-directory)
          (goto-char (point-min))
          (re-search-forward "\\* Menu: *\n" nil t)
          (let (manual)
            (while (re-search-forward "\\*.*: *(\\([^)]+\\))" nil t)
              (setq manual  (match-string 1))
              (set-text-properties 0 (length manual) nil manual)
              (add-to-list 'manuals (list manual)))))))
    manuals))

(if (fboundp 'pop-to-buffer-same-window)
    (defalias 'Info--pop-to-buffer-same-window 'pop-to-buffer-same-window)
  (defalias 'Info--pop-to-buffer-same-window 'switch-to-buffer))

;;; ;; Not currently used.
;;; (defun Info-display-node-time-header ()
;;;   "Insert current time and node name as header."
;;;   ;; `infop-node-name' is free here - bound in `Info-merge-subnodes'.
;;;   (insert (current-time-string) "    " infop-node-name) ; FREE here: INFOP-NODE-NAME
;;;   (beginning-of-buffer)
;;;   (center-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; info+.el ends here
