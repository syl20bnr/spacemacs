;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
(defconst +project-maria-dir+ "/home/ben/project-maria/"
  "On UNIX likes, expands to /home/ben/project-maria/")
(defconst +project-jerome-dir+ "/home/ben/project-jerome/"
  "On UNIX likes, expands to /home/ben/project-jerome/")
(defmacro with-system-name (name &rest body)
  "Evaluate BODY if `system-name' equals NAME.
BHW-THINKPAD is my laptop. localhost is my rooted termux phone."
  (declare (indent defun))
  `(when (string-equal system-name ',name)
     ,@body))

(defun dotspacemacs/layers ()
  "Layer configuration:
  This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      :packages
                      (not
                       ;; Helm is redundant for auto-complete. C-j, C-k instead.
                       helm-company
                       ;; The included snippets are not my taste and too trivial.
                       yasnippet-snippets
                       fuzzy
                       ;; Corfu or company have both replaced auto-complete.
                       ac-ispell
                       auto-complete
                       ))
     (better-defaults :packages
                      (not
                       ;; It is a motion based command, more suited to Emacs way of editing than
                       ;; Vim.
                       mwim))
     (bibtex :variables
             bibtex-enable-ebib-support t
             ebib-preload-bib-files (list (concat +project-maria-dir+ "project-jerome.bib"))
             ebib-file-search-dirs (list +project-jerome-dir+)
             ebib-import-directory (list +project-jerome-dir+))
     ;; $ git clone git@github.com:BenedictHW/common-lisp-sly.git ~/.emacs.d/private/
     common-lisp-sly
     (elfeed :variables
             elfeed-enable-goodies nil
             rmh-elfeed-org-files (list (concat +project-maria-dir+ "dotelfeed.org")))
     (emacs-lisp :packages (not
                            ;; I don't count myself as an elisp author.
                            flycheck-package
                            ;; When will elisp have namespacing a la Common
                            ;; Lisp/Scheme/Clojure?
                            nameless
                            ;; Made obselete by eglot/lsp and sly/slime.
                            emr
                            ;; Never used, look into when I plan on writing
                            ;; elisp unit tests.
                            overseer
                            ;; I don't use cask at all. And elsa depends on cask.
                            flycheck-elsa
                            ;; `load-prefer-newer' is set by spacemacs.
                            auto-compile
                            ;; Common-lisp counterpart is used a lot, and if I
                            ;; do end up using this for emacs lisp, I ought to
                            ;; contribute default bindings.
                            inspector
                            ))
     evil-better-jumper
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors 't)
     finance
     (git :packages (not
                     ;; How often to you edit these files?
                     git-modes
                     ;; More gimmicky than useful.
                     golden-ratio
                     ;; Highlights lines by last change time. Duplicate of magit blame functions.
                     ;; Magit blame has better highlighting too!
                     smeargle
                     ;; Can't say a .gitignore should ever be automatically generated.
                     ;; I mean the wildcard char * is enough.
                     gitignore-templates
                     ;; There is magit log, magit blame, and blamer.el.
                     ;; blamer.el can also show full commits.
                     git-messenger
                     ;; Use SPC g s l l -l -F (or -G) RET (or refresh with g) instead. Magit's
                     ;; transient interface is much better than remembering command line flags.
                     helm-git-grep
                     ;; You already have "magit-blame" with "magit-blame-cycle-style" which
                     ;; highlight the changes, and the ability to do a hard reset with git to
                     ;; a certain commit. If you don't want to look at changes, what do you
                     ;; want to look at? The same parts of the file? To manually pickout
                     ;; changes? Maybe there's a use case I'm missing.
                     git-timemachine
                     ))
     (helm :packages (not
                      ;; I do not prefer the helm interface here.
                      helm-descbinds
                      ;; Helm occur is built-in, retains all functionality and has better
                      ;; performance. It is also preferred by the HELM maintainers. Helm-swoop
                      ;; did come first, but has been superseded by helm occur.
                      helm-swoop
                      ;; I don't need a helm-package to select themes. See `dotspacemacs-themes'
                      helm-themes
                      ;; I have no need right now. How many makefiles does a project need to
                      ;; have to use helm to filter them?
                      helm-make
                      ;; Rendered obselete by SPG g s.
                      helm-ls-git
                      ;; I don't toggle major and minor modes often enough to justify a helm
                      ;; extension.
                      helm-mode-manager
                      ;; Helm has this built-in and flx has a performance cost.
                      helm-flx))
     helpful
     ;; (latex :variables
     ;;        latex-build-command "LatexMk")
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e"
           :packages (not
                      mu4e-maildirs-extension
                      mu4e-alert))
     (org :variables
          org-enable-modern-support nil
          org-want-todo-bindings t
          org-enable-org-brain-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support nil
          org-enable-org-contacts-support t
          org-enable-appear-support t
          org-enable-sticky-header nil
          :packages (not org-rich-yank))
     ;; pandoc
     pdf
     plantuml
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'shell
            :packages (not
                       ;; I use shell. I have no need for the extra eshell utilites.
                       esh-help
                       eshell-prompt-extras
                       eshell-z
                       ;; This package turns out to only be used on an eshell-init hook.
                       xterm-color
                       ;; Built-in Emacs shell has better auto-complete and text pasting abilites.
                       ;; No need for vterm. I don't run graphical ncurses programs.
                       vterm
                       multi-vterm
                       ;; An Emacs package to open an external terminal emulator in directories
                       ;; associated with the current buffer. Originally I thought it was
                       ;; responsible for =spacemacs/default-pop-shell= opening inside the
                       ;; current working directory. But nope, keyword external. I have current
                       ;; need for that.
                       terminal-here
                       ;; I prefer the much better integrated shell (comint) mode.
                       multi-term
                       ))
     (spell-checking :packages (not
                                ;; I only type english in my buffers.
                                auto-dictionary
                                ))
     (syntax-checking :packages (not
                                 ;; This does not work.
                                 flycheck-pos-tip))
     (transmission :variables transmission-auto-refresh-all t)
     ;; Danger! Making edits to the default layers.
     ;; See Spacemacs Issue 13595. The spacemacs distribution layer lists
     ;; treemacs in configuration-layer/declare-layers.
     (treemacs :packages nil)
     (spacemacs-bootstrap :packages (not
                                     dash
                                     holy-mode
                                     hybrid-mode))
     (spacemacs-defaults :packages (not
                                    ;; Quickrun compiles a buffer. Org-babel
                                    ;; exists for snippets, and I don't see
                                    ;; why you would need to call multiple
                                    ;; different compilers on one buffer.
                                    quickrun))
     (spacemacs-editing
      :variables
      vim-style-enable-undo-region t
      ;; :variables vim-style-enable-undo-region t
      :packages (not
                 ;; The flashing gets annoying after a while.
                 eval-sexp-fu
                 ;; Doom emacs forbids expand-region by
                 ;; default and instead rightly insists on
                 ;; using vim objects.
                 expand-region
                 ;; I am happy with the default Emacs
                 ;; electric-indent. It probably has better
                 ;; performance too. Aggressive-indent is
                 ;; buggy with org-babel blocks.
                 aggressive-indent
                 ;; .editorconfig is a formally specified grammar to specify consistent
                 ;; formatting across teams of developers using different IDE's and
                 ;; languages
                 editorconfig
                 ;; Deleting all whitespace with <BACKSPACE> instead of <C-BACKSPACE> just
                 ;; doesn't seem all that useful.
                 hungry-delete
                 ;; It can be comepletely replaced by a few keyboard macros. It remains
                 ;; arguably very useful. Just a bit too nichce, and keyboard macros are
                 ;; more general.
                 multi-line
                 ;; I use KeePass.
                 password-generator
                 ;; I don't currently need to generate UUID's
                 uuidgen
                 ;; I don't need to edit HTML in javascript buffers or
                 ;; edit strings in seperate buffer
                 string-edit
                 ;; I have no need/too niche.
                 lorem-ipsum
                 ;; Dragging stuff arond with arrow keys is never used when you have vim.
                 drag-stuff
                 ;; No need when =s= is already bound to avy-goto-word-or-subword-1.
                 evil-easymotion))
     (spacemacs-editing-visual :packages (not
                                          ;; I use GUI over terminal Emacs and so there is no need to change the cursor shape.
                                          term-cursor
                                          ;; I don't use it.
                                          highlight-indentation
                                          writeroom-mode
                                          ;; Super neat feature I don't need.
                                          hide-comnt
                                          ;; Depends on obselete
                                          ;; parent-mode-2.0 when there exists
                                          ;; (get 'latex-mode
                                          ;; 'derived-mode-parent). With
                                          ;; default theme, the highlight is
                                          ;; barely noticiable anyways.
                                          highlight-numbers
                                          ;; Slower and buggier when compared
                                          ;; to the already more popular and
                                          ;; more recently maintainted
                                          ;; highlight-indentation.el. And
                                          ;; highlight-indentation is already
                                          ;; included in Spacemacs...
                                          indent-guide
                                          ;; The package volatile highlights
                                          ;; temporarily highlights changes to
                                          ;; the buffer associated with
                                          ;; certain commands that add blocks
                                          ;; of text at once. An example is
                                          ;; that if you paste (yank) a block
                                          ;; of text, it will be highlighted
                                          ;; until you press the next key. I
                                          ;; don't need it for the same reason
                                          ;; I don't use evil-goggles. Plus
                                          ;; both evil goggles and
                                          ;; volatile-highlights are redundant
                                          ;; to an extent since the built-in
                                          ;; pulse exists.
                                          volatile-highlights
                                          ))
     (spacemacs-completion :packages (not
                                      ;; I use helm and not ido.
                                      flx-ido))
     (spacemacs-language :packages (not
                                    ;; Not exactly redundant, as lexic only searches offline dictionaries.
                                    ;; Still I have no use for it between lexic and the =google translate=
                                    ;; package
                                    define-word))
     (spacemacs-layouts :packages (not
                                   counsel-projectile))
     (spacemacs-misc :packages (not
                                ;; Each programming mode usually comes with it's own docs.
                                devdocs))
     (spacemacs-purpose :packages (not
                                   ;; With =helm-for-files= and =helm-ag= I have never used =SPC r b= =SPC r
                                   ;; p= or =SPC r B=. Having just tried all three, they leave me
                                   ;; unimpressed. Don't get me wrong, I quite like purpose (combined with
                                   ;; eyebrowse in the spcaemacs-layouts layer makes for fantastic stuff).
                                   ;; But I fail to see why I need helm for it.
                                   helm-purpose))
     ;; I prefer no battery info on my modeline. Emacs profiler tells me the
     ;; package uses a lot of battery itself.
     ;; Ditto for icons, lots of wasted cpu cycles for no functionality.
     ;; Symon is cute, but https://github.com/zk-phi/symon/issues/42 shows
     ;; that a significant performance price is paid.
     (spacemacs-modeline :packages (not
                                    anzu
                                    fancy-battery
                                    font-lock+
                                    neotree
                                    ;; spaceline-all-the-icons
                                    symon
                                    vim-powerline))
     (spacemacs-navigation :packages (not
                                      restart-emacs
                                      ;; `avy-goto-word-or-subword-1' is a more general use case.
                                      ace-link
                                      ;; I believe the winum (SPC 1) and avy-word (jumps across windows)
                                      ;; is enough.
                                      ace-window
                                      ;; package-list-packages and
                                      ;; package-menu-filter-by-keyword "status:installed"
                                      ;; paradox
                                      ;; More gimmicky than useful.
                                      golden-ratio
                                      ;; Utility packages that I have no use for.
                                      centered-cursor-mode
                                      open-junk-file
                                      auto-highlight-symbol))
     (spacemacs-org :packages (not
                               ;; use C-c C-k with pointer on org headline.
                               toc-org))
     (spacemacs-visual :packages (not all-the-icons))
     (spacemacs-evil :packages (not
                                ;; I never edit,
                                ;; Binary, e.g. 0b0101, 0B0101.
                                ;; Octal, e.g. 0o755, 0O700.
                                ;; Hexadecimal, e.g. 0xDEADBEEF, 0XCAFE.
                                ;; Unicode superscript and subscript, e.g. ² and 1.
                                evil-numbers
                                ;; There exists more updated and better semantic editing packages for sexps
                                evil-lisp-state
                                ;; Disables hitting "fd" in rapid succession to function as ESC
                                ;; We already swap ESC and CAPS LOCK keys.
                                evil-escape
                                ;; helm-occur/helm-ag/helm-fdfind then open results in a new buffer.
                                ;; Optionally then turn on follow-mode for said buffer. This is a better workflow.
                                evil-anzu
                                ;; Too niche of a use case. I've never used it myself. When vim's delete
                                ;; isn't enough we have search and replace.
                                evil-exchange
                                ;; Why? With the default spacemacs settings, I have never seen this
                                ;; package in my life. Package author says =d2w= is a common use case.
                                ;; Doesn't apply to me, but glad it exists for those that do.
                                evil-goggles
                                ;; From
                                ;; https://emacs.stackexchange.com/questions/47821/iedit-vs-multiple-cursors/47828#47828
                                ;; I never used iedit extensively, so I could be unaware of some of its
                                ;; features, but basically iedit is a more interactive search and
                                ;; replace: you can select occurrences of a string in some scope (region,
                                ;; defun, buffer, etc.) and then replace them with something else.
                                ;;
                                ;; Compared to that, multiple-cursors generalizes in two directions, the
                                ;; second much more important than the first in my opinion:
                                ;;
                                ;; 1. You are not limited to put cursors just at occurrences of some string.
                                ;; You can put cursors wherever you click the mouse, or at the end of a
                                ;; bunch of lines, or at matches of a regexp, etc., or any combination of
                                ;; these locations.
                                ;;
                                ;; 2. Even if you did choose to place cursors just at occurrences of some
                                ;; string, you are not limited to just replacing them with some other
                                ;; string. You can do any Emacs text editing simultaneously at every
                                ;; location. For example, you could transpose the words found around each
                                ;; cursor, or mark the entire defun containing each cursor and comment it
                                ;; out.
                                ;;
                                ;; Just as iedit can be thought of as an instant-preview version of
                                ;; query-replace, multiple-cursors can be thought of as an
                                ;; instant-preview version of keyboard macros.
                                evil-iedit-state
                                ;; Great tutorial, no need for it currently.
                                evil-tutor
                                ;; All the functionality under =[= and =]= are duplicated.
                                evil-unimpaired
                                ;; Mark functionality is pretty much replaced by =g d= for
                                ;; =jump-to-definition= and =C-o/C-i= to jump back.
                                evil-visual-mark-mode
                                ;; No need for this when you have helm occur.
                                evil-visualstar
                                ;; I don't need an evil argument object. I'll have "what is =ci)= for 500
                                ;; Alex." Of course, swapping [] and () on your keyboard will do wonders.
                                ;; I'd rather fix the problem at the root level.
                                evil-args
                                ;; This package marks empty lines with a =~=. Superfluous.
                                vi-tilde-fringe
                                ;; I use 'da(' wat more often than indendation.
                                evil-indent-plus
                                ;; I don't need this.
                                vim-empty-lines-mode
                                ))
     )
   ;; ***  Additional Packages
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(;; Replaces `dotspacemacs/user-env'.
     exec-path-from-shell
     org-noter
     org-pdftools
     org-web-tools
     ;; Pulls entire douay rheims bible from sword project
     ;; (sword-to-org :location (recipe :fetcher github
     ;;                                  :repo "alphapapa/sword-to-org"))
     cdlatex
     org-fragtog
     ;; keyfreq
     ;; For org-brain
     ascii-art-to-unicode
     ;; wolfram-mode
     ;; See emacs-jupyter in user config section
     ;; jupyter
     lexic
     ;; Since it is a pretty heavy package, only include it when doing heavy
     ;; analysis on my own time spent.
     ;; org-analyzer
     org-tanglesync
     poly-org
     ement
     ox-rss
     youtube-sub-extractor
     elfeed-tube
     elfeed-tube-mpv
     greader
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages
   '(;; Otherwise it will reinstall itself
     helm-swoop
     )

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(;; I don't use hybrid mode.
     hybrid-mode
     ;; These 4 packages can't be put into spacemacs/layers as they are part
     ;; of the built-in package org. Has scaling issues with large org files
     ;; and has been superseded by =org-ql=, according to the package author.
     helm-org-rifle
     ;; Superseded by org-re-reveal.
     org-present
     ;; I have a watch with a timer.
     org-pomodoro
     ;; Org-projectile provides functions for the creation of org-mode TODOs
     ;; that are associated with projectile projects. I keep all my TODO's in
     ;; a project-maria, a centralized place, a single projectile project.
     ;; Therefore I have no use for this package.
     org-projectile
     dotenv-mode
     )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))
(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   ;; Emacs maintainer Eli Zaretskii recommends not altering the value from
   ;; Emacs default.
   ;; https://old.reddit.com/r/emacs/comments/bg85qm/garbage_collector_magic_hack/
   ;; https://old.reddit.com/r/emacs/comments/6uc7g5/just_figured_out_why_emacs_pauses_sometimes/
   ;; Update: see `spacemacs-editing/init-undo-tree' undo limits.
   ;; If we lower the threshold there will be freezing
   ;; when we use the undo history.
   ;; Also see: https://github.com/lewang/flx#gc-optimization
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((agenda . 30))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'nil

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'common-lisp-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 0.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   ;; Dell 32 4K USB-C Hub Monitor - P3222QE
   dotspacemacs-default-font '("Iosevka Term Slab"
                               :size 18.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ""

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 50
   ;; Also see large-file-warning-threshold, set in dotspacemacs user config

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing the
   ;; commands bound to the current keystroke sequence. (default 0.4) HW annot:
   ;; the default sacrifices -quite a lot of!- performance for accessibility,
   ;; and after some experience a tradeoff in favour of the former is preferred.
   dotspacemacs-which-key-delay 2

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 100

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format nil

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode nil

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source t

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile t))


(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
  See the header of this file for more information.")


(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first.")


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs
initialization after layers configuration. This is the place
where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a
package is loaded, you should place your code here."
  (add-hook 'eww-after-render-hook 'eww-readable)
  (require 'greader)
  ;; FIXME Auto-completion causes lag in shell-mode.
  (setf company-global-modes '(not shell-mode))
  (require 'youtube-sub-extractor)
  (spacemacs/set-leader-keys
    (kbd "acm") #'ement-list-rooms
    (kbd "acn") #'ement-notify-switch-to-notifications-buffer
    (kbd "acc") #'ement-room-send-message
    (kbd "acv") #'ement-view-room)
  (with-eval-after-load 'ement
    (evil-collection-ement-setup)
    (define-key ement-room-mode-map (kbd ";") #'helm-occur)
    (evil-define-key* 'motion evil-snipe-override-local-mode-map ";" #'nil)
    (evil-define-key* 'motion evil-snipe-override-local-mode-map (kbd "<C-return>") #'ement-room-list-RET)
    (setf ement-room-mark-rooms-read 'send))
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
        org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
        plantuml-output-type "txt")
  (with-eval-after-load "org-mode"
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
  (setf paradox-github-token t)
  ;; (keyfreq-mode 1)
  ;; (keyfreq-autosave-mode 1)
  ;; (setf keyfreq-excluded-commands
  ;;       '(self-insert-command
  ;;         org-self-insert-command
  ;;         forward-char
  ;;         backward-char
  ;;         previous-line
  ;;         next-line
  ;;         evil-forward-char
  ;;         evil-backward-char
  ;;         evil-previous-visual-line
  ;;         evil-next-visual-line
  ;;         helm-next-line
  ;;         helm-previous-line
  ;;         evil-scroll-page-down
  ;;         evil-scroll-page-up
  ;;         delete-backward-char
  ;;         evil-delete-backward-char-and-join
  ;;         evil-undo
  ;;         mwheel-scroll
  ;;         mouse-set-point
  ;;         mouse-drag-region
  ;;         evil-mouse-drag-region
  ;;         evil-normal-state
  ;;         keyboard-escape-quit
  ;;         evil-goto-first-line
  ;;         evil-insert
  ;;         evil-append
  ;;         evil-delete
  ;;         evil-join
  ;;         evil-delete-char
  ;;         evil-open-below
  ;;         evil-change
  ;;         backward-delete-char-untabify
  ;;         dired-next-line
  ;; dired-previous-line))
  ;;-------------------------------------------------------------------------
  ;; ***  Pandoc Config
  ;;-------------------------------------------------------------------------
  ;;-------------------------------------------------------------------------
  ;; ***  PDF tools config
  ;;-------------------------------------------------------------------------
  ;; Initilization settings. Source: http://pragmaticemacs.com/emacs/even-more-pdf-tools-tweaks/
  ;; Allows scaling of images during rendering
  (setq pdf-view-use-scaling nil) ; possible source of memory leak?
  (setq pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1)
  ;; See also .wslconfig for virtual machine RAM limit.
  ;; 1 GB with a 4k display means Out of Memory is possibility.
  ;; Clears image cache to reduce ram usage.
  (setq image-cache-eviction-delay 5)
  (setf pdf-cache-image-limit 5)
  ;; Custom function to allow double page scrolling by calling
  ;; my-pdf-view-double-scroll-horizontal-view
  (defun my-pdf-view-double-scroll-up-or-next-page (&optional arg)
    "Scroll page up ARG lines if possible, else go to the next page.
  When `pdf-view-continuous' is non-nil, scrolling upward at the
  bottom edge of the page moves to the next page.  Otherwise, go to
  next page only on typing SPC (ARG is nil)."
    (interactive "P")
    (if (or pdf-view-continuous (null arg))
        (let ((hscroll (window-hscroll))
              (cur-page (pdf-view-current-page)))
          (when (or (= (window-vscroll) (image-scroll-up arg))
                    ;; Workaround rounding/off-by-one issues.
                    (memq pdf-view-display-size
                          '(fit-height fit-page)))
            (pdf-view-next-page 2)
            (when (/= cur-page (pdf-view-current-page))
              (image-bob)
              (image-bol 1))
            (set-window-hscroll (selected-window) hscroll)))
      (image-scroll-up arg)))
  (defun my-pdf-view-double-scroll-horizontal-view ()
    (interactive)
    (my-pdf-view-double-scroll-up-or-next-page)
    (other-window 1)
    (my-pdf-view-double-scroll-up-or-next-page)
    (other-window 1))
  ;; add spacemacs major mode keybind
  (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode "d" 'my-pdf-view-double-scroll-horizontal-view)
  ;; Allow rotating of sheet music in pdfs
  (defun pdf-view--rotate (&optional counterclockwise-p page-p)
    "Rotate PDF 90 degrees.  Requires pdftk to work.\n
  Clockwise rotation is the default; set COUNTERCLOCKWISE-P to
  non-nil for the other direction.  Rotate the whole document by
  default; set PAGE-P to non-nil to rotate only the current page.
  \nWARNING: overwrites the original file, so be careful!"
    ;; error out when pdftk is not installed
    (if (null (executable-find "pdftk"))
        (error "Rotation requires pdftk")
      ;; only rotate in pdf-view-mode
      (when (eq major-mode 'pdf-view-mode)
        (let* ((rotate (if counterclockwise-p "left" "right"))
               (file   (format "\"%s\"" (pdf-view-buffer-file-name)))
               (page   (pdf-view-current-page))
               (pages  (cond ((not page-p)                        ; whole doc?
                              (format "1-end%s" rotate))
                             ((= page 1)                          ; first page?
                              (format "%d%s %d-end"
                                      page rotate (1+ page)))
                             ((= page (pdf-info-number-of-pages)) ; last page?
                              (format "1-%d %d%s"
                                      (1- page) page rotate))
                             (t                                   ; interior page?
                              (format "1-%d %d%s %d-end"
                                      (1- page) page rotate (1+ page))))))
          ;; empty string if it worked
          (if (string= "" (shell-command-to-string
                           (format (concat "pdftk %s cat %s "
                                           "output %s.NEW "
                                           "&& mv %s.NEW %s")
                                   file pages file file file)))
              (pdf-view-revert-buffer nil t)
            (error "Rotation error!"))))))
  
  (defun pdf-view-rotate-clockwise (&optional arg)
    "Rotate PDF page 90 degrees clockwise.  With prefix ARG, rotate
  entire document."
    (interactive "P")
    (pdf-view--rotate nil (not arg)))
  
  (defun pdf-view-rotate-counterclockwise (&optional arg)
    "Rotate PDF page 90 degrees counterclockwise.  With prefix ARG,
  rotate entire document."
    (interactive "P")
    (pdf-view--rotate :counterclockwise (not arg)))
  
  (define-key spacemacs-pdf-view-mode-map (kbd "R") 'pdf-view-rotate-clockwise)
  (setf forge-owned-accounts '(("BenedictHW" :remote-name "origin"))
        magit-save-repository-buffers 'dontask)
  ;;-------------------------------------------------------------------------
  ;; ***  Emacs Jupyter Config
  ;;-------------------------------------------------------------------------
  
  ;; Changes the dired buffer to display less details.
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode)))
  (global-auto-revert-mode t)
  (setf dired-omit-mode t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        ;; Stop asking to quit dired buffers of deleted files
        dired-clean-up-buffers-too nil)
  ;; Rebinds "s" from "hydra-dired-quick-sort/body" to "avy-goto-word-or-subword-1"
  (with-eval-after-load "dired"
    (evil-define-key 'normal dired-mode-map (kbd "s") #'avy-goto-word-or-subword-1)
    (evil-define-key 'normal dired-mode-map (kbd "S") #'hydra-dired-quick-sort/body))
  (evil-define-key 'normal dired-mode-map (kbd ";") #'helm-occur)
  (evil-define-key 'normal dired-mode-map (kbd "C-i") #'better-jumper-jump-forward)
  (evil-define-key 'normal dired-mode-map (kbd "C-o") #'better-jumper-jump-backward)
  (evil-define-key 'normal dired-mode-map (kbd "q") #'spacemacs/kill-this-buffer)
  (evil-define-key 'normal dired-mode-map (kbd "Q") #'kill-buffer-and-window)
  (setf common-lisp-hyperspec-root
        (concat
         "file://"
         +project-jerome-dir+
         "000-generalities-information-computers/000-computer-science/HyperSpec/"))
  ;; https://emacs.stackexchange.com/questions/62536/what-does-making-browse-url
  ;; -browser-function-local-to-eww-while-let-bound-m
  (advice-add 'hyperspec-lookup
              :around
              (lambda (orig-fun &rest args)
                (setq-local browse-url-browser-function 'eww-browse-url)
                (apply orig-fun args)))
  ;;-------------------------------------------------------------------------
  ;; ***  Python Layer Config
  ;;-------------------------------------------------------------------------
  ;; (setq python-format-on-save t)
  ;; Make sure the black formatter package is installed
  ;; (setq python-formatter 'black)
  ;; (setf python-indent-offset 4)
  (require 'lexic)
  (setf lexic-dictionary-specs '
        (;; The Golden Mean
         ("Webster's Revised Unabridged Dictionary (1913)"
          :short "===========================================================\n Webster's Revised Unabridged Dictionary (1913)\n============================================================"
          :formatter lexic-format-webster
          :priority 1)
         ;; Synonyms
         ("Soule's Dictionary of English Synonyms (En-En)"
          :short "===========================================================\n Soule's Dictionary of English Synonyms (1871)\n============================================================"
          :formatter lexic-format-soule
          :priority 2)
         ("Moby Thesaurus II"
          :short "===========================================================\n Moby Thesaurus II by Grady Ward, 1.0 (2002)\n============================================================"
          :priority 3)
         ;; Wham Bam Thank you Ma'am
         ("WordNet 3.0 (En-En)"
          :short "===========================================================\n WordNet (r) 3.0 (2009)\n============================================================"
          :formatter lexic-format-webster
          :priority 4)
         ;; The Standard Bearer
         ("Oxford English Dictionary 2nd Ed. P1"
          :short "===========================================================\n Oxford English Dictionary 2nd Ed. (1989)\n============================================================"
          :formatter lexic-format-soule
          :priority 5)
         ("Oxford English Dictionary 2nd Ed. P2"
          :short "===========================================================\n Oxford English Dictionary 2nd Ed. (1989)\n============================================================"
          :formatter lexic-format-soule
          :priority 6)
         ;; Encyclopedia
         ;; Anything more and one should turn to google
         ("The Britannica Concise"
          :short "===========================================================\n Britannica Concise Encyclopedia (2006)\n============================================================"
          :priority 7)
         ;; Specialized Dictionaries
         ;; Insert Bible Catholic Dictionary
         ;; Law
         ("Bouvier's Law Dictionary"
          :short "===========================================================\n Bouvier's Law Dictionary, Revised 6th Ed. (1856)\n============================================================"
          :priority 8)
         ("Black's Law Dictionary 8th Edition"
          :short "===========================================================\n Black's Law Dictionary 8th Edition\n============================================================"
          :priority 9)
         ;; Medicine
         ("Black's Medical Dictionary"
          :short "===========================================================\n Black's Medical Dictionary 41st Ed.\n============================================================"
          :formatter lexic-format-soule
          :priority 10)
         ("Stedman's Medical Dictionary"
          :short "===========================================================\n Stedman's Medical Dictionary\n============================================================"
          :formatter lexic-format-soule
          :priority 11)
         ;; Science
         ("McGraw-Hill Dictionary of Science and Technology"
          :short "===========================================================\n McGraw-Hill Dictionary of Science and Technology\n============================================================"
          :formatter lexic-format-soule
          :priority 12)
         ("Elements database"
          :short "===========================================================\n Elements Database compiled J. Kominek (2000)\n============================================================"
          :formatter lexic-format-element
          :priority 13)
         ("Online Etymology Dictionary"
          :short "===========================================================\n Online Etymology Dictionary (2000)\n============================================================"
          :formatter lexic-format-online-etym
          :priority 14)
         ("Hitchcock's Bible Names Dictionary"
          :short "===========================================================\n Hitchcock's Bible Names (2000)\n============================================================"
          :priority 15)
         ))
  ;; Set Global Keybindings
  (spacemacs/set-leader-keys "sx" 'lexic-search-word-at-point)
  (spacemacs/set-leader-keys "sX" 'lexic-search)
  ;; Set Lexic Major Mode Keybindings
  (spacemacs/set-leader-keys-for-major-mode 'lexic-mode "q" 'lexic-return-from-lexic)
  (spacemacs/set-leader-keys-for-major-mode 'lexic-mode (kbd "RET") 'lexic-search-word-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'lexic-mode "a" 'outline-show-all)
  (spacemacs/set-leader-keys-for-major-mode 'lexic-mode "h" 'outline-hide-body)
  (spacemacs/set-leader-keys-for-major-mode 'lexic-mode "o" 'lexic-toggle-entry)
  (spacemacs/set-leader-keys-for-major-mode 'lexic-mode "n" 'lexic-next-entry)
  (spacemacs/set-leader-keys-for-major-mode 'lexic-mode "p" 'lexic-previous-entry)
  (spacemacs/set-leader-keys-for-major-mode 'lexic-mode "b" 'lexic-search-history-backwards)
  (spacemacs/set-leader-keys-for-major-mode 'lexic-mode "f" 'lexic-search-history-forwards)
  ;; I am convinced this is a case of bad defaults. Setting
  ;; =savehist-autosave-interval= to 60 seconds (from the default of 300) and
  ;; =history-length= to 1000 (from the default of 100) causes disproportionate
  ;; performance problems for arguable benefits. Performance problems can be plainly
  ;; seen by using the Emacs cpu+mem profiler. See
  ;; https://emacs.stackexchange.com/questions/12086/high-cpu-memory-usage-and-abnormally-large-savehist-file
  ;; =spacemacs-defaults/init-savehist=, Spacemacs Github issues #9409, #1369.
  ;; Reset variables to sensible Emacs defaults.
  (setf history-length 25
        savehist-save-minibuffer-history nil
        savehist-autosave-interval nil
        kill-ring-max 200
        savehist-mode nil)
  (delq 'mark-ring savehist-additional-variables)
  (delq 'global-mark-ring savehist-additional-variables)
  (delq 'search-ring savehist-additional-variables)
  (delq 'regexp-search-ring savehist-additional-variables)
  (delq 'extended-command-history savehist-additional-variables)
  (delq 'kill-ring savehist-additional-variables)
  (put 'org-brain-headline-cache 'history-length 10)
  (put 'bibtex-completion-cache 'history-length 10)
  (push 'org-brain-headline-cache savehist-additional-variables)
  (push 'bibtex-completion-cache savehist-additional-variables)
  (push 'helm-ff-history savehist-additional-variables)
  (push 'org-clock-history savehist-additional-variables)
  ;; Emacs profiler shows `savehist-autosave' is very performance intensive.
  (add-hook 'kill-emacs-hook #'savehist-save) ; Savehist only on exit.
  ;; Scrolling.
  ;;
  ;; The behaviors Emacs offers for scrolling can be customized
  ;; by the variables some of which were already mentioned:
  ;; scroll-conservatively, scroll-margin, scroll-step, and
  ;; scroll-up/down-aggressively. They basically control whether
  ;; Emacs recenters point when it scrolls the window, when (if
  ;; at all) it does recenter, by how many lines it scrolls if
  ;; it doesn't recenter, and how close to window edges point is
  ;; allowed to be before the window is scrolled. This defines a
  ;; set of behaviors you can get universally. In general, the
  ;; default is to recenter if scrolling by a few lines fails to
  ;; bring point into view. That is what you see, and that is
  ;; how Emacs works.
  ;; Source: https://old.reddit.com/r/emacs/comments/8jli87/is_there_a_hook_after_cursor_jump/
  (setq scroll-conservatively 0)
  (setq scroll-preserve-screen-position t)
  ;; Line Breaks/Fill Column/Characters
  ;;
  ;; Prefer 80 chars due to anatomical restriction of the human eye.
  ;; Secondary concern of long known emacs performance issues with long lines.
  ;;
  ;; According to the Emacs manual, to enable autofill in all major modes:
  ;; (setq-default auto-fill-function 'do-auto-fill)
  ;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
  ;;
  ;; However, it does have side-effects in some modes, most notably it screws
  ;; up auto-completion in lisp mode buffers. In addition to performance
  ;; reasons, it makes sense to selectively enable for certain modes.
  ;; Therefore look under the mode configuration for the added hooks.
  ;; i.e. search for (add-hook 'example-mode-hook 'turn-on-auto-fill)
  ;; allows the use of SPC leader key in calc buffer
  (with-eval-after-load 'calc
    (define-key calc-mode-map " " spacemacs-cmds))
  (setq large-file-warning-threshold '100000000)
  ;; https://www.masteringemacs.org/article/disabling-prompts-emacs
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))
  (require 'cl-lib)
  (defun site/always-save-advice (oldfn &optional arg)
    "Overwrite `yes-or-no-p' in OLDFN.
    The new temporary function will return non-nil, when the message
    wants to save modified buffers, without querying the user.
    Otherwise the original behaviour is preserves, and ARG is passed
    on to OLDFN."
    (cl-letf* ((real-yes-or-no-p (symbol-function 'yes-or-no-p))
               ((symbol-function 'yes-or-no-p)
                (lambda (msg)
                  (or (string= msg "Modified buffers exist; exit anyway? ")
                      (funcall real-yes-or-no-p msg)))))
      (funcall oldfn arg)))
  
  (advice-add #'save-buffers-kill-emacs :around #'site/always-save-advice)
  ;; Spacemacs default is 60 seconds. Ridiculous.
  (setf auto-save-interval 1000
        auto-save-timeout nil)
  ;; https://github.com/joaotavora/yasnippet/issues/998#issuecomment-496449546
  (defun my-yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  (add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)
  ;; C-h f "evilnc-comment-operator" or any of the default evil keybindings
  ;; to discover what maps you need to override.
  ;; rebind "SPC j j" or avy-goto-word-or-subword-1 to "s"
  ;; At first, will not work as evil-surround is using "s" in visual mode
  (setf evil-move-beyond-eol t)
  (evil-define-key 'visual evil-surround-mode-map
    (kbd "s") 'avy-goto-word-or-subword-1)
  (evil-define-key 'visual evil-surround-mode-map
    (kbd "S") 'evil-surround-region)
  (define-key evil-motion-state-map (kbd "s") 'avy-goto-word-or-subword-1)
  (define-key evil-normal-state-map (kbd "s") 'avy-goto-word-or-subword-1)
  (define-key evil-visual-state-map (kbd "s") 'avy-goto-word-or-subword-1)
  (define-key evil-operator-state-map (kbd "s") 'avy-goto-word-or-subword-1)
  (define-key package-menu-mode-map (kbd "s") 'avy-goto-word-or-subword-1)
  ;; #'helm-occur rebinding
  (define-key evil-motion-state-map (kbd ";") #'helm-occur)
  (define-key evil-normal-state-map (kbd ";") #'helm-occur)
  (define-key evil-visual-state-map (kbd ";") #'helm-occur)
  (define-key evil-operator-state-map (kbd ";") #'helm-occur)
  (define-key package-menu-mode-map (kbd ";") #'helm-occur)
  (define-key undo-tree-visualizer-mode-map (kbd ";") #'helm-occur)
  ;; Whenever the commands modified below are called, they are pushed
  ;; onto the evil-jumps-history stack
  (evil-add-command-properties #'evil-scroll-down :jump t)
  (evil-add-command-properties #'evil-scroll-up :jump t)
  (evil-add-command-properties #'evil-scroll-page-down :jump t)
  (evil-add-command-properties #'evil-scroll-page-up :jump t)
  (evil-add-command-properties #'helm-occur :jump t)
  (evil-add-command-properties #'helm-for-files :jump t)
  (evil-add-command-properties #'helm-projectile-find-file :jump t)
  (evil-add-command-properties #'spacemacs/alternate-buffer :jump t)
  (evil-add-command-properties #'spacemacs/helm-M-x-fuzzy-matching :jump t)
  (evil-add-command-properties #'helm-find-files :jump t)
  (evil-add-command-properties #'org-previous-visible-heading :jump t)
  (evil-add-command-properties #'org-next-visible-heading :jump t)
  (evil-add-command-properties #'outline-up-heading :jump t)
  (evil-add-command-properties #'outline-next-heading :jump t)
  (evil-add-command-properties #'org-bable-goto-src-block-head :jump t)
  (define-key evil-normal-state-map (kbd "q") #'spacemacs/kill-this-buffer)
  (define-key evil-normal-state-map (kbd "Q") #'kill-buffer-and-window)
  ;;-------------------------------------------------------------------------
  ;; ***  Evil Snipe Config
  ;;-------------------------------------------------------------------------
  ;; Disable all keybindings other than f/t
  (evil-snipe-mode -1)
  (setq  evil-snipe-scope 'whole-visible)
  ;; Alias [ and ] to all types of brackets
  ;; Alias ' to ' and "
  (setq evil-snipe-aliases
        '((?\' "['\"]")
          ;; No longer needed as () are translated to []
          ;; via keyboard-translate function
          ;; (?\[ "[[{(]")
          ;; (?\] "[]})]")
          ))
  ;; Remove overriding of "," key in visual mode Ex. "vf),"
  (setq evil-snipe-override-evil-repeat-keys nil)
    ;; See /home/ben/.config/fd/ignore
    (require 'helm-fd)
    (require 'helm-ag)
  
    (defvar bhw/helm-source-fd
      (helm-make-source
          (format "fd (%s)"
                  (abbreviate-file-name default-directory)) 'helm-fd-class)
      "For use of FD in `helm-for-files'. See also `helm-fd-switches'")
  
    (defvar bhw/helm-source-maria-ag
      (helm-make-source "Project Maria - AG" 'helm-do-ag-class
        :candidates-process
        (lambda ()
          (helm-ag--do-ag-set-command)
          (helm-ag--do-ag-candidate-process +project-maria-dir+)))
      "To search Project Maria files from `helm-for-files'.
  `helm-ag--do-ag-set-source' used as exemplar. You may have to run
  `helm-projectile-ag' once for fuzzy matching to kick in :O.")
  
    (defvar bhw/helm-source-emacs-commands
      (helm-build-sync-source "Emacs Commands"
        :candidates (lambda ()
                      (let ((cmds))
                        (mapatoms (lambda (elt)
                                    (when (commandp elt)
                                      (push (symbol-name elt) cmds))))
                        cmds))
        :coerce #'intern-soft
        :action #'command-execute)
      "A simple helm source for Emacs commands. Used in `helm-for-files'.")
    (setf ace-jump-helm-line-default-action 'select
          ace-jump-helm-line-idle-delay 1
          helm-ff-auto-update-initial-value t
          recentf-max-saved-items 1000
          ;; FIXME `helm-source-buffers-list' causes, ad-Advice-set-window-buffer:
          ;; Wrong type argument: bufferp, nil when selecting buffer. I'm fairly
          ;; sure the problem is in `spacemacs//helm-open-buffers-in-windows'. It
          ;; might also be because of helm-fd not giving control back, As I
          ;; noticed this error would be fixed if I Ctrl-o to another source after
          ;; helm-fd process was done.
          helm-for-files-preferred-list '(;; helm-source-buffers-list
                                          helm-source-recentf
                                          bhw/helm-source-fd
                                          bhw/helm-source-maria-ag
                                          bhw/helm-source-emacs-commands)
          helm-candidate-number-limit 100
          helm-ff-skip-boring-files t
          helm-ag-fuzzy-match t
          helm-fd-executable "fdfind"
          helm-fd-switches '("--search-path" "/home/ben" "--hidden" "--type" "f"
                             "--type" "d" "--color" "never" "--max-results" "10"
                             "--full-path")
          ;; FIXME Remove when Debian stable fdfind --version > 8.3.0
          ;; https://github.com/bbatsov/projectile/issues/1788
          projectile-generic-command "fdfind . -0 --type f --color=never"
          projectile-git-fd-args "-H -0 -E .git -tf")
  
    (spacemacs/set-leader-keys "SPC" #'helm-for-files)
    (define-key helm-map (kbd "C-q") nil) ; Replace default binding.
    (define-key helm-map (kbd "C-d") 'ace-jump-helm-line)
  (require 'org)
  ;; Format text to fit 80 chars when pressing RET or ENTER.
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  ;; Source https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
  (defun afs/org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
    (interactive)
    (if (org-in-regexp org-link-bracket-re 1)
        (save-excursion
          (let ((remove (list (match-beginning 0) (match-end 0)))
                (description
                 (if (match-end 2)
                     (org-match-string-no-properties 2)
                   (org-match-string-no-properties 1))))
            (apply 'delete-region remove)
            (insert description)))))
  ;; Source https://emacs.stackexchange.com/questions/12391/insert-org-id-link-at-point-via-outline-path-completion
  (defun org-id-complete-link (&optional arg)
    "Create an id: link using completion"
    (concat "id:"
            (org-id-get-with-outline-path-completion)))
  (org-link-set-parameters "id"
                           :complete 'org-id-complete-link)
  ;; Source https://hungyi.net/posts/copy-org-mode-url/
  (defun org-retrieve-url-from-point ()
    "Copies the URL from an org link at the point"
    (interactive)
    (let ((plain-url (thing-at-point-url-at-point)))
      (if plain-url
          (progn
            (kill-new plain-url)
            (message (concat "Copied: " plain-url)))
        (let* ((link-info (assoc :link (org-context)))
               (text (when link-info
                       (buffer-substring-no-properties
                        (or (cadr link-info) (point-min))
                        (or (caddr link-info) (point-max))))))
          (if (not text)
              (error "Oops! Point isn't in an org link")
            (string-match org-link-bracket-re text)
            (let ((url (substring text (match-beginning 1) (match-end 1))))
              (kill-new url)
              (message (concat "Copied: " url))))))))
  (setf org-directory +project-maria-dir+
        org-agenda-files
        (cl-loop for agenda-file in
                 '("0inbox.org"
                   "0projects.org"
                   "0solo.org"
                   "0someday.org"
                   "0contacts.org"
                   "0calendar.org"
                   "project-jerome-index.org")
                 collect
                 (concat +project-maria-dir+ "task-management/" agenda-file))
        ;; See org-superstar package for more context
        inhibit-compacting-font-caches t
        ;; https://helpdeskheadesk.net/2022-03-13/
        ;; For org attach, change org timestamps to more human readable format.
        org-id-method 'ts
        org-attach-id-to-path-function-list
        '(org-attach-id-ts-folder-format org-attach-id-uuid-folder-format)
        ;; See Org Manual 16.4 A Cleaner Outline View
        ;; I prefer a book-like view, which also allows for auto-fill
        org-adapt-indentation nil
        org-list-allow-alphabetical t
        org-image-actual-width '600
        org-hide-emphasis-markers nil
        org-footnote-auto-adjust "Renumber and Sort"
        org-persist-directory "~/.cache/org-persist/")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "xR" 'afs/org-replace-link-by-link-description)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "iI" 'org-id-get-create)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "tC" 'org-table-create-or-convert-from-region)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "hn" 'org-next-visible-heading)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "hp" 'org-previous-visible-heading)
  (setq org-sticky-header-full-path 'full)
  ;; Require org-contacts to work with mu4e
  (require 'org-contacts)
  (setf org-contacts-files (list (concat +project-maria-dir+ "task-management/0contacts.org")))
  (require 'org-re-reveal)
  (setq org-re-reveal-revealjs-version "4"
        org-re-reveal-root  "https://cdn.jsdelivr.net/npm/reveal.js")
  (setf org-mime-export-options '(:section-numbers nil
                                                   ;; otherwise tables will not work
                                                   :with-broken-links t
                                                   :with-author nil
                                                   :with-toc nil
                                                   :with-latex dvipng))
  (setq org-export-backends '(ascii html icalendar latex odt beamer man md
                                         org texinfo))
  (setf org-download-method 'attach)
  ;; Add key bindings for org-expiry package
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "dc" 'org-expiry-insert-created)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "de" 'org-expiry-insert-expiry)
  ;; Add call to org-expiry-insert-created every time org-id-get-create is run
  (advice-add 'org-id-get-create :after 'org-expiry-insert-created)
  (org-clock-persistence-insinuate)
  
  (add-hook 'org-clock-in-prepare-hook 'my-org-mode-ask-effort)
  
  (defun my-org-mode-ask-effort ()
    "Ask for an effort estimate when clocking in if none exists."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
             (completing-read
              "Effort: "
              (org-entry-get-multivalued-property (point) "Effort"))))
        (unless (equal effort "")
          (org-set-property "Effort" effort)))))
  
  (defun eos/org-clock-in ()
    (interactive)
    (org-clock-in '(4)))
  
  ;; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  
  ;; https://orgmode.org/worg/org-contrib/org-depend.html
  (defun mm/org-insert-trigger ()
    "Automatically insert chain-find-next trigger when entry becomes NEXT"
    (cond ((equal org-state "NEXT")
           (unless org-depend-doing-chain-find-next
             (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-up)")))
          ((not (member org-state org-done-keywords))
           (org-delete-property "TRIGGER"))))
  (add-hook 'org-after-todo-state-change-hook 'mm/org-insert-trigger)
  
  (spacemacs/set-leader-keys "oa" 'hanshen/default-custom-agenda)
  (spacemacs/set-leader-keys "oj" 'spacemacs/org-clock-jump-to-current-clock)
  (spacemacs/set-leader-keys "oi" 'eos/org-clock-in)
  (spacemacs/set-leader-keys "oI" 'org-clock-in)
  (spacemacs/set-leader-keys "oo" 'org-clock-out)
  (spacemacs/set-leader-keys "or" 'org-resolve-clocks)
  (spacemacs/set-leader-keys "oc" 'org-capture)
  
  ;; Press t to change task todo state
  (setf org-use-fast-todo-selection t
        org-treat-S-cursor-todo-selection-as-state-change t
        ;; Require exit notes for modifying a scheduled for deadline date
        org-log-reschedule 'note
        org-log-redeadline 'note
        org-todo-keywords
        '((sequence "PROJ(p)" "APPT(a)" "TODO(t)" "NEXT(n)" "PROG(i)"
                    "WAIT(w@/!)" "HOLD" "|" "DONE(d)" "CXLD(c@/!)"))
        org-todo-keyword-faces
        '(;; Project Defined
          ("PROJ" :foreground "gold" :weight bold)
          ;; Todo's Brainstormed
          ("TODO" :foreground "tomato" :weight bold)
          ;; Next Action(s) chosen
          ("NEXT" :foreground "RoyalBlue" :weight bold)
          ;; Delegated or out of your control
          ("WAIT" :foreground "magenta" :weight bold)
          ;; Reducing from potential to actual
          ("PROG" :foreground "cyan2" :weight bold)
          ;; Completed task
          ("DONE" :foreground "SpringGreen3" :weight bold)
          ;; Formal appointment, in-person/scheduled in advance
          ;; Of type WAIT, but with a definte deadline
          ("APPT" :foreground "DarkViolet" :weight bold)
          ;; Informal (interruption) meeting/verbal/email
          ;; Informal (interruption) calls/texts
          ;; ("MEET" :foreground "MediumOrchid" :weight bold)
          ;; Decided not to decide, need more information
          ;; BUT with a deadline set.
          ("HOLD" :foreground "orchid" :weight bold)
          ;; Cancelled task, unable to complete
          ("CXLD" :foreground "SaddleBrown" :weight bold))
        org-enforce-todo-dependencies t
        org-agenda-dim-blocked-tasks t
        org-habit-graph-column 80
        org-agenda-skip-scheduled-if-deadline-is-shown t
        ;; 6) Adding New Tasks Quickly with Org Capture
        ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
        ;; \n is newline in the template. Functions as RET would in insert mode
        ;; placing a backslash before " in TRIGGER below to have the string not end
        org-capture-templates
        '(("n" "Next Action" entry (file "~/project-maria/task-management/0inbox.org") "* NEXT [#C] %?%^G\n:PROPERTIES:\n:EFFORT: %^{0:00|0:10|0:30|1:00|1:30|2:00|2:30|3:00|4:00|5:00|6:00|7:00|8:00}\n:ASSIGNED: %U\n:END:\n")
          ("t" "Todo Task" entry (file "~/project-maria/task-management/0inbox.org") "* TODO [#C] %?%^G\n :PROPERTIES:\n:EFFORT: %^{0:00|0:10|0:30|1:00|1:30|2:00|2:30|3:00|4:00|5:00|6:00|7:00|8:00}\n:ASSIGNED: %U\n:END:\n")
          ("p" "New Project" entry (file "~/project-maria/task-management/0projects.org") "* PROJ %? [#C] [/] [%] %^G\n:PROPERTIES:\n:ASSIGNED: %U\n:CATEGORY: %^{CATEGORY|Misc.}\n:END:\n** NEXT [#C]\n:PROPERTIES:\n:TRIGGER: chain-find-next(NEXT,from-current,priority-up,effort-up)\n:EFFORT: %^{0:00|0:10|0:30|1:00|1:30|2:00|2:30|3:00|4:00|5:00|6:00|7:00|8:00}\n:ASSIGNED: %U\n:END:\n")
          ("a" "Set Appointment" entry (file "~/project-maria/task-management/0calendar.org") "* %?\n :PROPERTIES:\n:LOCATION: %^{LOCATION|TBD}\n:calendar-id: contacthanshenwang@gmail.com\n:EFFORT: %^{0:00|0:10|0:30|1:00|1:30|2:00|2:30|3:00|4:00|5:00|6:00|7:00|8:00}\n:ASSIGNED: %U\n:END:\n:org-gcal:\n%^T\n:END:")
          ;; Meeting (ongoing/interruption) should be filed under a specific project
          ;; If it is really an orphan, refile 0solo.org
          ("m" "Meeting (Ongoing)" entry (file "~/project-maria/task-management/0inbox.org") "* MEETING with %?%^G\n:PROPERTIES:\n:ASSIGNED: %U\n:END:\n" :clock-in t :clock-resume t)
          ("h" "Hold Entry" entry (file "~/project-maria/task-management/0someday.org") "* HOLD %?%^G\n SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n:PROPERTIES:\n:EFFORT: %^{0:00|0:10|0:30|1:00|1:30|2:00|2:30|3:00|4:00|5:00|6:00|7:00|8:00}\n:ASSIGNED: %U\n:END:\n")
          ("C" "Add Contacts" entry (file "~/project-maria/task-management/0inbox.org") "* %(org-contacts-template-name)\n:PROPERTIES:\n:PHONE: %?\n:EMAIL:\n:ADDRESS:\n:BIRTHDAY:\n:NOTE: Added on: %U\n:END:")
          ("H" "Habit" entry (file "~/project-maria/task-management/0inbox.org")"* NEXT %?\nSCHEDULED: %(format-time-string \"%\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:ASSIGNED: %U\n:END:")
          ("j" "Journal Entry" entry (file "~/project-maria/task-management/0inbox.org")"* NEXT JOURNAL ENTRY %U\n:PROPERTIES:\n:EFFORT: %^{0:00|0:10|0:30|1:00|1:30|2:00|2:30|3:00|4:00|5:00|6:00|7:00|8:00}\n:ASSIGNED: %U\n :END:\n%?"))
        ;; **** 9) Clocking
        org-clock-in-switch-to-state "PROG"
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t
        org-clock-persist t
        org-clock-in-resume t
        org-clock-persist-query-resume nil
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-report-include-clocking-task t
        org-time-stamp-rounding-minutes '(1 1)
        org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 10 :fileskip0 t :stepskip0 t :compact t :narrow 80)
        org-log-into-drawer t
        org-clock-history-length 35
        ;; **** 7) Refiling Tasks
        org-refile-targets '((nil :maxlevel . 9)
                            (org-agenda-files :maxlevel . 9))
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path 'file
        org-refile-target-verify-function 'bh/verify-refile-target
        ;; **** 11) Context Tags with fast selection keys
        org-tag-alist '(;; Mark for quote export
                        ("exp" . ?x)
                        ;; Sets geo-spatial and context tags
                        ;; Startgroup and endgroup make tags mutually
                        ;; exclusive (:startgroup)
                        ("0errand" . ?e)
                        ("0office" . ?o)
                        ("0calls" . ?c)
                        ("0home" . ?h)
                        ;; (:endgroup)
                        ;; Person(s) can be contexts too.
                        ("0father" . ?d)
                        ("0mother" . ?d)
                        ("0brother" . ?d)
                        ("0family" . ?d)
                        ("0workteam1" . ?d)
                        ("0docket" . ?d)
                        ("REF" . ?r)
                        ("FLAGGED" . ??))
        org-fast-tag-selection-single-key 'expert
        ;; For tag searches ignore tasks with scheduled and deadline dates
        org-agenda-tags-todo-honor-ignore-options t
        ;; **** 14) Stuck Projects
        org-stuck-projects   '("+TODO=\"PROJ\"" ("NEXT") nil nil)
        ;; **** 15) Archiving
        org-archive-default-command 'org-archive-subtree
        org-archive-location
        (concat +project-maria-dir+
                "task-management/archived-tasks/0taskings-"
                (format-time-string "%Y") ".org::datetree/")
        org-archive-save-context-info '(time file category olpath todo priority
                                             ltags itags))
  ;; **** 8) Org Agenda Custom Views
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (define-key
                org-agenda-mode-map (kbd "s")
                'avy-goto-word-or-subword-1)))
  ;; Colorize clocked tasks with a block
  ;; Show Org Agenda tasks with height spacing based on clock time with
  ;; org-agenda-log-mode.
  (require 'cl-lib)
  (defun my/org-agenda-calculate-efforts (limit)
    "Sum the efforts of scheduled entries up to LIMIT in the
  agenda buffer."
    (let (total)
      (save-excursion
        (while (< (point) limit)
          (when (member (org-get-at-bol 'type) '("scheduled" "past-scheduled" "timestamp"))
            (push (org-entry-get (org-get-at-bol 'org-hd-marker) "EFFORT") total))
          (forward-line)))
      (org-duration-from-minutes
       (cl-reduce #'+
                  (mapcar #'org-duration-to-minutes
                          (cl-remove-if-not 'identity total))))))
  (defun my/org-agenda-insert-efforts ()
    "Insert the efforts for each day inside the agenda buffer."
    (save-excursion
      (let (pos)
        (while (setq pos (text-property-any
                          (point) (point-max) 'org-agenda-date-header t))
          (goto-char pos)
          (end-of-line)
          (insert-and-inherit (concat " ("
                                      (my/org-agenda-calculate-efforts
                                       (next-single-property-change (point) 'day))
                                      ")"))
          (forward-line)))))
  
  (add-hook 'org-agenda-finalize-hook 'my/org-agenda-insert-efforts)
  ;; (defun org-agenda-log-mode-colorize-block ()
  ;;   "Set different line spacing and coloring based on clock time duration."
  ;;   (save-excursion
  ;;     (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
  ;;                      ('light
  ;;                       (list "#FFE6E6" "#FFFFCC" "#CCFFCC" "#CCFFFF"))
  ;;                      ('dark
  ;;                       (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
  ;;            pos
  ;;            duration)
  ;;       (nconc colors colors)
  ;;       (goto-char (point-min))
  ;;       (while (setq pos (next-single-property-change (point) 'duration))
  ;;         (goto-char pos)
  ;;         (when (and (not (equal pos (point-at-eol)))
  ;;                    (setq duration (org-get-at-bol 'duration)))
  ;;           ;; larger duration bar height
  ;;           (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 60))))
  ;;                 (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
  ;;             (overlay-put ov 'face `(:background ,(car colors)))
  ;;             (setq colors (cdr colors))
  ;;             (overlay-put ov 'line-height line-height)
  ;;             (overlay-put ov 'line-spacing (1- line-height))))))))
  ;; (add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode-colorize-block)
  ;; Source:  https://emacs.stackexchange.com/questions/51708/is-there-a-way-to-start-org-mode-agenda-with-a-custom-search
  (defun hanshen/default-custom-agenda()
    "Functionally call custom agenda command bound to KEY"
    (interactive)
    (org-agenda nil "d"))
  ;; Shortens the agenda seperator line
  (setq org-agenda-block-separator 61)
  (setq org-agenda-breadcrumbs-separator " | ")
  ;; Add %b if you want breadcrumbs in agenda items. I.e. goal1 | subgoal 2 |...
  ;; (agenda . "%?-3b%-t %? e%s")
  ;; Deadlines in agenda view source: https://stackoverflow.com/questions/58820073/s-in-org-agenda-prefix-format-doesnt-display-dates-in-the-todo-view
  (setq org-agenda-prefix-format
        (quote
         ((agenda . "%-t %? e%c%s")
          (todo . "%? e%c%s%(let ((scheduled (org-get-deadline-time (point)))) (if scheduled (format-time-string \" [%Y-%m-%d] \" scheduled) \"\"))")
          (tags . "%? e%c%s%(let ((scheduled (org-get-deadline-time (point)))) (if scheduled (format-time-string \" [%Y-%m-%d] \" scheduled) \"\"))")
          (search . "%? e%c%s"))
         ))
  (setq org-agenda-deadline-leaders (quote ("!D!: " "D%3d: " "")))
  (setq org-agenda-scheduled-leaders (quote ("!S!: " "S%3d: " "")))
  ;; Customize time-grid of Agenda
  (setq org-agenda-time-grid (quote ((daily today remove-match)
                                     (0600 0900 1200 1500 1800 2100)
                                     "......" "----------------")))
  ;; Modify columns in org-agenda-clock-report-mode
  ;; Add effort property to all agenda entries
  ;; When columnizing org tasks, we can format how they look like this.
  (setq org-columns-default-format-for-agenda "%75ITEM(Task) %10Effort(Estim){:} %10CLOCKSUM(ActTime) %5TODO(State)")
  (setq org-columns-default-format "%75ITEM(Task) %10Effort(Estim){:} %10CLOCKSUM(ActTime) %5TODO(State)")
  ;; Set global properties that apply to all org entries' autocompletes
  (setq org-global-properties '(quote (("Effort_ALL" . "0:00 0:10 0:30 1:00 1:30 2:00 2:30 3:00 4:00 5:00 6:00 7:00 8:00")
                                       ("STYLE_ALL" . "habit"))))
  ;; In the estimated work effort for a day, add appointments that have a
  ;; specified time interval.
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  ;; Setting the default duration of timestamps which have no ending timestamp
  (setq org-agenda-default-appointment-duration 0)
  ;; In Org Agenda Log Mode, show all the information about them that we can.
  (setq org-agenda-log-mode-items '(closed state clock))
  (setq org-agenda-start-with-log-mode t)
  ;; Entry text mode displays the body text of an org heading
  (setq org-agenda-start-with-entry-text-mode t)
  ;; Export body text in agenda view
  (setq org-agenda-add-entry-text-maxlines 5)
  ;; Maximum lines shown are set with this variable
  (setq org-agenda-entry-text-maxlines 5)
  ;; In Org Agenda Clock Report Mode, start new agenda buffer with this on
  ;; This is no longer needed, obsoleted by function above =org-agenda-log-mode-colorize-block=
  (setq org-agenda-start-with-clockreport-mode nil)
  ;; Function found here https://emacs.stackexchange.com/questions/21380/show-sum-of-efforts-for-a-day-in-org-agenda-day-title
  
  ;; Set default column properties for org agenda
  ;; Context Padding Function
  ;; If my elisp improves, I'll modify the function to
  ;; include not just the first element (car) of the list
  ;; but the entire list.
  ;; (defun sl-get-padded-todo-parent (size)
  ;;   "Return string of length SIZE containing either padded or truncated parent name."
  ;;   (let* ((parent (car (org-get-outline-path)))
  ;;          (padding (- size (length parent))))
  ;;     (if (< padding 0) (substring parent 0 size)
  ;;       (concat parent (make-string padding ?\ )))))
  ;; ;; Insert this to override the default.
  ;; (org-agenda-prefix-format "%(sl-get-padded-todo-parent 12): ")
  (setq org-agenda-custom-commands
        '(
          ;; ***** Default Agenda
          ("d" "Default (Master) Agenda"
           ;; Presents the same view as Google Calendar i.e. only APPT and Routine Events
           ;; Inclusive of all tags. I can't bi-locate after all.
           (;; Presents Scheduled and Deadline Items, includes Overdue Items
            ;; Exclusive to 0home tasks
            (agenda "" ((org-agenda-span 1)                      ; daily agenda
                        ;; 7 day warning for deadlines, adjust period based on clutter
                        (org-deadline-warning-days 7)
                        ;; (org-agenda-todo-keyword-format "[ ]")
                        (org-agenda-overriding-header "Today's Agenda\n")
                        ;; (org-agenda-scheduled-leaders '("" ""))
                        ))
            ;; Presents in progress tasks for easy access to clock in
            (tags "TODO=\"PROG\""
                  (
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nAll Tasks in Progress\n")
                   ))
            ;; Presents NEXT 0home tasks
            (tags "TODO=\"NEXT\""
                  (
                   ;; Sorting for [A] Priority items, then by upcoming deadlines
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nAll Action Items\n")
                   ;; Skip listing a next/todo item that has already been scheduled
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                   ))
            ;; Presents 0home WAIT's sorted by oldest first
            ;; This is as it really should have a scheduled or
            ;; deadline date, but sometimes you really don't know
            ;; If given a scheduled or deadline date it will show up in
            ;; the day's agenda. Make sure to include an active timestamp
            ;; when switching states
            ;; Make sure to write down what you are waiting for
            (tags "+TODO=\"WAIT\""
                  (
                   (org-agenda-sorting-strategy '(timestamp-down))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nAll Delegated/Waiting For\n")
                   ))
            ;; Presents 0home PROJ's grouped by categories
            (tags "TODO=\"TODO\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nAll Tasks\n")
                   ))
            ;; Presents 0home PROJ's grouped by categories
            (tags "TODO=\"PROJ\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nAll Projects\n")
                   ))
            ;; Presents Cancelled and Stuck Projects
            (tags "TODO=\"CXLD\""
                  (
                   (org-agenda-sorting-strategy '(tsia-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nTerminated Tasks\n")
                   ))
            (stuck ""
                   (
                    (org-agenda-overriding-header "\nStuck Projects\n")
                    ))
            (agenda "" (
                        ;; Increase or decrease if cluttered or empty
                        (org-agenda-span '33)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")         ;; calendar begins tomorrow
                        (org-agenda-entry-types '(:timestamp :sexp :scheduled))
                        (org-agenda-overriding-header "Routine & Appointments\n")
                        ))
            )
           ((org-agenda-tag-filter-preset '("-SDAY"))))
          ;; ***** Home Agenda
          ("h" "Home Agenda"
           ;; Presents the same view as Google Calendar i.e. only APPT and Routine Events
           ;; Inclusive of all tags. I can't bi-locate after all.
           (;; Presents Scheduled and Deadline Items, includes Overdue Items
            ;; Exclusive to 0home tasks
            (agenda "" ((org-agenda-span 1)                      ; daily agenda
                        ;; 7 day warning for deadlines, adjust period based on clutter
                        (org-deadline-warning-days 7)
                        ;; (org-agenda-todo-keyword-format "[ ]")
                        (org-agenda-overriding-header "Today's Agenda\n")
                        ;; (org-agenda-scheduled-leaders '("" ""))
                        ))
            ;; Presents in progress tasks for easy access to clock in
            (tags "0home+TODO=\"PROG\""
                  (
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0home Tasks in Progress\n")
                   ))
            ;; Presents NEXT 0home tasks
            (tags "0home+TODO=\"NEXT\""
                  (
                   ;; Sorting for [A] Priority items, then by upcoming deadlines
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0home Action Items\n")
                   ;; Skip listing a next/todo item that has already been scheduled
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                   ))
            ;; Presents 0home WAIT's sorted by oldest first
            ;; This is as it really should have a scheduled or
            ;; deadline date, but sometimes you really don't know
            ;; If given a scheduled or deadline date it will show up in
            ;; the day's agenda. Make sure to include an active timestamp
            ;; when switching states
            ;; Make sure to write down what you are waiting for
            (tags "0home+TODO=\"WAIT\""
                  (
                   (org-agenda-sorting-strategy '(timestamp-down))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0home Delegated/Waiting For\n")
                   ))
            ;; Presents 0home PROJ's grouped by categories
            (tags "0home+TODO=\"TODO\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0home Tasks\n")
                   ))
            ;; Presents 0home PROJ's grouped by categories
            (tags "0home+TODO=\"PROJ\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0home Projects\n")
                   ))
            ;; Presents Cancelled and Stuck Projects
            (tags "0home+TODO=\"CXLD\""
                  (
                   (org-agenda-sorting-strategy '(tsia-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nTerminated Tasks\n")
                   ))
            (stuck ""
                   (
                    (org-agenda-overriding-header "\nStuck Projects\n")
                    ))
            (agenda "" (
                        ;; Increase or decrease if cluttered or empty
                        (org-agenda-span '33)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")         ;; calendar begins tomorrow
                        (org-agenda-entry-types '(:timestamp :sexp :scheduled))
                        (org-agenda-overriding-header "Routine & Appointments\n")
                        ))
  
            )
           ((org-agenda-tag-filter-preset '("-SDAY")))
           )
          ;; ***** Office Agenda
          ("o" "Office Agenda"
           ;; Presents the same view as Google Calendar i.e. only APPT and Routine Events
           ;; Inclusive of all tags. I can't bi-locate after all.
           (;; Presents Scheduled and Deadline Items, includes Overdue Items
            ;; Exclusive to 0home tasks
            (agenda "" ((org-agenda-span 1)                      ; daily agenda
                        ;; 7 day warning for deadlines, adjust period based on clutter
                        (org-deadline-warning-days 7)
                        ;; (org-agenda-todo-keyword-format "[ ]")
                        (org-agenda-overriding-header "Today's Agenda\n")
                        ;; (org-agenda-scheduled-leaders '("" ""))
                        ))
            ;; Presents in progress tasks for easy access to clock in
            (tags "0office+TODO=\"PROG\""
                  (
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0office Tasks in Progress\n")
                   ))
            ;; Presents NEXT 0office tasks
            (tags "0office+TODO=\"NEXT\""
                  (
                   ;; Sorting for [A] Priority items, then by upcoming deadlines
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0office Action Items\n")
                   ;; Skip listing a next/todo item that has already been scheduled
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                   ))
            ;; Presents 0office WAIT's sorted by oldest first
            ;; This is as it really should have a scheduled or
            ;; deadline date, but sometimes you really don't know
            ;; If given a scheduled or deadline date it will show up in
            ;; the day's agenda. Make sure to include an active timestamp
            ;; when switching states
            ;; Make sure to write down what you are waiting for
            (tags "0office+TODO=\"WAIT\""
                  (
                   (org-agenda-sorting-strategy '(timestamp-down))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0office Delegated/Waiting For\n")
                   ))
            ;; Presents 0office PROJ's grouped by categories
            (tags "0office+TODO=\"TODO\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0office Tasks\n")
                   ))
            ;; Presents 0office PROJ's grouped by categories
            (tags "0office+TODO=\"PROJ\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0office Projects\n")
                   ))
            ;; Presents Cancelled and Stuck Projects
            (tags "0office+TODO=\"CXLD\""
                  (
                   (org-agenda-sorting-strategy '(tsia-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nTerminated Tasks\n")
                   ))
            (stuck ""
                   (
                    (org-agenda-overriding-header "\nStuck Projects\n")
                    ))
            (agenda "" (
                        ;; Increase or decrease if cluttered or empty
                        (org-agenda-span '33)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")         ;; calendar begins tomorrow
                        (org-agenda-entry-types '(:timestamp :sexp :scheduled))
                        (org-agenda-overriding-header "Routine & Appointments\n")
                        ))
            )
           ((org-agenda-tag-filter-preset '("-SDAY")))
           )
          ;; ***** Errand Agenda
          ("e" "Errand Agenda"
           ;; Presents the same view as Google Calendar i.e. only APPT and Routine Events
           ;; Inclusive of all tags. I can't bi-locate after all.
           (;; Presents Scheduled and Deadline Items, includes Overdue Items
            ;; Exclusive to 0home tasks
            (agenda "" ((org-agenda-span 1)                      ; daily agenda
                        ;; 7 day warning for deadlines, adjust period based on clutter
                        (org-deadline-warning-days 7)
                        ;; (org-agenda-todo-keyword-format "[ ]")
                        (org-agenda-overriding-header "Today's Agenda\n")
                        ;; (org-agenda-scheduled-leaders '("" ""))
                        ))
            ;; Presents in progress tasks for easy access to clock in
            (tags "0errand+TODO=\"PROG\""
                  (
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0errand Tasks in Progress\n")
                   ))
            ;; Presents NEXT 0errand tasks
            (tags "0errand+TODO=\"NEXT\""
                  (
                   ;; Sorting for [A] Priority items, then by upcoming deadlines
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0errand Action Items\n")
                   ;; Skip listing a next/todo item that has already been scheduled
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                   ))
            ;; Presents 0errand WAIT's sorted by oldest first
            ;; This is as it really should have a scheduled or
            ;; deadline date, but sometimes you really don't know
            ;; If given a scheduled or deadline date it will show up in
            ;; the day's agenda. Make sure to include an active timestamp
            ;; when switching states
            ;; Make sure to write down what you are waiting for
            (tags "0errand+TODO=\"WAIT\""
                  (
                   (org-agenda-sorting-strategy '(timestamp-down))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0errand Delegated/Waiting For\n")
                   ))
            ;; Presents 0errand PROJ's grouped by categories
            (tags "0errand+TODO=\"TODO\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0errand Tasks\n")
                   ))
            ;; Presents 0errand PROJ's grouped by categories
            (tags "0errand+TODO=\"PROJ\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0errand Projects\n")
                   ))
            ;; Presents Cancelled and Stuck Projects
            (tags "0errand+TODO=\"CXLD\""
                  (
                   (org-agenda-sorting-strategy '(tsia-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nTerminated Tasks\n")
                   ))
            (stuck ""
                   (
                    (org-agenda-overriding-header "\nStuck Projects")
                    ))
            (agenda "" (
                        ;; Increase or decrease if cluttered or empty
                        (org-agenda-span '33)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")         ;; calendar begins tomorrow
                        (org-agenda-entry-types '(:timestamp :sexp :scheduled))
                        (org-agenda-overriding-header "Routine & Appointments\n")
                        ))
            )
           ((org-agenda-tag-filter-preset '("-SDAY")))
           )
          ;; ***** Home Agenda
          ("c" "Mobile Agenda"
           ;; Presents the same view as Google Calendar i.e. only APPT and Routine Events
           ;; Inclusive of all tags. I can't bi-locate after all.
           (;; Presents Scheduled and Deadline Items, includes Overdue Items
            ;; Exclusive to 0home tasks
            (agenda "" ((org-agenda-span 1)                      ; daily agenda
                        ;; 7 day warning for deadlines, adjust period based on clutter
                        (org-deadline-warning-days 7)
                        ;; (org-agenda-todo-keyword-format "[ ]")
                        (org-agenda-overriding-header "Today's Agenda\n")
                        ;; (org-agenda-scheduled-leaders '("" ""))
                        ))
            ;; Presents in progress tasks for easy access to clock in
            (tags "0calls+TODO=\"PROG\""
                  (
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0calls Tasks in Progress\n")
                   ))
            ;; Presents NEXT 0calls tasks
            (tags "0calls+TODO=\"NEXT\""
                  (
                   ;; Sorting for [A] Priority items, then by upcoming deadlines
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0calls Action Items\n")
                   ;; Skip listing a next/todo item that has already been scheduled
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                   ))
            ;; Presents 0calls WAIT's sorted by oldest first
            ;; This is as it really should have a scheduled or
            ;; deadline date, but sometimes you really don't know
            ;; If given a scheduled or deadline date it will show up in
            ;; the day's agenda. Make sure to include an active timestamp
            ;; when switching states
            ;; Make sure to write down what you are waiting for
            (tags "0calls+TODO=\"WAIT\""
                  (
                   (org-agenda-sorting-strategy '(timestamp-down))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0calls Delegated/Waiting For\n")
                   ))
            ;; Presents 0calls PROJ's grouped by categories
            (tags "0calls+TODO=\"TODO\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0calls Tasks\n")
                   ))
            ;; Presents 0calls PROJ's grouped by categories
            (tags "0calls+TODO=\"PROJ\""
                  (
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\n0calls Projects\n")
                   ))
            ;; Presents Cancelled and Stuck Projects
            (tags "0calls+TODO=\"CXLD\""
                  (
                   (org-agenda-sorting-strategy '(tsia-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nTerminated Tasks\n")
                   ))
            (stuck ""
                   (
                    (org-agenda-overriding-header "\nStuck Projects\n")
                    ))
            (agenda "" (
                        ;; Increase or decrease if cluttered or empty
                        (org-agenda-span '33)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")         ;; calendar begins tomorrow
                        (org-agenda-entry-types '(:timestamp :sexp :scheduled))
                        (org-agenda-overriding-header "Routine & Appointments\n")
                        ))
            )
           ((org-agenda-tag-filter-preset '("-SDAY")))
           )
          ;; ***** Review Agenda
          ("r" "Review Agenda"
           ;; Presents the same view as Google Calendar i.e. only APPT and Routine Events
           ;; Inclusive of all tags. I can't bi-locate after all.
           (;; Presents Scheduled and Deadline Items, includes Overdue Items
            ;; Exclusive to 0home tasks
            ;; Presents in progress tasks for easy access to clock in
            (tags "TODO=\"DONE\"-LIB"
                  (
                   (org-agenda-sorting-strategy '(priority-down deadline-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nCompleted Tasks\n")
                   ))
            ;; Presents Cancelled and Stuck Projects
            (tags "TODO=\"CXLD\""
                  (
                   (org-agenda-sorting-strategy '(tsia-up))
                   (org-agenda-todo-keyword-format "%-3s")
                   (org-agenda-overriding-header "\nTerminated Tasks\n")
                   ))
            (stuck ""
                   (
                    (org-agenda-overriding-header "\nStuck Projects\n")
                    ))
            (agenda "" (
                        ;; Increase or decrease if cluttered or empty
                        (org-agenda-span '33)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")         ;; calendar begins tomorrow
                        (org-agenda-entry-types '(:timestamp :sexp :scheduled))
                        (org-agenda-overriding-header "Routine & Appointments\n")
                        ))
            )
           ((org-agenda-tag-filter-preset '("-SDAY -LIB")))
           )
          ))
  ;; Following 2 lines are needed to exclude parent heading from table of contents but still export the content
  ;; https://emacs.stackexchange.com/questions/30183/orgmode-export-skip-ignore-first-headline-level
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  ;; Allows exporting bibtex citations to html
  (require 'ox-bibtex)
  ;; Exclude default CSS from html export and add external stylesheet
  (setq org-html-head-include-default-style nil)
  ;; Omit inline css as we use an imported stylesheet
  (setq org-html-htmlize-output-type 'css)
  ;; https://www.taingram.org/blog/org-mode-blog.html
  (setq org-export-global-macros
        '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))
  (defun my/org-sitemap-date-entry-format (entry style project)
    "Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that includes date."
    (let ((filename (org-publish-find-title entry project)))
      (if (= (length filename) 0)
          (format "*%s*" entry)
        (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
                (format-time-string "%Y-%m-%d"
                                    (org-publish-find-date entry project))
                entry
                filename))))
  (setq org-publish-project-alist
        '(("blog"
           :components ("blog-content" "blog-rss"))
          ("blog-content"
           :base-directory "~/project-maria/blog"
           :html-extension "html"
           :base-extension "org"
           :recursive t
           :publishing-function org-html-publish-to-html
           :publishing-directory "~/common-lisp/project-isidore/assets/blog"
           :section-numbers t
           :table-of-contents t
           :exclude "rss.org"
           :with-title nil
           :auto-sitemap t
           :sitemap-filename "archive.org"
           :sitemap-title "Blog Archive"
           :sitemap-sort-files anti-chronologically
           :sitemap-style tree
           :sitemap-format-entry my/org-sitemap-date-entry-format
           ;; Use HTML5
           ;; https://orgmode.org/manual/HTML-doctypes.html#HTML-doctypes
           :html-doctype "html5"
           :html-html5-fancy t
           ;; Link to external custom stylesheet
           ;; If you need code highlight from highlight.js, include the latter three lines.
           :html-head "
                      <link rel=\"stylesheet\" type=\"text/css\" href=\"../global.css\"/>
                      <link rel=\"stylesheet\"
                            href=\"//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.2.0/styles/base16/solarized-light.min.css\">
                      <script src=\"//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.2.0/highlight.min.js\" defer></script>
                      <script>var hlf=function(){Array.prototype.forEach.call(document.querySelectorAll(\"pre.src\"),function(t){var e;e=t.getAttribute(\"class\"),e=e.replace(/src-(\w+)/,\"src-$1 $1\"),console.log(e),t.setAttribute(\"class\",e),hljs.highlightBlock(t)})};addEventListener(\"DOMContentLoaded\",hlf);</script>"
           :html-preamble "
                                    <div class=\"header header-fixed\">
                                      <div class=\"navbar container\">
                                        <div class=\"logo\"><a href=\"/\">BHW</a></div>
                                        <input type=\"checkbox\" id=\"navbar-toggle\" >
                                        <label for=\"navbar-toggle\"><i></i></label>
                                        <nav class=\"menu\">
                                          <ul>
                                            <li><a href=\"/about\">About</a></li>
                                            <li><a href=\"/work\">Work</a></li>
                                            <li><a href=\"/assets/blog/archive.html\">Blog</a></li>
                                            <li><a href=\"/contact\">Contact</a></li>
                                          </ul>
                                        </nav>
                                      </div>
                                    </div>
                                    <h1 class=\"title\">%t</h1>
                                    <p class=\"subtitle\">%s</p> <br/>
                                    <p class=\"updated\"><a href=\"/contact#article-history\">Updated:</a> %C</p>"
  
           ;; Article Postamble includes
           ;; Javascript snippet to insert anchor links to Table of Contents
           ;; HTML Footer
           :html-postamble "<script>
                              const headers = Array.from( document.querySelectorAll('h2, h3, h4, h5, h6') );
  
                              headers.forEach( header => {
                                header.insertAdjacentHTML('afterbegin',
                                 '<a href=\"#table-of-contents\">&#8689;</a>'
                                );
                              });
                              </script>
                              <hr/>
                              <footer>
                                <div class=\"copyright-container\">
                                    Comments? Corrections? <a href=\"https://bhw.name/contact\"> Please do reach out.</a><a href=\"https://bhw.name/blog/rss.xml\"> RSS Feed. </a><a href=\"https://bhw.name/subscribe\"> Mailing List. </a><br/>
                                    Copyright 2021 Benedict Hanshen Wang. <br/>
                                    Blog content is available under <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\"> CC-BY-SA 4.0 </a> unless otherwise noted.<br/>
                                    Created with %c on <a href=\"https://www.gnu.org\">GNU</a>/<a href=\"https://www.kernel.org/\">Linux</a><br/>
                                </div>
                              </footer>"
           )
          ("blog-rss"
           :base-directory "~/project-maria/blog"
           :base-extension "org"
           :publishing-directory "~/common-lisp/project-isidore/assets/blog"
           :publishing-function publish-posts-rss-feed
           :rss-extension "xml"
           :html-link-home "http://bhw.name/"
           :html-link-use-abs-url t
           :html-link-org-files-as-html t
           :exclude "archive.org"
           :auto-sitemap t
           :sitemap-function posts-rss-feed
           :sitemap-title "Benedict Hanshen Wang Blog RSS"
           :sitemap-filename "rss.org"
           :sitemap-style list
           :sitemap-sort-files anti-chronologically
           :sitemap-format-entry format-posts-rss-feed-entry)
          ))
  ;; https://alhassy.github.io/AlBasmala#Clickable-Headlines
  (defun my/ensure-headline-ids (&rest _)
    "Org trees without a custom ID will have
                              All non-alphanumeric characters are cleverly replaced with ‘-’.
  
                              If multiple trees end-up with the same id property, issue a
                              message and undo any property insertion thus far.
  
                              E.g., ↯ We'll go on a ∀∃⇅ adventure
                                 ↦  We'll-go-on-a-adventure
                              "
    (interactive)
    (let ((ids))
      (org-map-entries
       (lambda ()
         (org-with-point-at (point)
           (let ((id (org-entry-get nil "CUSTOM_ID")))
             (unless id
               (thread-last (nth 4 (org-heading-components))
                 (s-replace-regexp "[^[:alnum:]']" "-")
                 (s-replace-regexp "-+" "-")
                 (s-chop-prefix "-")
                 (s-chop-suffix "-")
                 (setq id))
               (if (not (member id ids))
                   (push id ids)
                 (message-box "Oh no, a repeated id!\n\n\t%s" id)
                 (undo)
                 (setq quit-flag t))
               (org-entry-put nil "CUSTOM_ID" id))))))))
  
  ;; Whenever html & md export happens, ensure we have headline ids.
  (advice-add 'org-html-export-to-html   :before 'my/ensure-headline-ids)
  (advice-add 'org-md-export-to-markdown :before 'my/ensure-headline-ids)
  ;; https://nicolasknoebber.com/posts/blogging-with-emacs-and-org.html
  (defun format-posts-rss-feed-entry (entry _style project)
    "Format ENTRY for the posts RSS feed in PROJECT."
    (org-publish-initialize-cache "blog-rss")
    (let* ((title (org-publish-find-title entry project))
           (link (concat "blog/" (file-name-sans-extension entry) ".html"))
           (author (org-publish-find-property entry :author project))
           (pubdate (format-time-string (car org-time-stamp-formats)
                                        (org-publish-find-date entry project))))
      (message pubdate)
      (format "%s
                :properties:
                :rss_permalink: %s
                :author: %s
                :pubdate: %s
                :end:\n"
              title
              link
              author
              pubdate)))
  (defun posts-rss-feed (title list)
    "Generate a sitemap of posts that is exported as a RSS feed.
                TITLE is the title of the RSS feed.  LIST is an internal
                representation for the files to include.  PROJECT is the current
                project."
    (concat
     "#+TITLE: " title "\n#+EMAIL: admin@bhw.name" "\n\n"
     (org-list-to-subtree list)))
  (defun publish-posts-rss-feed (plist filename dir)
    "Publish PLIST to RSS when FILENAME is rss.org.
                DIR is the location of the output."
    (if (equal "rss.org" (file-name-nondirectory filename))
        (org-rss-publish-to-rss plist filename dir)))
  ;;-------------------------------------------------------------------------
  ;; ***  Org Tanglesync Config
  ;;-------------------------------------------------------------------------
  (add-hook 'org-mode 'org-tanglesync-mode)
  (add-hook 'prog-mode 'org-tanglesync-watch-mode)
  (add-hook 'text-mode 'org-tanglesync-watch-mode)
  (setf org-tanglesync-default-diff-action ':diff
        org-tanglesync-watch-files '("dotfiles.org"))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "bS" 'org-tanglesync-process-buffer-interactive)
  ;; Fix for wget option flags, as per https://github.com/alphapapa/org-web-tools/issues/35
  (setq org-web-tools-archive-wget-options '("--ignore-tags=script,iframe" "--reject=eot,ttf,svg,otf,*.woff*" "--execute" "robots=off" "--adjust-extension" "--span-hosts" "--convert-links" "--page-requisites" "--timestamping" "--no-directories"))
  (setq org-web-tools-archive-wget-html-only-options '("--execute" "robots=off" "--adjust-extension" "--timestamping" "--no-directories"))
  ;; For when you are lost in a long code block
  (spacemacs/set-leader-keys "aob" 'org-babel-goto-src-block-head)
  ;; Used below to rename org edit blocks
  ;; https://emacs.stackexchange.com/questions/2483/referring-to-the-org-babel-src-block-name-from-within-the-script
  ;; EDIT please change function so that src blocks with no name get a temporary
  ;;name. otherwise code highlighting is broken.
  (defun org-src--construct-edit-buffer-name (org-buffer-name lang)
    "Construct the buffer name for a source editing buffer."
    (concat (nth 4 (org-babel-get-src-block-info)) " [" lang "]"))
  ;; =SPC h d v "org-babel-load-languages" shows that emacs-lisp and
  ;; shell code is already enabled in org-babel.
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((ledger . t)
                                         (calc . t)
                                         (js . t)
                                         (emacs-lisp . t)
                                         (shell . t)
                                         (lisp . t)
                                         ;; (mathematica . t)
                                         (latex . t)
                                         ;; (jupyter . t) ;; must be last
                                         )))
  ;; Sanitize output and deal with paths
  (setq org-babel-mathematica-command (concat +project-maria-dir+ "mash.pl"))
  ;; Font-locking
  ;; (add-to-list 'org-src-lang-modes '("mathematica" . wolfram))
  ;; (autoload 'wolfram-mode "wolfram-mode" nil t)
  ;; (autoload 'run-wolfram "wolfram-mode" nil t)
  ;; (setq wolfram-program "/home/hanshen/Wolfram/WolframEngine/12.2/Executables/WolframKernel")
  ;; (add-to-list 'auto-mode-alist '("\.m$" . wolfram-mode))
  ;; (setq wolfram-path "~/.WolframEngine/Applications") ;; e.g. on Linux ~/.Mathematica/Applications
  ;; For wolfram-mode
  ;; (setq mathematica-command-line "~/project-maria/mash.pl")
  ;; (setq org-confirm-babel-evaluate nil)
  ;; (setq org-babel-python-command "/usr/bin/python3")
  ;; enable proper mode for sagemath code blocks
  ;; (add-to-list 'org-src-lang-modes '("jupyter-sage" . python))
  ;; See library of babel > org babel org heading for more detail
  (defun org-in-tangle-dir (sub-path)
    "Expand the SUB-PATH into the directory given by the tangle-dir
           property if that property exists, else use the
           `default-directory'."
    (expand-file-name sub-path
                      (or
                       (org-entry-get (point) "tangle-dir" 'inherit)
                       (default-directory))))
  (setf org-confirm-babel-evaluate nil)
  ;;-------------------------------------------------------------------------
  ;; ***  Org Recoll Config
  ;;-------------------------------------------------------------------------
  ;; (load "~/.emacs.d/private/local/org-recoll.el")
  ;; (setq org-recoll-results-num 50)
  ;; (spacemacs/set-leader-keys "ss" 'org-recoll-search)
  (setq org-attach-id-dir "~/project-jerome/org-attach-data/" )
  (setq org-attach-preferred-new-method 'dir)
  (setf org-noter-always-create-frame nil
        org-noter-hide-other nil
        org-noter-auto-save-last-location t)
  
  (spacemacs/set-leader-keys
    "aon" 'org-noter)
  (require 'org-brain)
  (require 'org-expiry)
  ;; Add CREATED property when adding a new org-brain headline entry
  (add-hook 'org-brain-new-entry-hook #'org-expiry-insert-created)
  
  (spacemacs/set-leader-keys "o SPC" 'org-brain-visualize-dwim)
  ;; For evil users,
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  ;; Automatically add ID properties to all org headlines when saving
  ;; Disabled because of slowdown, use org-id-get-create instead
  ;; (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Org-Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (defun org-expiry-created-comp (a b)
    "Compare `org-expiry-created-property-name' properties of A and B."
    (let ((ta (ignore-errors
                (org-time-string-to-seconds
                 (org-entry-get (get-text-property 0 'org-marker a)
                                org-expiry-created-property-name))))
          (tb (ignore-errors
                (org-time-string-to-seconds
                 (org-entry-get (get-text-property 0 'org-marker b)
                                org-expiry-created-property-name)))))
      (cond ((if ta (and tb (< ta tb)) tb) -1)
            ((if tb (and ta (< tb ta)) ta) +1))))
  
  (defun org-brain-timeline ()
    "List all org-brain headlines in chronological order."
    (interactive)
    (let ((org-agenda-files (org-brain-files))
          (org-agenda-cmp-user-defined #'org-expiry-created-comp)
          (org-agenda-sorting-strategy '(user-defined-down)))
      (org-tags-view nil (format "+%s>\"\"" org-expiry-created-property-name))))
  
  (defun org-brain-cliplink-resource ()
    "Add a URL from the clipboard as an org-brain resource.
                  Suggest the URL title as a description for resource."
    (interactive)
    (let ((url (org-cliplink-clipboard-content)))
      (org-brain-add-resource
       url
       (org-cliplink-retrieve-title-synchronously url)
       t)))
  
  (define-key org-brain-visualize-mode-map (kbd "L") #'org-brain-cliplink-resource)
  
  ;; Prettify the lines via aa2u package, or ascii art to unicode
  (defface aa2u-face '((t . nil))
    "Face for aa2u box drawing characters")
  (advice-add #'aa2u-1c :filter-return
              (lambda (str) (propertize str 'face 'aa2u-face)))
  (defun aa2u-org-brain-buffer ()
    (let ((inhibit-read-only t))
      (make-local-variable 'face-remapping-alist)
      (add-to-list 'face-remapping-alist
                   '(aa2u-face . org-brain-wires))
      (ignore-errors (aa2u (point-min) (point-max)))))
  (with-eval-after-load 'org-brain
    (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer))
  (define-key org-brain-visualize-mode-map (kbd "j") #'evil-scroll-page-down)
  (define-key org-brain-visualize-mode-map (kbd "k") #'evil-scroll-page-up)
  (define-key org-brain-visualize-mode-map (kbd "i") #'org-brain-select-map)
  (define-key org-brain-visualize-mode-map (kbd "I") #'org-brain-select-dwim)
  (define-key org-brain-visualize-mode-map (kbd "s") #'link-hint-open-link)
  ;; Org-brain initialization
  (setf org-brain-path +project-maria-dir+
        org-id-track-globally t
        org-brain-data-file "~/.emacs.d/.cache/.org-brain-data.el"
        org-id-locations-file "~/.emacs.d/.cache/.org-id-locations"
        org-brain-visualize-default-choices 'all
        org-brain-title-max-length 90
        org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil
        org-brain-headline-entry-name-format-string "%2$s"
        org-brain-quit-after-goto t
        org-brain-backlink "<--"
        org-expiry-inactive-timestamps t)
  (use-package poly-org :after org) ; Should be after org mode config.
  ;; ;; (setq org-gcal-client-id "694392086923-tp9vlkjdl9k037ueocv0ihv284gukbo2.apps.googleusercontent.com"
  ;; ;;       org-gcal-client-secret "1pxJTZ48qJWg3DmV8qltAU3b"
  ;; ;;       org-gcal-file-alist '(("contacthanshenwang@gmail.com" .  "~/Dropbox/project-maria/task-management/0calendar.org")))
  ;; ;; ;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  ;; ;; ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  ;; ;; ;; Nests repeating weekly events
  ;; ;; (setq org-gcal-recurring-events-mode 'nested)
  ;; ;; ;; Turn off auto-archive
  ;; ;; (setq org-gcal-auto-archive nil)
  ;; ;; ;; Fetch events 90 days after, and 0 before
  ;; ;; (setq org-gcal-down-days '90)
  ;; ;; (setq org-gcal-up-days '0)
  ;; ;; (defun my-org-gcal-set-effort (_calendar-id event _update-mode)
  ;; ;;   "Set Effort property based on EVENT if not already set."
  ;; ;;   (when-let* ((stime (plist-get (plist-get event :start)
  ;; ;;                                 :dateTime))
  ;; ;;               (etime (plist-get (plist-get event :end)
  ;; ;;                                 :dateTime))
  ;; ;;               (diff (float-time
  ;; ;;                      (time-subtract (org-gcal--parse-calendar-time-string etime)
  ;; ;;                                     (org-gcal--parse-calendar-time-string stime))))
  ;; ;;               (minutes (floor (/ diff 60))))
  ;; ;;     (let ((effort (org-entry-get (point) org-effort-property)))
  ;; ;;       (unless effort
  ;; ;;         (message "need to set effort - minutes %S" minutes)
  ;; ;;         (org-entry-put (point)
  ;; ;;                        org-effort-property
  ;; ;;                        (apply #'format "%d:%02d" (cl-floor minutes 60)))))))
  ;; ;; (add-hook 'org-gcal-after-update-entry-functions #'my-org-gcal-set-effort)
  ;; ;; ;; see, https://github.com/kidd/org-gcal.el/pull/139
  ;; ;; ;; (defun hanshen/org-post-appt-to-gcal ()
  ;; ;; ;;   "Post capture hook for appointments that post event to Google Calendar"
  ;; ;; ;;   (if (equal (plist-get org-capture-plist :key) "a") ; Get buffer key & compare
  ;; ;; ;;       (progn (org-capture-goto-last-stored) (org-gcal-post-at-point))))
  ;; ;; ;; (add-hook 'org-capture-after-finalize-hook 'hanshen/org-post-appt-to-gcal)
  ;; https://old.reddit.com/r/emacs/comments/g8ecpj/advice_for_auclatex_what_keybinds_do_you_find/foo64ge/
  ;; What really increased my speed is having snippets (yasnippet) for
  ;; frequently used patterns, auto paired parentheses
  ;; (electric-pair-local-mode or smartparens), and cd-latex
  ;; (org-cdlatex-mode) which auto inserts brackets for
  ;; subscript/superscripts. There's still a lot more to be done for speed,
  ;; learning these packages and creating keybindings though. All in due
  ;; time! Turns on unicode characters for org-mode
  (setf org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil
        ;; Bigger latex fragment
        org-format-latex-options (plist-put org-format-latex-options :scale 3))
  ;; It generates a a png and overlays it onto the text as soon as your cursor
  ;; moves away from the math mode dollar signs. Automatically toggle org-mode
  ;; latex fragment previews as the cursor enters and exits them
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (find-file pdf-file)
        (message "No PDF found for %s" key))))
  
  (spacemacs/set-leader-keys "s SPC" 'helm-bibtex)
  
  (setf org-bibtex-file (concat +project-maria-dir+ "project-jerome.bib")
        reftex-default-bibliography (list (concat +project-maria-dir+ "project-jerome.bib"))
        helm-bibtex-full-frame nil
        bibtex-completion-bibliography (list (concat +project-maria-dir+ "project-jerome.bib"))
        ;; Clicking on a citation in an org file will draw up a list of actions
        ;; One of these is view notes. this is where the index of notes is stored
        ;; Alternatively, calling ", n" while in project-jerome.bib will call
        ;; org-ref-open-bibtex-notes, which will populate project-jerome-index.org
        ;; Tell org-ref to let helm-bibtex find notes for it
        org-ref-notes-function
        (lambda (thekey)
          (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
            (bibtex-completion-edit-notes
             (list (car (org-ref-get-bibtex-key-and-file thekey))))))
        ;; Taken from  https://github.com/jkitchin/org-ref/blob/master/org-ref.org#customizing-how-pdfs-are-opened
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
        bibtex-completion-bibliography (concat +project-maria-dir+ "project-jerome.bib")
        bibtex-completion-notes-path (concat +project-maria-dir+ "bibtex-notes")
        bibtex-completion-notes-template-multiple-files
        (concat
         "* ${title}\n"
         ":PROPERTIES:\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":YEAR: ${year}\n"
         ":END:\n\n")
        bibtex-completion-pdf-field "file"
        bibtex-completion-library-path
        '("~/project-jerome"
          "~/project-jerome/000-generalities-information-computers"
          "~/project-jerome/000-generalities-information-computers/000-computer-science"
          "~/project-jerome/000-generalities-information-computers/010-bibliography"
          "~/project-jerome/000-generalities-information-computers/020-library-information-sciences"
          "~/project-jerome/000-generalities-information-computers/030-general-encyclopedic-works"
          "~/project-jerome/000-generalities-information-computers/040-special-topics"
          "~/project-jerome/000-generalities-information-computers/050-general-serials-indexes"
          "~/project-jerome/000-generalities-information-computers/060-general-organizations-museums"
          "~/project-jerome/000-generalities-information-computers/070-news-media-jounalism-publishing"
          "~/project-jerome/000-generalities-information-computers/080-general-collections"
          "~/project-jerome/000-generalities-information-computers/090-manuscripts-rare-books"
          "~/project-jerome/100-philosphy-psychology"
          "~/project-jerome/100-philosphy-psychology/110-metaphysics"
          "~/project-jerome/100-philosphy-psychology/120-epistemology-causation-humankind"
          "~/project-jerome/100-philosphy-psychology/130-paranormal-phenomena"
          "~/project-jerome/100-philosphy-psychology/140-specific-philosophical-schools"
          "~/project-jerome/100-philosphy-psychology/150-psychology"
          "~/project-jerome/100-philosphy-psychology/160-logic"
          "~/project-jerome/100-philosphy-psychology/170-ethics-moral-philosophy"
          "~/project-jerome/100-philosphy-psychology/180-ancient-medieval-oriental-philosophy"
          "~/project-jerome/100-philosphy-psychology/190-modern-western-philosophy"
          "~/project-jerome/200-religion"
          "~/project-jerome/200-religion/210-natural-theology"
          "~/project-jerome/200-religion/220-bible"
          "~/project-jerome/200-religion/230-christian-theology"
          "~/project-jerome/200-religion/230-christian-theology/239-apologetics"
          "~/project-jerome/200-religion/240-christian-moral-devotional-theology"
          "~/project-jerome/200-religion/240-christian-moral-devotional-theology/242-devotional-literature"
          "~/project-jerome/200-religion/250-christian-orders-local-churches"
          "~/project-jerome/200-religion/250-christian-orders-local-churches/252-texts-of-sermons"
          "~/project-jerome/200-religion/250-christian-orders-local-churches/253-pastoral-office-and-work"
          "~/project-jerome/200-religion/260-christian-social-theology"
          "~/project-jerome/200-religion/270-christian-church-history"
          "~/project-jerome/200-religion/280-christian-denominations-sects"
          "~/project-jerome/200-religion/290-other-comparative-religions"
          "~/project-jerome/300-social-sciences"
          "~/project-jerome/300-social-sciences/310-general-statisticnbox.cs"
          "~/project-jerome/300-social-sciences/320-political-science"
          "~/project-jerome/300-social-sciences/330-economics"
          "~/project-jerome/300-social-sciences/340-law"
          "~/project-jerome/300-social-sciences/350-public-administration"
          "~/project-jerome/300-social-sciences/360-social-problems-services"
          "~/project-jerome/300-social-sciences/370-education"
          "~/project-jerome/300-social-sciences/380-commerce-communications-transport"
          "~/project-jerome/300-social-sciences/390-customs-etiquette-folklore"
          "~/project-jerome/400-philology"
          "~/project-jerome/400-philology/410-linguistics"
          "~/project-jerome/400-philology/420-english-anglosaxon-languages"
          "~/project-jerome/400-philology/430-germanic-languages"
          "~/project-jerome/400-philology/440-romance-languages"
          "~/project-jerome/400-philology/450-italian-romanian-rhaeto-romanic"
          "~/project-jerome/400-philology/460-spanish-portuguese-languages"
          "~/project-jerome/400-philology/470-italic-languages-latin"
          "~/project-jerome/400-philology/480-hellenic-languages-classical-greek"
          "~/project-jerome/400-philology/490-other-languages"
          "~/project-jerome/500-natural-science"
          "~/project-jerome/500-natural-science/510-mathematics"
          "~/project-jerome/500-natural-science/520-astronomy-allied-sciences"
          "~/project-jerome/500-natural-science/530-physics"
          "~/project-jerome/500-natural-science/540-chemistry-allied-sciences"
          "~/project-jerome/500-natural-science/550-earth-sciences"
          "~/project-jerome/500-natural-science/560-paleontology-paleozoology"
          "~/project-jerome/500-natural-science/570-life-sciences"
          "~/project-jerome/500-natural-science/580-botanical-sciences"
          "~/project-jerome/500-natural-science/590-zoological-sciences"
          "~/project-jerome/600-applied-science"
          "~/project-jerome/600-applied-science/610-medical-sciences-psychiatry"
          "~/project-jerome/600-applied-science/620-engineering"
          "~/project-jerome/600-applied-science/630-agriculture"
          "~/project-jerome/600-applied-science/640-home-economics-family-living"
          "~/project-jerome/600-applied-science/650-management"
          "~/project-jerome/600-applied-science/660-chemical-engineering"
          "~/project-jerome/600-applied-science/670-manufacturing"
          "~/project-jerome/600-applied-science/680-manufacture-for-specific-use"
          "~/project-jerome/600-applied-science/690-buildings"
          "~/project-jerome/700-arts-recreation"
          "~/project-jerome/700-arts-recreation/710-civic-landscape-art"
          "~/project-jerome/700-arts-recreation/720-architecture"
          "~/project-jerome/700-arts-recreation/730-sculpture"
          "~/project-jerome/700-arts-recreation/740-drawings-decorative-arts"
          "~/project-jerome/700-arts-recreation/750-paintings-painters"
          "~/project-jerome/700-arts-recreation/760-graphics-arts-printmaking"
          "~/project-jerome/700-arts-recreation/770-photography"
          "~/project-jerome/700-arts-recreation/780-music"
          "~/project-jerome/700-arts-recreation/790-recreational-performing-arts"
          "~/project-jerome/800-literature"
          "~/project-jerome/800-literature/810-american-literature-in-english"
          "~/project-jerome/800-literature/820-english-literature"
          "~/project-jerome/800-literature/830-literature-of-germanic-language"
          "~/project-jerome/800-literature/840-literatures-of-romance-language"
          "~/project-jerome/800-literature/850-italian-romanian-rhaeto-romaic-literatures"
          "~/project-jerome/800-literature/860-spanish-portuguese-literatures"
          "~/project-jerome/800-literature/870-italic-literatures-latin"
          "~/project-jerome/800-literature/880-hellenic-literatures-classical-greek"
          "~/project-jerome/800-literature/890-literatures-of-other-languages"
          "~/project-jerome/900-history-geography-biography"
          "~/project-jerome/900-history-geography-biography/910-geography-travel"
          "~/project-jerome/900-history-geography-biography/920-biography-genealogy-insignia"
          "~/project-jerome/900-history-geography-biography/930-history-of-the-ancient-world"
          "~/project-jerome/900-history-geography-biography/940-general-history-of-europe"
          "~/project-jerome/900-history-geography-biography/950-general-history-of-asia"
          "~/project-jerome/900-history-geography-biography/960-general-history-of-africa"
          "~/project-jerome/900-history-geography-biography/970-general-history-of-north-america"
          "~/project-jerome/900-history-geography-biography/980-general-history-of-south-america"
          "~/project-jerome/900-history-geography-biography/990-general-history-of-other-areas"))
  (with-system-name "BHW-THINKPAD"
    (defun mu4e-headers-mark-all-unread-read ()
      "Put a ! \(read) mark on all visible unread messages."
      (interactive)
      (mu4e-headers-mark-for-each-if
       (cons 'read nil)
       (lambda (msg _param)
         (memq 'unread (mu4e-msg-field msg :flags))))
      (mu4e-mark-execute-all t))
  
    (defun mu4e-headers-refile-all ()
      "Refile all messages in buffer."
      (interactive)
      (mu4e-headers-mark-for-each-if
       (cons 'refile nil)
       (lambda (_msg _param) t))
      (mu4e-mark-execute-all t)
      (mu4e-search-prev))
  
    (setf mu4e-change-filenames-when-moving t  ; mbsync specific.
          ;; see an ASCII table for the character decimal codes
          mu4e-bookmarks '(("maildir:/INBOX" "Inbox" 105 )
                           ("\"maildir:/[Gmail]/All Mail\" and flag:unread" "Unread" 85))
          user-mail-address "benedicthanshenwang@gmail.com"
          user-full-name "Benedict Hanshen Wang"
          ;; mu4e-compose-signature
          mail-user-agent 'mu4e-user-agent
          mu4e-attachment-dir "/mnt/c/Users/Ben/Downloads/"
          mu4e-drafts-folder "/[Gmail]/Drafts"
          mu4e-sent-folder "/[Gmail]/Sent Mail"
          mu4e-trash-folder "/[Gmail]/Trash"
          mu4e-refile-folder "/[Gmail]/All Mail"
          send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587
          message-sendmail-f-is-evil t
          mu4e-index-update-in-background t
          mu4e-update-interval 300
          mu4e-autorun-background-at-startup t
          mu4e-get-mail-command "mbsync -a"
          mu4e-hide-index-messages t
          mu4e-enable-mode-line t
         ;; If this is enabled, prompts for new gpg fingerprints will not show up.
         ;; Instead emails will silently fail to send.
          mu4e-enable-async-operations nil
          mu4e-headers-show-threads t
          mu4e-headers-skip-duplicates t
          ;; Multipart html/plaintext email default, if the html portion is larger
          ;; by a factor of 5, it is assumed the user wants to view html. This
          ;; sets the factor to the largest possible fixnum, for we prefer the
          ;; plaintext version.
          mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
          gnus-blocked-images "."
          mu4e-org-link-query-in-headers-mode nil
          mu4e-view-show-images t
          mu4e-view-show-addresses t
          mu4e-org-contacts-file (concat +project-maria-dir+ "task-management/0contacts.org")
          message-kill-buffer-on-exit t
          mu4e-confirm-quit nil
          mu4e-headers-fields
          '((:human-date . 5)
            (:from-or-to . 20)
            (:subject))
          mml-secure-openpgp-sign-with-sender t
          mml-secure-openpgp-signers '("06DDA93690F775E3715B628CCA949A6D46BC2BBE")
          mu4e-compose-complete-only-after nil
          browse-url-filename-alist
          '(("^/\\(ftp@\\|anonymous@\\)?\\([^:/]+\\):/*" . "ftp://\\2/")
            ("^/\\([^:@/]+@\\)?\\([^:/]+\\):/*" . "ftp://\\1\\2/")
            ;; For gnus-article-browse-html-article on Windows Subsystem for Linux.
            ("^/+" . "file://///wsl$/Debian/"))
          )
    (add-to-list 'mu4e-headers-actions
                 '("org-contact-add" . mu4e-action-add-org-contact) t)
    (with-eval-after-load "recentf"
      (progn
        (add-to-list 'recentf-exclude "~/project-jerome/email-archive/")
        (add-to-list 'recentf-exclude (concat +project-jerome-dir+ "email-archive/"))
        (add-to-list 'recentf-exclude "/tmp/")))
    ;; Unbind s, originally bound to mu4e-headers-search.
    ;; Unset =mu4e-headers=search= from both =mu4e-headers-mode-map= and
    ;; =mu4e-view-mode-map=. Retain =s= for search in =mu4e-main-mode-map=.
    (define-key mu4e-headers-mode-map (kbd "s") #'avy-goto-word-or-subword-1)
    (define-key mu4e-headers-mode-map (kbd "K") #'mu4e-view-save-url)
    (define-key mu4e-headers-mode-map "S" 'helm-mu)
    (define-key mu4e-view-mode-map (kbd "s") #'avy-goto-word-or-subword-1)
    (define-key mu4e-view-mode-map (kbd "K") #'mu4e-view-save-url)
    (define-key mu4e-view-mode-map "S" 'helm-mu)
    ;; (define-key mu4e-main-mode-map (kbd "s") #'avy-goto-word-or-subword-1)
    ;; (define-key mu4e-main-mode-map "S" 'helm-mu)
    (add-hook 'mu4e-view-mode-hook (lambda () (evil-evilified-state)))
    (define-key mu4e-headers-mode-map (kbd "c") #'mu4e-headers-mark-all-unread-read)
    (define-key mu4e-headers-mode-map (kbd "C") #'mu4e-headers-refile-all)
    ;; Functions ran on every message sent
    (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode "o" 'org-mime-edit-mail-in-org-mode)
    (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode "p" 'mml-secure-message-sign-pgpmime)
    (spacemacs/set-leader-keys
      (kbd "se") #'helm-mu
      (kbd "je") #'mu4e-headers-search-bookmark)
    )
  (require 'elfeed)
  (require 'hydra)
  (require 'elfeed-tube)
  (setf elfeed-db-directory "~/.emacs.d/.cache/elfeed"
        elfeed-use-curl t)
  ;; Remove default binding so evilify mapping takes over.
  ;; Set shortcut for org-web-tools to fetch full article
  (with-eval-after-load "elfeed"
    (progn
      (define-key elfeed-search-mode-map (kbd "RET") nil)
      (define-key elfeed-search-mode-map (kbd "RET") #'ap/elfeed-search-browse-org)))
  (defhydra hanshen/hydra-elfeed (:exit t)
    ("g" (elfeed-search-set-filter "@6-months-ago +unread +gbl") "Global News")
    ("l" (elfeed-search-set-filter "@6-months-ago +unread +lcl") "Local News")
    ("s" (elfeed-search-set-filter "@6-months-ago +unread +sci") "Science & Tech")
    ("b" (elfeed-search-set-filter "@6-months-ago +unread +blog") "Misc. Blogs")
    ("c" (elfeed-search-set-filter "@6-months-ago +unread +rel") "Catholic")
    ("f" (elfeed-search-set-filter "@6-months-ago +unread +frm") "Forums")
    ("o" (elfeed-search-set-filter "@6-months-ago +unread +pod") "Podcasts")
    ("y" (elfeed-search-set-filter "@6-months-ago +unread +vid") "Youtube")
    ("a" (elfeed-search-set-filter "@6-months-ago +unread") "All")
    ("q" nil "quit" :color blue))
  (evilified-state-evilify-map elfeed-search-mode-map
    :mode elfeed-search-mode
    :eval-after-load elfeed-search
    :bindings
    "s"  #'avy-goto-word-or-subword-1
    "S"  #'elfeed-search-live-filter
    "f"  #'hanshen/hydra-elfeed/body
    "r"  #'elfeed-mark-all-as-read
    "RET"  #'ap/elfeed-search-browse-org
    "t"   #'elfeed-search-show-entry
    ";" #'helm-occur
    "b" #'ben/elfeed-search-browse-url
    "C-u b" #'elfeed-search-browse-url
    "o" #'elfeed-tube-fetch
    "gr" #'elfeed-update)
  (evilified-state-evilify-map elfeed-show-mode-map
    :bindings
    "s"  #'avy-goto-word-or-subword-1
    ";" #'helm-occur)
  (defun elfeed-mark-all-as-read ()
    "Marks entire buffer before tagging marked region as read"
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))
  ;; Source https://www.reddit.com/r/emacs/comments/g6oowz/elfeed_rules/fodeb8x/
  (defun ap/elfeed-search-browse-org ()
    "Open selected items as Org."
    (interactive)
    (let ((browse-url-browser-function (lambda (url _)
                                         (org-web-tools-read-url-as-org url))))
      (ap/elfeed-search-selected-map #'ap/elfeed-search-browse-entry)))
  
  (defun ap/elfeed-search-browse-entry (entry)
    "Browse ENTRY with `browse-url' and mark as read.
      If ENTRY is unread, it will also be unstarred.  To override the
      browser function, bind `browse-url-browser-function' around the
      call to this."
    (let ((url (elfeed-entry-link entry))
          (tags (elfeed-entry-tags entry)))
      ;; Mark as read first, because apparently the elfeed functions don't work after `browse-url'
      ;; potentially changes the buffer.
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (browse-url url)))
  
  (cl-defun ap/elfeed-search-selected-map (fn)
    "Map FN across selected entries in elfeed-search buffer using `mapcar'."
    (mapcar fn (elfeed-search-selected)))
  
  (defun ben/elfeed-search-browse-url (&optional use-generic-p)
    "Visit the current entry in your browser using `browse-url'.
  If there is a prefix argument, visit the current entry in the
  browser defined by `browse-url-generic-program'."
    (interactive "P")
    (let ((buffer (current-buffer))
          (entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (if use-generic-p
                      (browse-url-generic it)
                    (eww it)))
      ;; `browse-url' could have switched to another buffer if eww or another
      ;; internal browser is used, but the remainder of the functions needs to
      ;; run in the elfeed buffer.
      (with-current-buffer buffer
        (mapc #'elfeed-search-update-entry entries)
        (unless (or elfeed-search-remain-on-entry (use-region-p))
          (forward-line)))))
  (add-hook 'elfeed-new-entry-hook #'elfeed-tube--auto-fetch)
  (advice-add 'elfeed-show-entry
              :after #'elfeed-tube--auto-fetch)
  (advice-add elfeed-show-refresh-function
              :after #'elfeed-tube-show)
  ;;-------------------------------------------------------------------------
  ;; ***  Shell Config
  ;;-------------------------------------------------------------------------
  ;; Forces bash shell into interactive mode, leadings to sourcing of
  ;; ~/.bashrc and interactive aliases and functions
  ;; (setq shell-command-switch "-ic")
  ;; Default value was "-c"
  (setq shell-command-switch "-c")
  ;; Try to find and set a font if one exists. Emacsclient -c being unable to find fonts is behaviour only seen with emacs-gtk29.0 and not emacs-pgtk29.0
  (defun my/set-font ()
    (cond
     ((find-font (font-spec :family "Iosevka Term Slab"))
      (set-face-attribute 'default nil
                          :family "Iosevka Term Slab"
                          :weight 'normal))))
  (add-hook 'after-make-frame-functions
            (defun my/set-new-frame-font (frame)
              (with-selected-frame frame
                (my/set-font))))
  (my/set-font)
  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
  ;; (set-selection-coding-system 'utf-16-le)
  ;; Based on https://stackoverflow.com/questions/12102554/emacs-skip-whitespace-kills
  (define-advice kill-new (:around (orig-fn string &optional rest) ignore-whitespaces)
    "Don't put whitespaces into kill ring."
    (let* ((string-raw (substring-no-properties string))
           (space-p (not (string-match-p "[^ \t\n\r]" string-raw))))
      (if (not space-p)
          (apply orig-fn string rest)
        (message "skipped whitespace kill")
        nil)))
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\[ ?\()
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\] ?\))
  ;; Should double buffering cause lag spikes on 3840 x 2160 displays
  ;; we can disable it via...
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  ;; Sets default browser
  (setf browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args     '("/c" "start")
        browse-url-browser-function #'browse-url-generic
        package-install-upgrade-built-in t)
  ;; Useful trick to share snippets between modes. Whenever a major mode
  ;; is loaded, fundamental-mode is also loaded
  (add-hook 'yas-minor-mode-hook (lambda ()
                                   (yas-activate-extra-mode 'fundamental-mode)))
  ;; User Defined Toggles -> See SPC t hydra menu
  ;; VISUALLY wraps words that go past screen length
  ;; setq-default command means the command is run in every major mode buffer
  (setq-default truncate-lines nil)
  ;; Enables line by line navigation. Lines are not line broken > use RET for that
  (spacemacs/toggle-visual-line-navigation-globally-on)
  ;; Determines the length of time between the end of typing for SPC j j (avy-timer)
  ;; and the appearance of green prompt letters
  (setq avy-timeout-seconds 0.50)
  ;; Change the scale of the powerline
  ;; Commented out as I'm currently using default Emacs modeline.
  ;; (setq powerline-scale 0.50)
  ;; Self explanatory (menu-bar-mode 1) (tool-bar-mode 1)
  )
