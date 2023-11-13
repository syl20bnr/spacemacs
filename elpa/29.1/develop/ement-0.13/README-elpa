                               ━━━━━━━━━━
                                EMENT.EL
                               ━━━━━━━━━━


Table of Contents
─────────────────

1. Installation
2. Usage
3. Changelog
4. Development
5. License


[https://elpa.gnu.org/packages/ement.svg]

Ement.el is a Matrix client for Emacs.  It aims to be simple, fast,
featureful, and reliable.

Feel free to join us in the chat room:
[https://img.shields.io/matrix/ement.el:matrix.org.svg?label=%23ement.el:matrix.org]


[https://elpa.gnu.org/packages/ement.svg]
<https://elpa.gnu.org/packages/ement.html>

[https://img.shields.io/matrix/ement.el:matrix.org.svg?label=%23ement.el:matrix.org]
<https://matrix.to/#/#ement.el:matrix.org>


1 Installation
══════════════

GNU ELPA
────────

  Ement.el is published in [GNU ELPA] as [ement], so it may be installed
  in Emacs with the command `M-x package-install RET ement RET'.  This
  is the recommended way to install Ement.el, as it will install the
  current stable release.

  The latest development build may be installed from [ELPA-devel] or
  from Git (see below).


[GNU ELPA] <http://elpa.gnu.org/>

[ement] <https://elpa.gnu.org/packages/ement.html>

[ELPA-devel] <https://elpa.gnu.org/devel/ement.html>


GNU Guix
────────

  Ement.el is available in [GNU Guix] as [emacs-ement].


[GNU Guix] <https://guix.gnu.org/>

[emacs-ement] <https://packages.guix.gnu.org/packages/emacs-ement/>


Debian, Ubuntu
──────────────

  Ement.el is available in [Debian as elpa-ement] and in [Ubuntu as
  elpa-ement].


[Debian as elpa-ement] <https://packages.debian.org/elpa-ement>

[Ubuntu as elpa-ement]
<https://packages.ubuntu.com/search?suite=default&section=all&arch=any&keywords=elpa-ement&searchon=names>


Nix
───

  Ement.el is available in [NixOS] as [emacsPackages.ement].


[NixOS] <https://nixos.org/>

[emacsPackages.ement]
<https://search.nixos.org/packages?channel=23.05&show=emacsPackages.ement&from=0&size=50&sort=relevance&type=packages&query=ement>


Other distributions
───────────────────

  Ement.el is also available in some other distributions.  See
  [Repology] for details.


[Repology] <https://repology.org/project/emacs:ement/related>


Git master
──────────

  The `master' branch of the Git repository is intended to be usable at
  all times; only minor bugs are expected to be found in it before a new
  stable release is made.

  To install, it is recommended to use [quelpa-use-package], like this
  (using [this helpful command] for upgrading versions):

  ┌────
  │ ;; Install and load `quelpa-use-package'.
  │ (package-install 'quelpa-use-package)
  │ (require 'quelpa-use-package)
  │ 
  │ ;; Install Ement.
  │ (use-package ement
  │   :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))
  └────

  One might also use systems like [Elpaca] or [Straight] (which is also
  used by [DOOM]), but the author cannot offer support for them.


[quelpa-use-package] <https://github.com/quelpa/quelpa-use-package>

[this helpful command]
<https://github.com/alphapapa/unpackaged.el#upgrade-a-quelpa-use-package-forms-package>

[Elpaca] <https://github.com/progfolio/elpaca>

[Straight] <https://github.com/radian-software/straight.el>

[DOOM] <https://github.com/doomemacs/doomemacs>


Manual
──────

  Ement.el is intended to be installed with Emacs's package system,
  which will ensure that the required autoloads are generated, etc.  If
  you choose to install it manually, you're on your own.


2 Usage
═══════

  • 
  • 
  • 

  1. Call command `ement-connect' to connect.  Multiple sessions are
     supported, so you may call the command again to connect to another
     account.
  2. Wait for initial sync to complete (which can take a few
     moments–initial sync JSON requests can be large).
  3. Use these commands (room-related commands may be called with
     universal prefix to prompt for the room):
     • `ement-list-rooms' to view the list of joined rooms.
     • `ement-view-room' to view a room's buffer, selected with
       completion.
     • `ement-create-room' to create a new room.
     • `ement-create-space' to create a space.
     • `ement-invite-user' to invite a user to a room.
     • `ement-join-room' to join a room.
     • `ement-leave-room' to leave a room.
     • `ement-forget-room' to forget a room.
     • `ement-tag-room' to toggle a tag on a room (including
       favorite/low-priority status).
     • `ement-list-members' to list members in a room.
     • `ement-send-direct-message' to send a direct message to a user
       (in an existing direct room, or creating a new one
       automatically).
     • `ement-room-edit-message' to edit a message at point.
     • `ement-room-send-file' to send a file.
     • `ement-room-send-image' to send an image.
     • `ement-room-set-topic' to set a room's topic.
     • `ement-room-occur' to search in a room's known events.
     • `ement-room-override-name' to override a room's display name.
     • `ement-ignore-user' to ignore a user (or with interactive prefix,
       un-ignore).
     • `ement-room-set-message-format' to set a room's message format
       buffer-locally.
     • `ement-room-toggle-space' to toggle a room's membership in a
       space (a way to group rooms in Matrix).
     • `ement-directory' to view a room directory.
     • `ement-directory-search' to search a room directory.
  4. Use these special buffers to see events from multiple rooms (you
     can also reply to messages from these buffers!):
     • See all new events that mention you in the `*Ement Mentions*'
       buffer.
     • See all new events in rooms that have open buffers in the `*Ement
       Notifications*' buffer.


Bindings
────────

  These bindings are common to all of the following buffer types:

  ⁃ Switch to a room buffer: `M-g M-r'
  ⁃ Switch to the room list buffer: `M-g M-l'
  ⁃ Switch to the mentions buffer: `M-g M-m'
  ⁃ Switch to the notifications buffer: `M-g M-n'


Room buffers
╌╌╌╌╌╌╌╌╌╌╌╌

  ⁃ Show command menu: `?'



  *Movement*

  ⁃ Next event: `n'
  ⁃ Previous event: `p'
  ⁃ End of buffer: `N'
  ⁃ Scroll up and mark read: `SPC'
  ⁃ Scroll down: `S-SPC'
  ⁃ Jump to fully-read marker: `M-g M-p'
  ⁃ Move read markers to point: `m'
  ⁃ Load older messages: at top of buffer, scroll contents up
    (i.e. `S-SPC', `M-v' or `mwheel-scroll')

  *Switching*

  ⁃ List rooms: `M-g M-l'
  ⁃ Switch to other room: `M-g M-r'
  ⁃ Switch to mentions buffer: `M-g M-m'
  ⁃ Switch to notifications buffer: `M-g M-n'
  ⁃ Quit window: `q'

  *Messages*

  ⁃ Write message: `RET'
  ⁃ Write reply to event at point (when region is active, only quote
    marked text) : `S-RET'
  ⁃ Compose message in buffer: `M-RET' (while writing in minibuffer:
    `C-c ')' (Use command `ement-room-compose-org' to activate Org mode
    in the compose buffer.)
  ⁃ Edit message: `<insert>'
  ⁃ Delete message: `C-k'
  ⁃ Send reaction to event at point, or send same reaction at point: `s
    r'
  ⁃ Send emote: `s e'
  ⁃ Send file: `s f'
  ⁃ Send image: `s i'
  ⁃ View event source: `v'
  ⁃ Complete members and rooms at point: `C-M-i' (standard
    `completion-at-point' command).  (Type an `@' prefix for a member
    mention, a `#' prefix for a room alias, or a `!' prefix for a room
    ID.)

  *Images*

  ⁃ Toggle scale of image (between fit-to-window and thumbnail):
    `mouse-1'
  ⁃ Show image in new buffer at full size: `double-mouse-1'

  *Users*

  ⁃ Send direct message: `u RET'
  ⁃ Invite user: `u i'
  ⁃ Ignore user: `u I'

  *Room*

  ⁃ Occur search in room: `M-s o'
  ⁃ List members: `r m'
  ⁃ Set topic: `r t'
  ⁃ Set message format: `r f'
  ⁃ Set notification rules: `r n'
  ⁃ Override display name: `r N'
  ⁃ Tag/untag room: `r T'

  *Room membership*

  ⁃ Create room: `R c'
  ⁃ Join room: `R j'
  ⁃ Leave room: `R l'
  ⁃ Forget room: `R F'
  ⁃ Toggle room's spaces: `R s'

  *Other*

  ⁃ Sync new messages (not necessary if auto sync is enabled; with
    prefix to force new sync): `g'


Room list buffer
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  ⁃ Show buffer of room at point: `RET'
  ⁃ Show buffer of next unread room: `SPC'
  ⁃ Move between room names: `TAB' / `<backtab>'

  ⁃ Kill room's buffer: `k'
  ⁃ Toggle room's membership in a space: `s'


Directory buffers
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  ⁃ View/join a room: `RET' / `mouse-1'
  ⁃ Load next batch of rooms: `+'


Mentions/notifications buffers
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  ⁃ Move between events: `TAB' / `<backtab>'
  ⁃ Go to event at point in its room buffer: `RET'
  ⁃ Write reply to event at point (shows the event in its room while
    writing) : `S-RET'


Tips
────

  ⁃ Desktop notifications are enabled by default for events that mention
    the local user.  They can also be shown for all events in rooms with
    open buffers.
  ⁃ Send messages in Org mode format by customizing the option
    `ement-room-send-message-filter' (which enables Org format by
    default), or by calling `ement-room-compose-org' in a compose buffer
    (which enables it for a single message).  Then Org-formatted
    messages are automatically converted and sent as HTML-formatted
    messages (with the Org syntax as the plain-text fallback).  You can
    send syntax such as:
    • Bold, italic, underline, strikethrough
    • Links
    • Tables
    • Source blocks (including results with `:exports both')
    • Footnotes (okay, that might be pushing it, but you can!)
    • And, generally, anything that Org can export to HTML
  ⁃ Starting in the room list buffer, by pressing `SPC' repeatedly, you
    can cycle through and read all rooms with unread buffers.  (If a
    room doesn't have a buffer, it will not be included.)
  ⁃ Room buffers and the room-list buffer can be bookmarked in Emacs,
    i.e. using `C-x r m'.  This is especially useful with [Burly]: you
    can arrange an Emacs frame with several room buffers displayed at
    once, use `burly-bookmark-windows' to bookmark the layout, and then
    you can restore that layout and all of the room buffers by opening
    the bookmark, rather than having to manually arrange them every time
    you start Emacs or change the window configuration.
  ⁃ Images and other files can be uploaded to rooms using drag-and-drop.
  ⁃ Mention members by typing a `@' followed by their displayname or
    Matrix ID.  (Members' names and rooms' aliases/IDs may be completed
    with `completion-at-point' commands.)
  ⁃ You can customize settings in the `ement' group.
    • *Note:* `setq' should not be used for certain options, because it
       will not call the associated setter function.  Users who have an
       aversion to the customization system may experience problems.


[Burly] <https://github.com/alphapapa/burly.el>

Displaying symbols and emojis
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  Emacs may not display certain symbols and emojis well by default.
  Based on [this question and answer], you may find that the simplest
  way to fix this is to install an appropriate font, like [Noto Emoji],
  and then use this Elisp code:

  ┌────
  │ (setf use-default-font-for-symbols nil)
  │ (set-fontset-font t 'unicode "Noto Emoji" nil 'append)
  └────


[this question and answer]
<https://emacs.stackexchange.com/questions/62049/override-the-default-font-for-emoji-characters>

[Noto Emoji] <https://www.google.com/get/noto/#emoji-zsye>


Encrypted room support through Pantalaimon
──────────────────────────────────────────

  Ement.el doesn't support encrypted rooms natively, but it can be used
  transparently with the E2EE-aware reverse proxy daemon [Pantalaimon].
  After configuring it according to its documentation, call
  `ement-connect' with the appropriate hostname and port, like:

  ┌────
  │ (ement-connect :uri-prefix "http://localhost:8009")
  └────


[Pantalaimon] <https://github.com/matrix-org/pantalaimon/>


3 Changelog
═══════════

0.13
────

  *Additions*

  ⁃ Group joined direct rooms in directory buffers.
  ⁃ Command `end-of-buffer' is bound to `N' in room buffers.

  *Changes*

  ⁃ Command `ement-room-image-show' use frame parameters to maximize the
    frame, making it easier for users to override.  ([#223].  Thanks to
    [Nicholas Vollmer].)

  *Fixes*

  ⁃ Name for direct rooms in directory buffers.
  ⁃ Editing a message from the compose buffer would be sent as a reply
    to the edited message.  (Fixes [#189].  Thanks to [Phil Sainty] for
    reporting.)
  ⁃ Editing an already-edited message.  ([#226].  Thanks to [Phil
    Sainty] for reporting.)
  ⁃ Replying to an already-edited message.  ([#227].  Thanks to [Phil
    Sainty] for reporting.)
  ⁃ Rendering redactions of edited messages.  ([#228].  Thanks to [Phil
    Sainty] for reporting.)
  ⁃ Redacting an edited message.  ([#228].  Thanks to [Phil Sainty] for
    reporting.)
  ⁃ Command `ement-room-flush-colors' maintains point position.


[#223] <https://github.com/alphapapa/ement.el/issues/223>

[Nicholas Vollmer] <https://github.com/progfolio>

[#189] <https://github.com/alphapapa/ement.el/issues/189>

[Phil Sainty] <https://github.com/phil-s>

[#226] <https://github.com/alphapapa/ement.el/issues/226>

[#227] <https://github.com/alphapapa/ement.el/issues/227>

[#228] <https://github.com/alphapapa/ement.el/issues/228>


0.12
────

  *Additions*

  ⁃ Command `ement-notifications' shows recent notifications, similar to
    the pane in the Element client.  (This new command fetches recent
    notifications from the server and allows scrolling up to retrieve
    older ones.  Newly received notifications, as configured in the
    `ement-notify' options, are displayed in the same buffer.  This
    functionality will be consolidated in the future.)
  ⁃ Face `ement-room-quote', applied to quoted parts of replies.

  *Changes*
  ⁃ Commands `ement-room-goto-next' and `ement-room-goto-prev' work more
    usefully at the end of a room buffer.  (Now pressing `n' on the last
    event moves point to the end of the buffer so it will scroll
    automatically for new messages, and then pressing `p' skips over any
    read marker to the last event.)
  ⁃ Room buffer bindings:
    ⁃ `ement-room-goto-next' and `ement-room-goto-prev' are bound to `n'
      and `p', respectively.
    ⁃ `ement-room-goto-fully-read-marker' is bound to `M-g M-p' (the
      mnemonic being "go to previously read").
  ⁃ The quoted part of a reply now omits the face applied to the rest of
    the message, helping to distinguish them.
  ⁃ Commands that read a string from the minibuffer in `ement-room'
    buffers and `ement-connect' user ID prompts use separate history
    list variables.
  ⁃ Use Emacs's Jansson-based JSON-parsing functions when available.
    (This results in a 3-5x speed improvement for parsing JSON
    responses, which can be significant for large initial sync
    responses.  Thanks to [Ryan Rix] for discovering this!)

  *Fixes*

  ⁃ File event formatter assumed that file size metadata would be
    present (a malformed, e.g. spam, event might not have it).
  ⁃ Send correct file size when sending files/images.
  ⁃ Underscores are no longer interpreted as denoting subscripts when
    sending messages in Org format.  (Thanks to [Phil Sainty].)
  ⁃ Add workaround for `savehist-mode''s serializing of the
    `command-history' variable's arguments.  (For `ement-' commands,
    that may include large data structures, like `ement-session'
    structs, which should never be serialized or reused, and
    `savehist''s doing so could cause noticeable delays for users who
    enabled it).  (See [#216].  Thanks to [Phil Sainty] and other users
    who helped to discover this problem.)


[Ryan Rix] <https://github.com/rrix/>

[Phil Sainty] <https://github.com/phil-s>

[#216] <https://github.com/alphapapa/ement.el/issues/216>


0.11
────

  *Additions*
  ⁃ Commands `ement-room-image-show' and `ement-room-image-scale' (bound
    to `RET' and `M-RET' when point is at an image) view and scale
    images.  (Thanks to [Steven Allen] for these and other image-related
    improvements.)
  ⁃ Command `ement-room-image-show-mouse' is used to show an image with
    the mouse.

  *Changes*
  ⁃ Enable `image-mode' when showing images in a new buffer.  (Thanks to
    [Steven Allen].)
  ⁃ Command `ement-room-image-show' is not used for mouse events.
  ⁃ Show useful message in SSO login page.

  *Fixes*
  ⁃ Allow editing of already-edited events.
  ⁃ Push rules' actions may be listed in any order.  (Fixes
    compatibility with [v1.7 of the spec].  Thanks to [Steven Allen].)
  ⁃ Call external browser for SSO login page.  (JavaScript is usually
    required, which EWW doesn't support, and loading the page twice
    seems to change state on the server that causes the SSO login to
    fail, so it's best to load the page in the external browser
    directly).
  ⁃ Clean up SSO server process after two minutes in case SSO login
    fails.
  ⁃ Don't stop syncing if an error is signaled while sending a
    notification.
  ⁃ Command `ement-room-list-next-unread' could enter an infinite loop.
    (Thanks to [Visuwesh] and `@mrtnmrtn:matrix.org'.)
  ⁃ Events in notifications buffer could appear out-of-order.  ([#191].
    Thanks to [Phil Sainty].)

  *Internal*
  ⁃ The `ement-read-receipt-idle-timer' could be duplicated when using
    multiple sessions.  ([#196].  Thanks to [Phil Sainty].)


[Steven Allen] <https://github.com/Stebalien>

[v1.7 of the spec]
<https://spec.matrix.org/v1.7/client-server-api/#actions>

[Visuwesh] <https://github.com/vizs>

[#191] <https://github.com/alphapapa/ement.el/issues/191>

[Phil Sainty] <https://github.com/phil-s>

[#196] <https://github.com/alphapapa/ement.el/issues/196>


0.10
────

  *Security Fixes*
  ⁃ When uploading a GPG-encrypted file (i.e. one whose filename ends in
    `.gpg'), if the recipient's private key or the symmetric encryption
    key were cached by Emacs (or a configured agent, like `gpg-agent'),
    Emacs would automatically decrypt the file while reading its
    contents and then upload the decrypted contents.  (This happened
    because the function `insert-file-contents' was used, which does
    many things automatically, some of which are not even mentioned in
    its docstring; refer to its entry in the Elisp Info manual for
    details.  The fix is to use `insert-file-contents-literally'
    instead.)  Thanks to `@welkinsl:matrix.org' for reporting.

  *Additions*
  ⁃ Support for Single Sign-On (SSO) authentication.  ([#24].  Thanks to
    [Jeffrey Stoffers] for development, and to [Phil Sainty], [Jakub
    Kadlčík], and [Juanjo Presa] for testing.)
  ⁃ Bind `m' in room buffers to `ement-room-mark-read' (which moves read
    markers to point).

  *Changes*

  ⁃ Activating a space in the room list uses `ement-view-space' (which
    shows a directory of rooms in the space) instead of
    `ement-view-room' (which shows events in the space, which is
    generally not useful).
  ⁃ Command `ement-view-room', when used for a space, shows a footer
    explaining that the buffer is showing a space rather than a normal
    room, with a button to call `ement-view-space' for it (which lists
    rooms in the space).
  ⁃ Command `ement-describe-room' shows whether a room is a space or a
    normal room.
  ⁃ Command `ement-view-space' shows the space's name and alias.
  ⁃ Command `ement-room-scroll-up-mark-read' moves the fully read marker
    to the top of the window (when the marker's position is within the
    range of known events), rather than only moving it when at the end
    of the buffer.  (This eases the process of gradually reading a long
    backlog of messages.)
  ⁃ Improve readme export settings.

  *Fixes*
  ⁃ Extra indentation of some membership events.  (Thanks to [Steven
    Allen].)
  ⁃ Customization group for faces.
  ⁃ Don't reinitialize `ement-room-list-mode' when room list buffer is
    refreshed.  ([#146].  Thanks to [Ted Reed] for reporting.)
  ⁃ Don't fetch old events when scrolling to the bottom of a room buffer
    (only when scrolling to the top).  (Thanks to [Steven Allen].)
  ⁃ Minor improvements to auto-detection of homeserver URIs.  (See
    [#24].  Thanks to [Phil Sainty].)
  ⁃ Uploading of certain filetypes (e.g. Emacs would decompress some
    archives before uploading).  Thanks to `@welkinsl:matrix.org' for
    reporting.
  ⁃ Messages edited multiple times sometimes weren't correctly replaced.


[#24] <https://github.com/alphapapa/ement.el/issues/24>

[Jeffrey Stoffers] <https://github.com/Necronian>

[Phil Sainty] <https://github.com/phil-s>

[Jakub Kadlčík] <https://github.com/FrostyX>

[Juanjo Presa] <https://github.com/oneingan>

[Steven Allen] <https://github.com/Stebalien>

[#146] <https://github.com/alphapapa/ement.el/issues/146>

[Ted Reed] <https://github.com/treed>

[#24]
<https://github.com/alphapapa/ement.el/issues/24#issuecomment-1569518713>


0.9.3
─────

  *Fixes*
  ⁃ Another attempt at restoring position in room list when refreshing.
  ⁃ Command `ement-room-list-next-unread'.


0.9.2
─────

  *Fixes*
  ⁃ Restore position in room list when refreshing.
  ⁃ Completion in minibuffer.


0.9.1
─────

  *Fixes*
  ⁃ Error in `ement-room-list' command upon initial sync.


0.9
───

  *Additions*

  ⁃ Option `ement-room-timestamp-header-align' controls how timestamp
    headers are aligned in room buffers.
  ⁃ Option `ement-room-view-hook' runs functions when `ement-room-view'
    is called.  (By default, it refreshes the room list buffer.)
  ⁃ In the room list, middle-clicking a room which has a buffer closes
    its buffer.
  ⁃ Basic support for video events.  (Thanks to [Arto Jantunen].)

  *Changes*

  ⁃ Using new option `ement-room-timestamp-header-align', timestamp
    headers default to right-aligned.  (With default settings, this
    keeps them near message timestamps and makes for a cleaner
    appearance.)

  *Fixes*

  ⁃ Recognition of certain MXID or displayname forms in outgoing
    messages when linkifying (aka "pilling") them.
  ⁃ Unreadable room avatar images no longer cause errors.  (Fixes
    [#147].  Thanks to [@jgarte] for reporting.)
  ⁃ Don't error in `ement-room-list' when no rooms are joined.  (Fixes
    [#123].  Thanks to [@Kabouik] and [Omar Antolín Camarena] for
    reporting.)
  ⁃ Enable member/room completion in compose buffers.  (Fixes [#115].
    Thanks to Thanks to [Justus Piater] and [Caleb Chase] for
    reporting.)


[Arto Jantunen] <https://github.com/viiru->

[#147] <https://github.com/alphapapa/ement.el/issues/147>

[@jgarte] <https://github.com/jgarte>

[#123] <https://github.com/alphapapa/ement.el/issues/123>

[@Kabouik] <https://github.com/Kabouik>

[Omar Antolín Camarena] <https://github.com/oantolin>

[#115] <https://github.com/alphapapa/ement.el/issues/115>

[Justus Piater] <https://github.com/piater>

[Caleb Chase] <https://github.com/chasecaleb>


0.8.3
─────

  *Fixes*

  ⁃ Avoid use of `pcase''s `(map :KEYWORD)' form.  (This can cause a
    broken installation on older versions of Emacs that have an older
    version of the `map' library loaded, such as Emacs 27.2 included in
    Debian 11.  Since there's no way to force Emacs to actually load the
    version of `map' required by this package before installing it
    (which would naturally happen upon restarting Emacs), we can only
    avoid using such forms while these versions of Emacs are widely
    used.)


0.8.2
─────

  *Fixes*

  ⁃ Deduplicate grouped membership events.


0.8.1
─────

  Added missing changelog entry (of course).


0.8
───

  *Additions*
  ⁃ Command `ement-create-space' creates a new space.
  ⁃ Command `ement-room-toggle-space' toggles a room's membership in a
    space (a way to group rooms in Matrix).
  ⁃ Visibility of sections in the room list is saved across sessions.
  ⁃ Command `ement-room-list-kill-buffer' kills a room's buffer from the
    room list.
  ⁃ Set `device_id' and `initial_device_display_name' upon login
    (e.g. `Ement.el: username@hostname').  ([#134].  Thanks to [Arto
    Jantunen] for reporting.)

  *Changes*

  ⁃ Room-related commands may be called interactively with a universal
    prefix to prompt for the room/session (allowing to send events or
    change settings in rooms other than the current one).
  ⁃ Command `ement-room-list' reuses an existing window showing the room
    list when possible.  ([#131].  Thanks to [Jeff Bowman] for
    suggesting.)
  ⁃ Command `ement-tag-room' toggles tags (rather than adding by default
    and removing when called with a prefix).
  ⁃ Default room grouping now groups "spaced" rooms separately.

  *Fixes*

  ⁃ Message format filter works properly when writing replies.
  ⁃ Improve insertion of sender name headers when using the "Elemental"
    message format.
  ⁃ Prompts in commands `ement-leave-room' and `ement-forget-room'.


[#134] <https://github.com/alphapapa/ement.el/issues/134>

[Arto Jantunen] <https://github.com/viiru->

[#131] <https://github.com/alphapapa/ement.el/issues/131>

[Jeff Bowman] <https://github.com/jeffbowman>


0.7
───

  *Additions*

  ⁃ Command `ement-room-override-name' sets a local override for a
    room's display name.  (Especially helpful for 1:1 rooms and bridged
    rooms.  See [MSC3015].)

  *Changes*

  ⁃ Improve display of room tombstones (displayed at top and bottom of
    buffer, and new room ID is linked to join).
  ⁃ Use descriptive prompts in `ement-leave-room' and
    `ement-forget-room' commands.

  *Fixes*

  ⁃ Command `ement-view-space' when called from a room buffer.  (Thanks
    to [Richard Brežák] for reporting.)
  ⁃ Don't call `display-buffer' when reverting room list buffer.  (Fixes
    [#121].  Thanks to [mekeor] for reporting.)
  ⁃ Retry sync for network timeouts.  (Accidentally broken in v0.6.)

  *Internal*

  ⁃ Function `ement-put-account-data' accepts `:room' argument to put on
    a room's account data.


[MSC3015]
<https://github.com/matrix-org/matrix-spec-proposals/pull/3015#issuecomment-1451017296>

[Richard Brežák] <https://github.com/MagicRB>

[#121] <https://github.com/alphapapa/ement.el/issues/121>

[mekeor] <https://github.com/mekeor>


0.6
───

  *Additions*
  ⁃ Command `ement-view-space' to view a space's rooms in a directory
    buffer.

  *Changes*
  ⁃ Improve `ement-describe-room' command (formatting, bindings).

  *Fixes*
  ⁃ Retry sync for HTTP 502 "Bad Gateway" errors.
  ⁃ Formatting of unban events.
  ⁃ Update password authentication according to newer Matrix spec.
    (Fixes compatibility with Conduit servers.  [#66].  Thanks to
    [Travis Peacock], [Arto Jantunen], and [Stephen D].)
  ⁃ Image scaling issues.  (Thanks to [Visuwesh].)


[#66] <https://github.com/alphapapa/ement.el/issues/66>

[Travis Peacock] <https://github.com/tpeacock19>

[Arto Jantunen] <https://github.com/viiru->

[Stephen D] <https://github.com/scd31>

[Visuwesh] <https://github.com/vizs>


0.5.2
─────

  *Fixes*
  ⁃ Apply `ement-initial-sync-timeout' properly (important for when the
    homeserver is slow to respond).


0.5.1
─────

  *Fixes*
  ⁃ Autoload `ement-directory' commands.
  ⁃ Faces in `ement-directory' listings.


0.5
───

  *Additions*
  ⁃ Present "joined-and-left" and "rejoined-and-left" membership event
    pairs as such.
  ⁃ Process and show rooms' canonical alias events.

  *Changes*
  ⁃ The [taxy.el]-based room list, with programmable, smart grouping, is
    now the default `ement-room-list'.  (The old,
    `tabulated-list-mode'-based room list is available as
    `ement-tabulated-room-list'.)
  ⁃ When selecting a room to view with completion, don't offer spaces.
  ⁃ When selecting a room with completion, empty aliases and topics are
    omitted instead of being displayed as nil.

  *Fixes*
  ⁃ Use of send-message filter when replying.
  ⁃ Replies may be written in compose buffers.


[taxy.el] <https://github.com/alphapapa/taxy.el>


0.4.1
─────

  *Fixes*
  ⁃ Don't show "curl process interrupted" message when updating a read
    marker's position again.


0.4
───

  *Additions*
  ⁃ Option `ement-room-unread-only-counts-notifications', now enabled by
    default, causes rooms' unread status to be determined only by their
    notification counts (which are set by the server and depend on
    rooms' notification settings).
  ⁃ Command `ement-room-set-notification-state' sets a room's
    notification state (imitating Element's user-friendly presets).
  ⁃ Room buffers' Transient menus show the room's notification state
    (imitating Element's user-friendly presets).
  ⁃ Command `ement-set-display-name' sets the user's global displayname.
  ⁃ Command `ement-room-set-display-name' sets the user's displayname in
    a room (which is also now displayed in the room's Transient menu).
  ⁃ Column `Notifications' in the `ement-taxy-room-list' buffer shows
    rooms' notification state.
  ⁃ Option `ement-interrupted-sync-hook' allows customization of how
    sync interruptions are handled.  (Now, by default, a warning is
    displayed instead of merely a message.)

  *Changes*
  ⁃ When a room's read receipt is updated, the room's buffer is also
    marked as unmodified.  (In concert with the new option, this makes
    rooms' unread status more intuitive.)

  *Fixes*
  ⁃ Binding of command `ement-forget-room' in room buffers.
  ⁃ Highlighting of `@room' mentions.


0.3.1
─────

  *Fixes*
  ⁃ Room unread status (when the last event in a room is sent by the
    local user, the room is considered read).


0.3
───

  *Additions*
  ⁃ Command `ement-directory' shows a server's room directory.
  ⁃ Command `ement-directory-search' searches a server's room directory.
  ⁃ Command `ement-directory-next' fetches the next batch of rooms in a
    directory.
  ⁃ Command `ement-leave-room' accepts a `FORCE-P' argument
    (interactively, with prefix) to leave a room without prompting.
  ⁃ Command `ement-forget-room' accepts a `FORCE-P' argument
    (interactively, with prefix) to also leave the room, and to forget
    it without prompting.
  ⁃ Option `ement-notify-mark-frame-urgent-predicates' marks the frame
    as urgent when (by default) a message mentions the local user or
    "@room" and the message's room has an open buffer.

  *Changes*
  ⁃ Minor improvements to date/time headers.

  *Fixes*
  ⁃ Command `ement-describe-room' for rooms without topics.
  ⁃ Improve insertion of old messages around existing timestamp headers.
  ⁃ Reduce D-Bus notification system check timeout to 2 seconds (from
    the default of 25).
  ⁃ Compatibility with Emacs 27.


0.2.1
─────

  *Fixes*
  ⁃ Info manual export filename.


0.2
───

  *Changes*
  ⁃ Read receipts are re-enabled.  (They're now implemented with a
    global idle timer rather than `window-scroll-functions', which
    sometimes caused a strange race condition that could cause Emacs to
    become unresponsive or crash.)
  ⁃ When determining whether a room is considered unread, non-message
    events like membership changes, reactions, etc. are ignored.  This
    fixes a bug that caused certain rooms that had no message events
    (like some bridged rooms) to appear as unread when they shouldn't
    have.  But it's unclear whether this is always preferable (e.g. one
    might want a member leaving a room to cause it to be marked unread),
    so this is classified as a change rather than simply a fix, and more
    improvements may be made to this in the future.  (Fixes [#97].
    Thanks to [Julien Roy] for reporting and testing.)
  ⁃ The `ement-taxy-room-list' view no longer automatically refreshes
    the list if the region is active in the buffer.  (This allows the
    user to operate on multiple rooms without the contents of the buffer
    changing before completing the process.)

  *Fixes*
  ⁃ Links to only rooms (as opposed to links to events in rooms) may be
    activated to join them.
  ⁃ Read receipts mark the last completely visible event (rather than
    one that's only partially displayed).
  ⁃ Prevent error when a room avatar image fails to load.


[#97] <https://github.com/alphapapa/ement.el/issues/97>

[Julien Roy] <https://github.com/MrRoy>


0.1.4
─────

  *Fixed*
  ⁃ Info manual directory headers.


0.1.3
─────

  *Fixed*

  ⁃ Temporarily disable sending of read receipts due to an unusual bug
    that could cause Emacs to become unresponsive.  (The feature will be
    re-enabled in a future release.)


0.1.2
─────

  *Fixed*
  ⁃ Function `ement-room-sync' correctly updates room-list buffers.
    (Thanks to [Visuwesh].)
  ⁃ Only send D-Bus notifications when supported.  (Fixes [#83].  Thanks
    to [Tassilo Horn].)


[Visuwesh] <https://github.com/vizs>

[#83] <https://github.com/alphapapa/ement.el/issues/83>

[Tassilo Horn] <https://github.com/tsdh>


0.1.1
─────

  *Fixed*
  ⁃ Function `ement-room-scroll-up-mark-read' selects the correct room
    window.
  ⁃ Option `ement-room-list-avatars' defaults to what function
    `display-images-p' returns.


0.1
───

  After almost two years of development, the first tagged release.
  Submitted to GNU ELPA.


4 Development
═════════════

  Bug reports, feature requests, suggestions — /oh my/!


Copyright Assignment
────────────────────

  Ement.el is published in GNU ELPA and is considered part of GNU Emacs.
  Therefore, cumulative contributions of more than 15 lines of code
  require that the author assign copyright of such contributions to the
  FSF.  Authors who are interested in doing so may contact
  [assign@gnu.org] to request the appropriate form.


[assign@gnu.org] <mailto:assign@gnu.org>


Matrix spec in Org format
─────────────────────────

  An Org-formatted version of the Matrix spec is available in the
  [meta/spec] branch.


[meta/spec] <https://github.com/alphapapa/ement.el/tree/meta/spec>


Rationale
─────────

  /This section is preserved for posterity.  As it says, Ement.el has
  long since surpassed `matrix-client', which should no longer be used./

  Why write a new Emacs Matrix client when there is already
  [matrix-client.el], by the same author, no less?  A few reasons:

  • `matrix-client' uses an older version of the Matrix spec, r0.3.0,
    with a few elements of r0.4.0 grafted in.  Bringing it up to date
    with the current version of the spec, r0.6.1, would be more work
    than to begin with the current version.  Ement.el targets r0.6.1
    from the beginning.
  • `matrix-client' does not use Matrix's lazy-loading feature (which
    was added to the specification later), so initial sync requests can
    take a long time for the server to process and can be large
    (sometimes tens of megabytes of JSON for the client to process!).
    Ement.el uses lazy-loading, which significantly improves
    performance.
  • `matrix-client' automatically makes buffers for every room a user
    has joined, even if the user doesn't currently want to watch a room.
    Ement.el opens room buffers on-demand, improving performance by not
    having to insert events into buffers for rooms the user isn't
    watching.
  • `matrix-client' was developed without the intention of publishing it
    to, e.g. MELPA or ELPA.  It has several dependencies, and its code
    does not always install or compile cleanly due to macro-expansion
    issues (apparently depending on the user's Emacs config).  Ement.el
    is designed to have minimal dependencies outside of Emacs (currently
    only one, `plz', which could be imported into the project), and
    every file is linted and compiles cleanly using [makem.sh].
  • `matrix-client' uses EIEIO, probably unnecessarily, since few, if
    any, of the benefits of EIEIO are realized in it.  Ement.el uses
    structs instead.
  • `matrix-client' uses bespoke code for inserting messages into
    buffers, which works pretty well, but has a few minor bugs which are
    difficult to track down.  Ement.el uses Emacs's built-in (and
    perhaps little-known) `ewoc' library, which makes it much simpler
    and more reliable to insert and update messages in buffers, and
    enables the development of advanced UI features more easily.
  • `matrix-client' was, to a certain extent, designed to imitate other
    messaging apps.  The result is, at least when used with the
    `matrix-client-frame' command, fairly pleasing to use, but isn't
    especially "Emacsy."  Ement.el is intended to better fit into
    Emacs's paradigms.
  • `matrix-client''s long name makes for long symbol names, which makes
    for tedious, verbose code.  `ement' is easy to type and makes for
    concise, readable code.
  • The author has learned much since writing `matrix-client' and hopes
    to write simpler, more readable, more maintainable code in Ement.el.
    It's hoped that this will enable others to contribute more easily.

  Note that, while `matrix-client' remains usable, and probably will for
  some time to come, Ement.el has now surpassed it in every way.  The
  only reason to choose `matrix-client' instead is if one is using an
  older version of Emacs that isn't supported by Ement.el.


[matrix-client.el] <https://github.com/alphapapa/matrix-client.el>

[makem.sh] <https://github.com/alphapapa/makem.sh>


5 License
═════════

  GPLv3
