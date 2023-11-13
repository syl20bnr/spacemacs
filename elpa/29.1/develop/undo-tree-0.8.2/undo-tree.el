;;; undo-tree.el --- Treat undo history as a tree  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2021  Free Software Foundation, Inc

;; Author: Toby Cubitt <toby-undo-tree@dr-qubit.org>
;; Maintainer: Toby Cubitt <toby-undo-tree@dr-qubit.org>
;; Version: 0.8.2
;; Keywords: convenience, files, undo, redo, history, tree
;; Package-Requires: ((queue "0.2"))
;; URL: https://www.dr-qubit.org/undo-tree.html
;; Repository: https://gitlab.com/tsc25/undo-tree

;; This file is part of Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Emacs has a powerful undo system. Unlike the standard undo/redo system in
;; most software, it allows you to recover *any* past state of a buffer
;; (whereas the standard undo/redo system can lose past states as soon as you
;; redo). However, this power comes at a price: many people find Emacs' undo
;; system confusing and difficult to use, spawning a number of packages that
;; replace it with the less powerful but more intuitive undo/redo system.
;;
;; Both the loss of data with standard undo/redo, and the confusion of Emacs'
;; undo, stem from trying to treat undo history as a linear sequence of
;; changes. It's not. The `undo-tree-mode' provided by this package replaces
;; Emacs' undo system with a system that treats undo history as what it is: a
;; branching tree of changes. This simple idea allows the more intuitive
;; behaviour of the standard undo/redo system to be combined with the power of
;; never losing any history. An added side bonus is that undo history can in
;; some cases be stored more efficiently, allowing more changes to accumulate
;; before Emacs starts discarding history.
;;
;; The only downside to this more advanced yet simpler undo system is that it
;; was inspired by Vim. But, after all, most successful religions steal the
;; best ideas from their competitors!
;;
;;
;; Installation
;; ============
;;
;; This package has only been tested with Emacs versions 24 and CVS. It should
;; work in Emacs versions 22 and 23 too, but will not work without
;; modifications in earlier versions of Emacs.
;;
;; To install `undo-tree-mode', make sure this file is saved in a directory in
;; your `load-path', and add the line:
;;
;;   (require 'undo-tree)
;;
;; to your .emacs file. Byte-compiling undo-tree.el is recommended (e.g. using
;; "M-x byte-compile-file" from within emacs).
;;
;; If you want to replace the standard Emacs' undo system with the
;; `undo-tree-mode' system in all buffers, you can enable it globally by
;; adding:
;;
;;   (global-undo-tree-mode)
;;
;; to your .emacs file.
;;
;;
;; Quick-Start
;; ===========
;;
;; If you're the kind of person who likes to jump in the car and drive,
;; without bothering to first figure out whether the button on the left dips
;; the headlights or operates the ejector seat (after all, you'll soon figure
;; it out when you push it), then here's the minimum you need to know:
;;
;; `undo-tree-mode' and `global-undo-tree-mode'
;;   Enable undo-tree mode (either in the current buffer or globally).
;;
;; C-_  C-/  (`undo-tree-undo')
;;   Undo changes.
;;
;; M-_  C-?  (`undo-tree-redo')
;;   Redo changes.
;;
;; `undo-tree-switch-branch'
;;   Switch undo-tree branch.
;;   (What does this mean? Better press the button and see!)
;;
;; C-x u  (`undo-tree-visualize')
;;   Visualize the undo tree.
;;   (Better try pressing this button too!)
;;
;; C-x r u  (`undo-tree-save-state-to-register')
;;   Save current buffer state to register.
;;
;; C-x r U  (`undo-tree-restore-state-from-register')
;;   Restore buffer state from register.
;;
;;
;;
;; In the undo-tree visualizer:
;;
;; <up>  p  C-p  (`undo-tree-visualize-undo')
;;   Undo changes.
;;
;; <down>  n  C-n  (`undo-tree-visualize-redo')
;;   Redo changes.
;;
;; <left>  b  C-b  (`undo-tree-visualize-switch-branch-left')
;;   Switch to previous undo-tree branch.
;;
;; <right>  f  C-f  (`undo-tree-visualize-switch-branch-right')
;;   Switch to next undo-tree branch.
;;
;; C-<up>  M-{  (`undo-tree-visualize-undo-to-x')
;;   Undo changes up to last branch point.
;;
;; C-<down>  M-}  (`undo-tree-visualize-redo-to-x')
;;   Redo changes down to next branch point.
;;
;; <down>  n  C-n  (`undo-tree-visualize-redo')
;;   Redo changes.
;;
;; <mouse-1>  (`undo-tree-visualizer-mouse-set')
;;   Set state to node at mouse click.
;;
;; t  (`undo-tree-visualizer-toggle-timestamps')
;;   Toggle display of time-stamps.
;;
;; d  (`undo-tree-visualizer-toggle-diff')
;;   Toggle diff display.
;;
;; s  (`undo-tree-visualizer-selection-mode')
;;   Toggle keyboard selection mode.
;;
;; q  (`undo-tree-visualizer-quit')
;;   Quit undo-tree-visualizer.
;;
;; C-q  (`undo-tree-visualizer-abort')
;;   Abort undo-tree-visualizer.
;;
;; ,  <
;;   Scroll left.
;;
;; .  >
;;   Scroll right.
;;
;; <pgup>  M-v
;;   Scroll up.
;;
;; <pgdown>  C-v
;;   Scroll down.
;;
;;
;;
;; In visualizer selection mode:
;;
;; <up>  p  C-p  (`undo-tree-visualizer-select-previous')
;;   Select previous node.
;;
;; <down>  n  C-n  (`undo-tree-visualizer-select-next')
;;   Select next node.
;;
;; <left>  b  C-b  (`undo-tree-visualizer-select-left')
;;   Select left sibling node.
;;
;; <right>  f  C-f  (`undo-tree-visualizer-select-right')
;;   Select right sibling node.
;;
;; <pgup>  M-v
;;   Select node 10 above.
;;
;; <pgdown>  C-v
;;   Select node 10 below.
;;
;; <enter>  (`undo-tree-visualizer-set')
;;   Set state to selected node and exit selection mode.
;;
;; s  (`undo-tree-visualizer-mode')
;;   Exit selection mode.
;;
;; t  (`undo-tree-visualizer-toggle-timestamps')
;;   Toggle display of time-stamps.
;;
;; d  (`undo-tree-visualizer-toggle-diff')
;;   Toggle diff display.
;;
;; q  (`undo-tree-visualizer-quit')
;;   Quit undo-tree-visualizer.
;;
;; C-q  (`undo-tree-visualizer-abort')
;;   Abort undo-tree-visualizer.
;;
;; ,  <
;;   Scroll left.
;;
;; .  >
;;   Scroll right.
;;
;;
;;
;; Persistent undo history:
;;
;; Note: Requires Emacs version 24.3 or higher.
;;
;; `undo-tree-auto-save-history' (variable)
;;    automatically save and restore undo-tree history along with buffer
;;    (disabled by default)
;;
;; `undo-tree-save-history' (command)
;;    manually save undo history to file
;;
;; `undo-tree-load-history' (command)
;;    manually load undo history from file
;;
;;
;;
;; Compressing undo history:
;;
;;   Undo history files cannot grow beyond the maximum undo tree size, which
;;   is limited by `undo-limit', `undo-strong-limit' and
;;   `undo-outer-limit'. Nevertheless, undo history files can grow quite
;;   large. If you want to automatically compress undo history, add the
;;   following advice to your .emacs file (replacing ".gz" with the filename
;;   extension of your favourite compression algorithm):
;;
;;   (defadvice undo-tree-make-history-save-file-name
;;     (after undo-tree activate)
;;     (setq ad-return-value (concat ad-return-value ".gz")))
;;
;;
;;
;;
;; Undo Systems
;; ============
;;
;; To understand the different undo systems, it's easiest to consider an
;; example. Imagine you make a few edits in a buffer. As you edit, you
;; accumulate a history of changes, which we might visualize as a string of
;; past buffer states, growing downwards:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;
;;
;; Now imagine that you undo the last two changes. We can visualize this as
;; rewinding the current state back two steps:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;                                |
;;                                |
;;                                o
;;                                |
;;                                |
;;                                o
;;
;;
;; However, this isn't a good representation of what Emacs' undo system
;; does. Instead, it treats the undos as *new* changes to the buffer, and adds
;; them to the history:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (buffer state before undo)
;;                                |
;;                                |
;;                                o  (first undo)
;;                                |
;;                                |
;;                                x  (second undo)
;;
;;
;; Actually, since the buffer returns to a previous state after an undo,
;; perhaps a better way to visualize it is to imagine the string of changes
;; turning back on itself:
;;
;;        (initial buffer state)  o
;;                                |
;;                                |
;;                  (first edit)  o  x  (second undo)
;;                                |  |
;;                                |  |
;;                 (second edit)  o  o  (first undo)
;;                                | /
;;                                |/
;;                                o  (buffer state before undo)
;;
;; Treating undos as new changes might seem a strange thing to do. But the
;; advantage becomes clear as soon as we imagine what happens when you edit
;; the buffer again. Since you've undone a couple of changes, new edits will
;; branch off from the buffer state that you've rewound to. Conceptually, it
;; looks like this:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (new edit)
;;                                |
;;                                |
;;                                o
;;
;; The standard undo/redo system only lets you go backwards and forwards
;; linearly. So as soon as you make that new edit, it discards the old
;; branch. Emacs' undo just keeps adding changes to the end of the string. So
;; the undo history in the two systems now looks like this:
;;
;;            Undo/Redo:                      Emacs' undo
;;
;;               o                                o
;;               |                                |
;;               |                                |
;;               o                                o  o
;;               .\                               |  |\
;;               . \                              |  | \
;;               .  x  (new edit)                 o  o  |
;;   (discarded  .                                | /   |
;;     branch)   .                                |/    |
;;               .                                o     |
;;                                                      |
;;                                                      |
;;                                                      x  (new edit)
;;
;; Now, what if you change your mind about those undos, and decide you did
;; like those other changes you'd made after all? With the standard undo/redo
;; system, you're lost. There's no way to recover them, because that branch
;; was discarded when you made the new edit.
;;
;; However, in Emacs' undo system, those old buffer states are still there in
;; the undo history. You just have to rewind back through the new edit, and
;; back through the changes made by the undos, until you reach them. Of
;; course, since Emacs treats undos (even undos of undos!) as new changes,
;; you're really weaving backwards and forwards through the history, all the
;; time adding new changes to the end of the string as you go:
;;
;;                       o
;;                       |
;;                       |
;;                       o  o     o  (undo new edit)
;;                       |  |\    |\
;;                       |  | \   | \
;;                       o  o  |  |  o  (undo the undo)
;;                       | /   |  |  |
;;                       |/    |  |  |
;;      (trying to get   o     |  |  x  (undo the undo)
;;       to this state)        | /
;;                             |/
;;                             o
;;
;; So far, this is still reasonably intuitive to use. It doesn't behave so
;; differently to standard undo/redo, except that by going back far enough you
;; can access changes that would be lost in standard undo/redo.
;;
;; However, imagine that after undoing as just described, you decide you
;; actually want to rewind right back to the initial state. If you're lucky,
;; and haven't invoked any command since the last undo, you can just keep on
;; undoing until you get back to the start:
;;
;;      (trying to get   o              x  (got there!)
;;       to this state)  |              |
;;                       |              |
;;                       o  o     o     o  (keep undoing)
;;                       |  |\    |\    |
;;                       |  | \   | \   |
;;                       o  o  |  |  o  o  (keep undoing)
;;                       | /   |  |  | /
;;                       |/    |  |  |/
;;      (already undid   o     |  |  o  (got this far)
;;       to this state)        | /
;;                             |/
;;                             o
;;
;; But if you're unlucky, and you happen to have moved the point (say) after
;; getting to the state labelled "got this far", then you've "broken the undo
;; chain". Hold on to something solid, because things are about to get
;; hairy. If you try to undo now, Emacs thinks you're trying to undo the
;; undos! So to get back to the initial state you now have to rewind through
;; *all* the changes, including the undos you just did:
;;
;;      (trying to get   o                          x  (finally got there!)
;;       to this state)  |                          |
;;                       |                          |
;;                       o  o     o     o     o     o
;;                       |  |\    |\    |\    |\    |
;;                       |  | \   | \   | \   | \   |
;;                       o  o  |  |  o  o  |  |  o  o
;;                       | /   |  |  | /   |  |  | /
;;                       |/    |  |  |/    |  |  |/
;;      (already undid   o     |  |  o<.   |  |  o
;;       to this state)        | /     :   | /
;;                             |/      :   |/
;;                             o       :   o
;;                                     :
;;                             (got this far, but
;;                              broke the undo chain)
;;
;; Confused?
;;
;; In practice you can just hold down the undo key until you reach the buffer
;; state that you want. But whatever you do, don't move around in the buffer
;; to *check* that you've got back to where you want! Because you'll break the
;; undo chain, and then you'll have to traverse the entire string of undos
;; again, just to get back to the point at which you broke the
;; chain. Undo-in-region and commands such as `undo-only' help to make using
;; Emacs' undo a little easier, but nonetheless it remains confusing for many
;; people.
;;
;;
;; So what does `undo-tree-mode' do? Remember the diagram we drew to represent
;; the history we've been discussing (make a few edits, undo a couple of them,
;; and edit again)? The diagram that conceptually represented our undo
;; history, before we started discussing specific undo systems? It looked like
;; this:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (current state)
;;                                |
;;                                |
;;                                o
;;
;; Well, that's *exactly* what the undo history looks like to
;; `undo-tree-mode'.  It doesn't discard the old branch (as standard undo/redo
;; does), nor does it treat undos as new changes to be added to the end of a
;; linear string of buffer states (as Emacs' undo does). It just keeps track
;; of the tree of branching changes that make up the entire undo history.
;;
;; If you undo from this point, you'll rewind back up the tree to the previous
;; state:
;;
;;                                o
;;                                |
;;                                |
;;                                x  (undo)
;;                                |\
;;                                | \
;;                                o  o
;;                                |
;;                                |
;;                                o
;;
;; If you were to undo again, you'd rewind back to the initial state. If on
;; the other hand you redo the change, you'll end up back at the bottom of the
;; most recent branch:
;;
;;                                o  (undo takes you here)
;;                                |
;;                                |
;;                                o  (start here)
;;                                |\
;;                                | \
;;                                o  x  (redo takes you here)
;;                                |
;;                                |
;;                                o
;;
;; So far, this is just like the standard undo/redo system. But what if you
;; want to return to a buffer state located on a previous branch of the
;; history? Since `undo-tree-mode' keeps the entire history, you simply need
;; to tell it to switch to a different branch, and then redo the changes you
;; want:
;;
;;                                o
;;                                |
;;                                |
;;                                o  (start here, but switch
;;                                |\  to the other branch)
;;                                | \
;;                        (redo)  o  o
;;                                |
;;                                |
;;                        (redo)  x
;;
;; Now you're on the other branch, if you undo and redo changes you'll stay on
;; that branch, moving up and down through the buffer states located on that
;; branch. Until you decide to switch branches again, of course.
;;
;; Real undo trees might have multiple branches and sub-branches:
;;
;;                                o
;;                            ____|______
;;                           /           \
;;                          o             o
;;                      ____|__         __|
;;                     /    |  \       /   \
;;                    o     o   o     o     x
;;                    |               |
;;                   / \             / \
;;                  o   o           o   o
;;
;; Trying to imagine what Emacs' undo would do as you move about such a tree
;; will likely frazzle your brain circuits! But in `undo-tree-mode', you're
;; just moving around this undo history tree. Most of the time, you'll
;; probably only need to stay on the most recent branch, in which case it
;; behaves like standard undo/redo, and is just as simple to understand. But
;; if you ever need to recover a buffer state on a different branch, the
;; possibility of switching between branches and accessing the full undo
;; history is still there.
;;
;;
;;
;; The Undo-Tree Visualizer
;; ========================
;;
;; Actually, it gets better. You don't have to imagine all these tree
;; diagrams, because `undo-tree-mode' includes an undo-tree visualizer which
;; draws them for you! In fact, it draws even better diagrams: it highlights
;; the node representing the current buffer state, it highlights the current
;; branch, and you can toggle the display of time-stamps (by hitting "t") and
;; a diff of the undo changes (by hitting "d"). (There's one other tiny
;; difference: the visualizer puts the most recent branch on the left rather
;; than the right.)
;;
;; Bring up the undo tree visualizer whenever you want by hitting "C-x u".
;;
;; In the visualizer, the usual keys for moving up and down a buffer instead
;; move up and down the undo history tree (e.g. the up and down arrow keys, or
;; "C-n" and "C-p"). The state of the "parent" buffer (the buffer whose undo
;; history you are visualizing) is updated as you move around the undo tree in
;; the visualizer. If you reach a branch point in the visualizer, the usual
;; keys for moving forward and backward in a buffer instead switch branch
;; (e.g. the left and right arrow keys, or "C-f" and "C-b").
;;
;; Clicking with the mouse on any node in the visualizer will take you
;; directly to that node, resetting the state of the parent buffer to the
;; state represented by that node.
;;
;; You can also select nodes directly using the keyboard, by hitting "s" to
;; toggle selection mode. The usual motion keys now allow you to move around
;; the tree without changing the parent buffer. Hitting <enter> will reset the
;; state of the parent buffer to the state represented by the currently
;; selected node.
;;
;; It can be useful to see how long ago the parent buffer was in the state
;; represented by a particular node in the visualizer. Hitting "t" in the
;; visualizer toggles the display of time-stamps for all the nodes. (Note
;; that, because of the way `undo-tree-mode' works, these time-stamps may be
;; somewhat later than the true times, especially if it's been a long time
;; since you last undid any changes.)
;;
;; To get some idea of what changes are represented by a given node in the
;; tree, it can be useful to see a diff of the changes. Hit "d" in the
;; visualizer to toggle a diff display. This normally displays a diff between
;; the current state and the previous one, i.e. it shows you the changes that
;; will be applied if you undo (move up the tree). However, the diff display
;; really comes into its own in the visualizer's selection mode (see above),
;; where it instead shows a diff between the current state and the currently
;; selected state, i.e. it shows you the changes that will be applied if you
;; reset to the selected state.
;;
;; (Note that the diff is generated by the Emacs `diff' command, and is
;; displayed using `diff-mode'. See the corresponding customization groups if
;; you want to customize the diff display.)
;;
;; Finally, hitting "q" will quit the visualizer, leaving the parent buffer in
;; whatever state you ended at. Hitting "C-q" will abort the visualizer,
;; returning the parent buffer to whatever state it was originally in when the
;; visualizer was invoked.
;;
;;
;;
;; Undo-in-Region
;; ==============
;;
;; Emacs allows a very useful and powerful method of undoing only selected
;; changes: when a region is active, only changes that affect the text within
;; that region will be undone. With the standard Emacs undo system, changes
;; produced by undoing-in-region naturally get added onto the end of the
;; linear undo history:
;;
;;                       o
;;                       |
;;                       |  x  (second undo-in-region)
;;                       o  |
;;                       |  |
;;                       |  o  (first undo-in-region)
;;                       o  |
;;                       | /
;;                       |/
;;                       o
;;
;; You can of course redo these undos-in-region as usual, by undoing the
;; undos:
;;
;;                       o
;;                       |
;;                       |  o_
;;                       o  | \
;;                       |  |  |
;;                       |  o  o  (undo the undo-in-region)
;;                       o  |  |
;;                       | /   |
;;                       |/    |
;;                       o     x  (undo the undo-in-region)
;;
;;
;; In `undo-tree-mode', undo-in-region works much the same way: when there's
;; an active region, undoing only undoes changes that affect that region. In
;; `undo-tree-mode', redoing when there's an active region similarly only
;; redoes changes that affect that region.
;;
;; However, the way these undo- and redo-in-region changes are recorded in the
;; undo history is quite different. The good news is, you don't need to
;; understand this to use undo- and redo-in-region in `undo-tree-mode' - just
;; go ahead and use them! They'll probably work as you expect. But if you're
;; masochistic enough to want to understand conceptually what's happening to
;; the undo tree as you undo- and redo-in-region, then read on...
;;
;;
;; Undo-in-region creates a new branch in the undo history. The new branch
;; consists of an undo step that undoes some of the changes that affect the
;; current region, and another step that undoes the remaining changes needed
;; to rejoin the previous undo history.
;;
;;      Previous undo history                Undo-in-region
;;
;;               o                                o
;;               |                                |
;;               |                                |
;;               |                                |
;;               o                                o
;;               |                                |
;;               |                                |
;;               |                                |
;;               o                                o_
;;               |                                | \
;;               |                                |  x  (undo-in-region)
;;               |                                |  |
;;               x                                o  o
;;
;; As long as you don't change the active region after undoing-in-region,
;; continuing to undo-in-region extends the new branch, pulling more changes
;; that affect the current region into an undo step immediately above your
;; current location in the undo tree, and pushing the point at which the new
;; branch is attached further up the tree:
;;
;;      First undo-in-region                 Second undo-in-region
;;
;;               o                                o
;;               |                                |
;;               |                                |
;;               |                                |
;;               o                                o_
;;               |                                | \
;;               |                                |  x  (undo-in-region)
;;               |                                |  |
;;               o_                               o  |
;;               | \                              |  |
;;		 |  x                             |  o
;;		 |  |                             |  |
;;		 o  o     			  o  o
;;
;; Redoing takes you back down the undo tree, as usual (as long as you haven't
;; changed the active region after undoing-in-region, it doesn't matter if it
;; is still active):
;;
;;                       o
;;			 |
;;			 |
;;			 |
;;			 o_
;;			 | \
;;			 |  o
;;			 |  |
;;			 o  |
;;			 |  |
;;			 |  o  (redo)
;;			 |  |
;;			 o  x  (redo)
;;
;;
;; What about redo-in-region? Obviously, redo-in-region only makes sense if
;; you have already undone some changes, so that there are some changes to
;; redo! Redoing-in-region splits off a new branch of the undo history below
;; your current location in the undo tree. This time, the new branch consists
;; of a first redo step that redoes some of the redo changes that affect the
;; current region, followed by *all* the remaining redo changes.
;;
;;      Previous undo history                Redo-in-region
;;
;;               o                                o
;;               |                                |
;;               |                                |
;;               |                                |
;;               x                                o_
;;               |                                | \
;;               |                                |  x  (redo-in-region)
;;               |                                |  |
;;               o                                o  |
;;               |                                |  |
;;               |                                |  |
;;               |                                |  |
;;               o                                o  o
;;
;; As long as you don't change the active region after redoing-in-region,
;; continuing to redo-in-region extends the new branch, pulling more redo
;; changes into a redo step immediately below your current location in the
;; undo tree.
;;
;;      First redo-in-region                 Second redo-in-region
;;
;;               o                                 o
;;               |                                 |
;;               |                                 |
;;               |                                 |
;;               o_                                o_
;;               | \                               | \
;;               |  x                              |  o
;;               |  |                              |  |
;;               o  |                              o  |
;;               |  |                              |  |
;;               |  |                              |  x  (redo-in-region)
;;               |  |                              |  |
;;               o  o                              o  o
;;
;; Note that undo-in-region and redo-in-region only ever add new changes to
;; the undo tree, they *never* modify existing undo history. So you can always
;; return to previous buffer states by switching to a previous branch of the
;; tree.



;;; Code:

(require 'cl-lib)
(require 'queue)
(require 'diff)
(require 'gv)



;;; =====================================================================
;;;              Compatibility hacks for older Emacsen

;; `characterp' isn't defined in Emacs versions < 23
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

;; `region-active-p' isn't defined in Emacs versions < 23
(unless (fboundp 'region-active-p)
  (defun region-active-p () (and transient-mark-mode mark-active)))


;; `registerv' defstruct isn't defined in Emacs versions < 24
(unless (fboundp 'registerv-make)
  (defmacro registerv-make (data &rest _dummy) data))

(unless (fboundp 'registerv-data)
  (defmacro registerv-data (data) data))


;; `diff-no-select' and `diff-file-local-copy' aren't defined in Emacs
;; versions < 24 (copied and adapted from Emacs 24)
(unless (fboundp 'diff-no-select)
  (defun diff-no-select (old new &optional switches no-async buf)
    ;; Noninteractive helper for creating and reverting diff buffers
    (unless (bufferp new) (setq new (expand-file-name new)))
    (unless (bufferp old) (setq old (expand-file-name old)))
    (or switches (setq switches diff-switches)) ; If not specified, use default.
    (unless (listp switches) (setq switches (list switches)))
    (or buf (setq buf (get-buffer-create "*Diff*")))
    (let* ((old-alt (diff-file-local-copy old))
	   (new-alt (diff-file-local-copy new))
	   (command
	    (mapconcat 'identity
		       `(,diff-command
			 ;; Use explicitly specified switches
			 ,@switches
			 ,@(mapcar #'shell-quote-argument
				   (nconc
				    (when (or old-alt new-alt)
				      (list "-L" (if (stringp old)
						     old (prin1-to-string old))
					    "-L" (if (stringp new)
						     new (prin1-to-string new))))
				    (list (or old-alt old)
					  (or new-alt new)))))
		       " "))
	   (thisdir default-directory))
      (with-current-buffer buf
	(setq buffer-read-only t)
	(buffer-disable-undo (current-buffer))
	(let ((inhibit-read-only t))
	  (erase-buffer))
	(buffer-enable-undo (current-buffer))
	(diff-mode)
	(set (make-local-variable 'revert-buffer-function)
	     (lambda (_ignore-auto _noconfirm)
	       (diff-no-select old new switches no-async (current-buffer))))
	(setq default-directory thisdir)
	(let ((inhibit-read-only t))
	  (insert command "\n"))
	(if (and (not no-async) (fboundp 'start-process))
	    (let ((proc (start-process "Diff" buf shell-file-name
				       shell-command-switch command)))
	      (set-process-filter proc 'diff-process-filter)
	      (set-process-sentinel
	       proc (lambda (proc _msg)
		      (with-current-buffer (process-buffer proc)
			(diff-sentinel (process-exit-status proc))
			(if old-alt (delete-file old-alt))
			(if new-alt (delete-file new-alt))))))
	  ;; Async processes aren't available.
	  (let ((inhibit-read-only t))
	    (diff-sentinel
	     (call-process shell-file-name nil buf nil
			   shell-command-switch command))
	    (if old-alt (delete-file old-alt))
	    (if new-alt (delete-file new-alt)))))
      buf)))

(unless (fboundp 'diff-file-local-copy)
  (defun diff-file-local-copy (file-or-buf)
    (if (bufferp file-or-buf)
	(with-current-buffer file-or-buf
	  (let ((tempfile (make-temp-file "buffer-content-")))
	    (write-region nil nil tempfile nil 'nomessage)
	    tempfile))
      (file-local-copy file-or-buf))))


;; `user-error' isn't defined in Emacs < 24.3
(unless (fboundp 'user-error)
  (defalias 'user-error 'error)
  ;; prevent debugger being called on user errors
  (add-to-list 'debug-ignored-errors "^No further undo information")
  (add-to-list 'debug-ignored-errors "^No further redo information")
  (add-to-list 'debug-ignored-errors "^No further redo information for region"))





;;; =====================================================================
;;;              Global variables and customization options

(defvar buffer-undo-tree nil
  "Tree of undo entries in current buffer.")
(put 'buffer-undo-tree 'permanent-local t)
(make-variable-buffer-local 'buffer-undo-tree)


(defgroup undo-tree nil
  "Tree undo/redo."
  :group 'undo)


(defcustom undo-tree-limit 80000000
  "Value of `undo-limit' used in `undo-tree-mode'.

If `undo-limit' is larger than `undo-tree-limit', the larger of
the two values will be used.

See also `undo-tree-strong-limit' and `undo-tree-outer-limit'.

Setting this to nil prevents `undo-tree-mode' ever discarding
undo history. (As far as possible. In principle, it is still
possible for Emacs to discard undo history behind
`undo-tree-mode's back.) USE THIS SETTING AT YOUR OWN RISK! Emacs
may crash if undo history exceeds Emacs' available memory. This
is particularly risky if `undo-tree-auto-save-history' is
enabled, as in that case undo history is preserved even between
Emacs sessions."
  :group 'undo-tree
  :type '(choice integer (const nil)))


(defcustom undo-tree-strong-limit 120000000
  "Value of `undo-strong-limit' used in `undo-tree-mode'.

If `undo-strong-limit' is larger than `undo-tree-strong-limit'
the larger of the two values will be used."
  :group 'undo-tree
  :type 'integer)


(defcustom undo-tree-outer-limit 360000000
  "Value of `undo-outer-limit' used in `undo-tree-mode'.

If `undo-outer-limit' is larger than `undo-tree-outer-limit' the
larger of the two values will be used."
  :group 'undo-tree
  :type 'integer)


(defcustom undo-tree-mode-lighter " Undo-Tree"
  "Lighter displayed in mode line
when `undo-tree-mode' is enabled."
  :group 'undo-tree
  :type 'string)


(defcustom undo-tree-incompatible-major-modes '(term-mode)
  "List of major-modes in which `undo-tree-mode' should not be enabled.
\(See `turn-on-undo-tree-mode'.\)"
  :group 'undo-tree
  :type '(repeat symbol))


(defcustom undo-tree-enable-undo-in-region nil
  "When non-nil, enable undo-in-region.

When undo-in-region is enabled, undoing or redoing when the
region is active (in `transient-mark-mode') or with a prefix
argument (not in `transient-mark-mode') only undoes changes
within the current region."
  :group 'undo-tree
  :type 'boolean)


(defcustom undo-tree-auto-save-history t
  "When non-nil, `undo-tree-mode' will save undo history to file
when a buffer is saved to file.

It will automatically load undo history when a buffer is loaded
from file, if an undo save file exists.

By default, undo-tree history is saved to a file called
\".<buffer-file-name>.~undo-tree~\" in the same directory as the
file itself. To save under a different directory, customize
`undo-tree-history-directory-alist' (see the documentation for
that variable for details).

WARNING! `undo-tree-auto-save-history' will not work properly in
Emacs versions prior to 24.3, so it cannot be enabled via
the customization interface in versions earlier than that one. To
ignore this warning and enable it regardless, set
`undo-tree-auto-save-history' to a non-nil value outside of
customize."
  :group 'undo-tree
  :type (if (version-list-< (version-to-list emacs-version) '(24 3))
	    '(choice (const :tag "<disabled>" nil))
	  'boolean))


(defcustom undo-tree-history-directory-alist nil
  "Alist of filename patterns and undo history directory names.
Each element looks like (REGEXP . DIRECTORY).  Undo history for
files with names matching REGEXP will be saved in DIRECTORY.
DIRECTORY may be relative or absolute.  If it is absolute, so
that all matching files are backed up into the same directory,
the file names in this directory will be the full name of the
file backed up with all directory separators changed to `!' to
prevent clashes.  This will not work correctly if your filesystem
truncates the resulting name.

For the common case of all backups going into one directory, the
alist should contain a single element pairing \".\" with the
appropriate directory name.

If this variable is nil, or it fails to match a filename, the
backup is made in the original file's directory.

On MS-DOS filesystems without long names this variable is always
ignored."
  :group 'undo-tree
  :type '(repeat (cons (regexp :tag "Regexp matching filename")
		       (directory :tag "Undo history directory name"))))



(defcustom undo-tree-visualizer-relative-timestamps t
  "When non-nil, display times relative to current time
when displaying time stamps in visualizer.

Otherwise, display absolute times."
  :group 'undo-tree
  :type 'boolean)


(defcustom undo-tree-visualizer-timestamps nil
  "When non-nil, display time-stamps by default
in undo-tree visualizer.

\\<undo-tree-visualizer-mode-map>You can always toggle time-stamps on and off \
using \\[undo-tree-visualizer-toggle-timestamps], regardless of the
setting of this variable."
  :group 'undo-tree
  :type 'boolean)


(defcustom undo-tree-visualizer-diff nil
  "When non-nil, display diff by default in undo-tree visualizer.

\\<undo-tree-visualizer-mode-map>You can always toggle the diff display \
using \\[undo-tree-visualizer-toggle-diff], regardless of the
setting of this variable."
  :group 'undo-tree
  :type 'boolean)


(defcustom undo-tree-visualizer-lazy-drawing 100
  "When non-nil, use lazy undo-tree drawing in visualizer.

Setting this to a number causes the visualizer to switch to lazy
drawing when the number of nodes in the tree is larger than this
value.

Lazy drawing means that only the visible portion of the tree will
be drawn initially, and the tree will be extended later as
needed. For the most part, the only visible effect of this is to
significantly speed up displaying the visualizer for very large
trees.

There is one potential negative effect of lazy drawing. Other
branches of the tree will only be drawn once the node from which
they branch off becomes visible. So it can happen that certain
portions of the tree that would be shown with lazy drawing
disabled, will not be drawn immediately when it is
enabled. However, this effect is quite rare in practice."
  :group 'undo-tree
  :type '(choice (const :tag "never" nil)
		 (const :tag "always" t)
		 (integer :tag "> size")))


(defvar undo-tree-pre-save-element-functions '()
  "Special hook to modify undo-tree elements prior to saving.
Each function on this hook is called in turn on each undo element
in the tree by `undo-tree-save-history' prior to writing the undo
history to file. It should return either nil, which removes that
undo element from the saved history, or a replacement element to
use instead (which should be identical to the original element if
that element should be saved unchanged).")


(defvar undo-tree-post-load-element-functions '()
  "Special hook to modify undo-tree undo elements after loading.
Each function on this hook is called in turn on each undo element
in the tree by `undo-tree-load-history' after loading the undo
history from file. It should return either nil, which removes that
undo element from the loaded history, or a replacement element to
use instead (which should be identical to the original element if
that element should be loaded unchanged).")


(defface undo-tree-visualizer-default-face
  '((((class color)) :foreground "gray"))
  "Face used to draw undo-tree in visualizer."
  :group 'undo-tree)

(defface undo-tree-visualizer-current-face
  '((((class color)) :foreground "red"))
  "Face used to highlight current undo-tree node in visualizer."
  :group 'undo-tree)

(defface undo-tree-visualizer-active-branch-face
  '((((class color) (background dark))
     (:foreground "white" :weight bold))
    (((class color) (background light))
     (:foreground "black" :weight bold)))
  "Face used to highlight active undo-tree branch in visualizer."
  :group 'undo-tree)

(defface undo-tree-visualizer-register-face
  '((((class color)) :foreground "yellow"))
  "Face used to highlight undo-tree nodes saved to a register
in visualizer."
  :group 'undo-tree)

(defface undo-tree-visualizer-unmodified-face
  '((((class color)) :foreground "cyan"))
  "Face used to highlight nodes corresponding to unmodified buffers
in visualizer."
  :group 'undo-tree)


(defvar undo-tree-visualizer-parent-buffer nil
  "Parent buffer in visualizer.")
(put 'undo-tree-visualizer-parent-buffer 'permanent-local t)
(make-variable-buffer-local 'undo-tree-visualizer-parent-buffer)

;; stores modification time of parent buffer's file, if any
(defvar undo-tree-visualizer-parent-mtime nil)
(put 'undo-tree-visualizer-parent-mtime 'permanent-local t)
(make-variable-buffer-local 'undo-tree-visualizer-parent-mtime)

;; stores current horizontal spacing needed for drawing undo-tree
(defvar undo-tree-visualizer-spacing nil)
(put 'undo-tree-visualizer-spacing 'permanent-local t)
(make-variable-buffer-local 'undo-tree-visualizer-spacing)

;; calculate horizontal spacing required for drawing tree with current
;; settings
(defsubst undo-tree-visualizer-calculate-spacing ()
  (if undo-tree-visualizer-timestamps
      (if undo-tree-visualizer-relative-timestamps 9 13)
    3))

;; holds node that was current when visualizer was invoked
(defvar undo-tree-visualizer-initial-node nil)
(put 'undo-tree-visualizer-initial-node 'permanent-local t)
(make-variable-buffer-local 'undo-tree-visualizer-initial-node)

;; holds currently selected node in visualizer selection mode
(defvar undo-tree-visualizer-selected-node nil)
(put 'undo-tree-visualizer-selected-node 'permanent-local t)
(make-variable-buffer-local 'undo-tree-visualizer-selected)

;; used to store nodes at edge of currently drawn portion of tree
(defvar undo-tree-visualizer-needs-extending-down nil)
(put 'undo-tree-visualizer-needs-extending-down 'permanent-local t)
(make-variable-buffer-local 'undo-tree-visualizer-needs-extending-down)
(defvar undo-tree-visualizer-needs-extending-up nil)
(put 'undo-tree-visualizer-needs-extending-up 'permanent-local t)
(make-variable-buffer-local 'undo-tree-visualizer-needs-extending-up)

;; dynamically bound to t when undoing from visualizer, to inhibit
;; `undo-tree-kill-visualizer' hook function in parent buffer
(defvar undo-tree-inhibit-kill-visualizer nil)

;; can be let-bound to a face name, used in drawing functions
(defvar undo-tree-insert-face nil)

;; visualizer buffer names
(defconst undo-tree-visualizer-buffer-name " *undo-tree*")
(defconst undo-tree-diff-buffer-name "*undo-tree Diff*")




;;; =================================================================
;;;                          Default keymaps

(defvar undo-tree-map nil
  "Keymap used in undo-tree-mode.")

(unless undo-tree-map
  (let ((map (make-sparse-keymap)))
    ;; remap `undo' and `undo-only' to `undo-tree-undo'
    (define-key map [remap undo] 'undo-tree-undo)
    (define-key map [remap undo-only] 'undo-tree-undo)
    ;; bind standard undo bindings (since these match redo counterparts)
    (define-key map (kbd "C-/") 'undo-tree-undo)
    (define-key map "\C-_" 'undo-tree-undo)
    ;; redo doesn't exist normally, so define our own keybindings
    (define-key map (kbd "C-?") 'undo-tree-redo)
    (define-key map (kbd "M-_") 'undo-tree-redo)
    ;; just in case something has defined `redo'...
    (define-key map [remap redo] 'undo-tree-redo)
    ;; we use "C-x u" for the undo-tree visualizer
    (define-key map (kbd "\C-x u") 'undo-tree-visualize)
    ;; bind register commands
    (define-key map (kbd "C-x r u") 'undo-tree-save-state-to-register)
    (define-key map (kbd "C-x r U") 'undo-tree-restore-state-from-register)
    ;; set keymap
    (setq undo-tree-map map)))


(defvar undo-tree-visualizer-mode-map nil
  "Keymap used in undo-tree visualizer.")

(unless undo-tree-visualizer-mode-map
  (let ((map (make-sparse-keymap)))
    ;; vertical motion keys undo/redo
    (define-key map [remap previous-line] 'undo-tree-visualize-undo)
    (define-key map [remap next-line] 'undo-tree-visualize-redo)
    (define-key map [up] 'undo-tree-visualize-undo)
    (define-key map "p" 'undo-tree-visualize-undo)
    (define-key map "\C-p" 'undo-tree-visualize-undo)
    (define-key map [down] 'undo-tree-visualize-redo)
    (define-key map "n" 'undo-tree-visualize-redo)
    (define-key map "\C-n" 'undo-tree-visualize-redo)
    ;; horizontal motion keys switch branch
    (define-key map [remap forward-char]
      'undo-tree-visualize-switch-branch-right)
    (define-key map [remap backward-char]
      'undo-tree-visualize-switch-branch-left)
    (define-key map [right] 'undo-tree-visualize-switch-branch-right)
    (define-key map "f" 'undo-tree-visualize-switch-branch-right)
    (define-key map "\C-f" 'undo-tree-visualize-switch-branch-right)
    (define-key map [left] 'undo-tree-visualize-switch-branch-left)
    (define-key map "b" 'undo-tree-visualize-switch-branch-left)
    (define-key map "\C-b" 'undo-tree-visualize-switch-branch-left)
    ;; paragraph motion keys undo/redo to significant points in tree
    (define-key map [remap backward-paragraph] 'undo-tree-visualize-undo-to-x)
    (define-key map [remap forward-paragraph] 'undo-tree-visualize-redo-to-x)
    (define-key map "\M-{" 'undo-tree-visualize-undo-to-x)
    (define-key map "\M-}" 'undo-tree-visualize-redo-to-x)
    (define-key map [C-up] 'undo-tree-visualize-undo-to-x)
    (define-key map [C-down] 'undo-tree-visualize-redo-to-x)
    ;; mouse sets buffer state to node at click
    (define-key map [mouse-1] 'undo-tree-visualizer-mouse-set)
    ;; toggle timestamps
    (define-key map "t" 'undo-tree-visualizer-toggle-timestamps)
    ;; toggle diff
    (define-key map "d" 'undo-tree-visualizer-toggle-diff)
    ;; toggle selection mode
    (define-key map "s" 'undo-tree-visualizer-selection-mode)
    ;; horizontal scrolling may be needed if the tree is very wide
    (define-key map "," 'undo-tree-visualizer-scroll-left)
    (define-key map "." 'undo-tree-visualizer-scroll-right)
    (define-key map "<" 'undo-tree-visualizer-scroll-left)
    (define-key map ">" 'undo-tree-visualizer-scroll-right)
    ;; vertical scrolling may be needed if the tree is very tall
    (define-key map [next] 'undo-tree-visualizer-scroll-up)
    (define-key map [prior] 'undo-tree-visualizer-scroll-down)
    ;; quit/abort visualizer
    (define-key map "q" 'undo-tree-visualizer-quit)
    (define-key map "\C-q" 'undo-tree-visualizer-abort)
    ;; set keymap
    (setq undo-tree-visualizer-mode-map map)))


(defvar undo-tree-visualizer-selection-mode-map nil
  "Keymap used in undo-tree visualizer selection mode.")

(unless undo-tree-visualizer-selection-mode-map
  (let ((map (make-sparse-keymap)))
    ;; vertical motion keys move up and down tree
    (define-key map [remap previous-line]
      'undo-tree-visualizer-select-previous)
    (define-key map [remap next-line]
      'undo-tree-visualizer-select-next)
    (define-key map [up] 'undo-tree-visualizer-select-previous)
    (define-key map "p" 'undo-tree-visualizer-select-previous)
    (define-key map "\C-p" 'undo-tree-visualizer-select-previous)
    (define-key map [down] 'undo-tree-visualizer-select-next)
    (define-key map "n" 'undo-tree-visualizer-select-next)
    (define-key map "\C-n" 'undo-tree-visualizer-select-next)
    ;; vertical scroll keys move up and down quickly
    (define-key map [next]
      (lambda () (interactive) (undo-tree-visualizer-select-next 10)))
    (define-key map [prior]
      (lambda () (interactive) (undo-tree-visualizer-select-previous 10)))
    ;; horizontal motion keys move to left and right siblings
    (define-key map [remap forward-char] 'undo-tree-visualizer-select-right)
    (define-key map [remap backward-char] 'undo-tree-visualizer-select-left)
    (define-key map [right] 'undo-tree-visualizer-select-right)
    (define-key map "f" 'undo-tree-visualizer-select-right)
    (define-key map "\C-f" 'undo-tree-visualizer-select-right)
    (define-key map [left] 'undo-tree-visualizer-select-left)
    (define-key map "b" 'undo-tree-visualizer-select-left)
    (define-key map "\C-b" 'undo-tree-visualizer-select-left)
    ;; horizontal scroll keys move left or right quickly
    (define-key map ","
      (lambda () (interactive) (undo-tree-visualizer-select-left 10)))
    (define-key map "."
      (lambda () (interactive) (undo-tree-visualizer-select-right 10)))
    (define-key map "<"
      (lambda () (interactive) (undo-tree-visualizer-select-left 10)))
    (define-key map ">"
      (lambda () (interactive) (undo-tree-visualizer-select-right 10)))
    ;; <enter> sets buffer state to node at point
    (define-key map "\r" 'undo-tree-visualizer-set)
    ;; mouse selects node at click
    (define-key map [mouse-1] 'undo-tree-visualizer-mouse-select)
    ;; toggle diff
    (define-key map "d" 'undo-tree-visualizer-selection-toggle-diff)
    ;; set keymap
    (setq undo-tree-visualizer-selection-mode-map map)))




;;; =====================================================================
;;;                     Undo-tree data structure

(cl-defstruct
  (undo-tree
   :named
   (:constructor nil)
   (:constructor make-undo-tree
                 (&aux
                  (root (undo-tree-make-node nil nil))
                  (current root)
                  (size 0)
		  (count 0)
		  (object-pool (make-hash-table :test 'eq :weakness 'value))))
   (:copier nil))
  root current size count object-pool)

(defun undo-tree-copy (tree)
  ;; Return a copy of undo-tree TREE.
  (unwind-protect
      (let ((new (make-undo-tree)))
	(undo-tree-decircle tree)
	(let ((max-lisp-eval-depth (* 100 (undo-tree-count tree)))
	      (max-specpdl-size (* 100 (undo-tree-count tree))))
	  (setf (undo-tree-root new)
		(undo-tree-node-copy (undo-tree-root tree)
				     new (undo-tree-current tree))))
	(setf (undo-tree-size new)
	      (undo-tree-size tree))
	(setf (undo-tree-count new)
	      (undo-tree-count tree))
	(setf (undo-tree-object-pool new)
	      (copy-hash-table (undo-tree-object-pool tree)))
	(undo-tree-recircle new)
	new)
    (undo-tree-recircle tree)))


(cl-defstruct
  (undo-tree-node
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor undo-tree-make-node
                 (previous undo
		  &optional redo
                  &aux
                  (timestamp (current-time))
                  (branch 0)))
   (:constructor undo-tree-make-node-backwards
                 (next-node undo
		  &optional redo
                  &aux
                  (next (list next-node))
                  (timestamp (current-time))
                  (branch 0)))
   (:constructor undo-tree-make-empty-node ())
   (:constructor undo-tree-copy-node-save-data
		 (node
		  &aux
		  (undo (let ((changeset (undo-tree-node-undo node)))
			  (run-hook-wrapped
			   'undo-tree-pre-save-element-functions
			   (lambda (fun)
			     (setq changeset (delq nil (mapcar fun changeset)))
			     nil))
			  changeset))
		  (redo (let ((changeset (undo-tree-node-redo node)))
			  (run-hook-wrapped
			   'undo-tree-pre-save-element-functions
			   (lambda (fun)
			     (setq changeset (delq nil (mapcar fun changeset)))
			     nil))
			  changeset))
		  (timestamp (undo-tree-node-timestamp node))
		  (branch (undo-tree-node-branch node))
		  (meta-data (undo-tree-node-meta-data node))))
   (:copier nil))
  previous next undo redo timestamp branch meta-data)


(defmacro undo-tree-node-p (n)
  (let ((len (length (undo-tree-make-node nil nil))))
    `(and (vectorp ,n) (= (length ,n) ,len))))

(defun undo-tree-node-copy (node &optional tree current)
  ;; Return a deep copy of undo-tree NODE, sans previous link or meta-data.
  ;; If TREE and CURRENT are supplied, set (undo-tree-current TREE) to the
  ;; copy of CURRENT node, if found.
  (let* ((new (undo-tree-make-empty-node))
	 (stack (list (cons node new)))
	 n)
    (while (setq n (pop stack))
      (setf (undo-tree-node-undo (cdr n))
	    (copy-tree (undo-tree-node-undo (car n)) 'copy-vectors))
      (setf (undo-tree-node-redo (cdr n))
	    (copy-tree (undo-tree-node-redo (car n)) 'copy-vectors))
      (setf (undo-tree-node-timestamp (cdr n))
	    (copy-sequence (undo-tree-node-timestamp (car n))))
      (setf (undo-tree-node-branch (cdr n))
	    (undo-tree-node-branch (car n)))
      (setf (undo-tree-node-next (cdr n))
	    (mapcar (lambda (_) (undo-tree-make-empty-node))
		    (make-list (length (undo-tree-node-next (car n))) nil)))
    ;; set (undo-tree-current TREE) to copy if we've found CURRENT
    (when (and tree (eq (car n) current))
      (setf (undo-tree-current tree) (cdr n)))
    ;; recursively copy next nodes
    (let ((next0 (undo-tree-node-next (car n)))
	  (next1 (undo-tree-node-next (cdr n))))
      (while (and next0 next1)
	(push (cons (pop next0) (pop next1)) stack))))
    new))


(cl-defstruct
  (undo-tree-region-data
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor undo-tree-make-region-data
		 (&optional undo-beginning undo-end
			     redo-beginning redo-end))
   (:constructor undo-tree-make-undo-region-data
		 (undo-beginning undo-end))
   (:constructor undo-tree-make-redo-region-data
		 (redo-beginning redo-end))
   (:copier nil))
  undo-beginning undo-end redo-beginning redo-end)


(defmacro undo-tree-region-data-p (r)
  (let ((len (length (undo-tree-make-region-data))))
    `(and (vectorp ,r) (= (length ,r) ,len))))

(defmacro undo-tree-node-clear-region-data (node)
  `(setf (undo-tree-node-meta-data ,node)
	 (delq nil
	       (delq :region
		     (plist-put (undo-tree-node-meta-data ,node)
				:region nil)))))


(defmacro undo-tree-node-undo-beginning (node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (when (undo-tree-region-data-p r)
       (undo-tree-region-data-undo-beginning r))))

(defmacro undo-tree-node-undo-end (node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (when (undo-tree-region-data-p r)
       (undo-tree-region-data-undo-end r))))

(defmacro undo-tree-node-redo-beginning (node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (when (undo-tree-region-data-p r)
       (undo-tree-region-data-redo-beginning r))))

(defmacro undo-tree-node-redo-end (node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (when (undo-tree-region-data-p r)
       (undo-tree-region-data-redo-end r))))


(gv-define-setter undo-tree-node-undo-beginning (val node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (unless (undo-tree-region-data-p r)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :region
			(setq r (undo-tree-make-region-data)))))
     (setf (undo-tree-region-data-undo-beginning r) ,val)))

(gv-define-setter undo-tree-node-undo-end (val node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (unless (undo-tree-region-data-p r)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :region
			(setq r (undo-tree-make-region-data)))))
     (setf (undo-tree-region-data-undo-end r) ,val)))

(gv-define-setter undo-tree-node-redo-beginning (val node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (unless (undo-tree-region-data-p r)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :region
			(setq r (undo-tree-make-region-data)))))
     (setf (undo-tree-region-data-redo-beginning r) ,val)))

(gv-define-setter undo-tree-node-redo-end (val node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (unless (undo-tree-region-data-p r)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :region
			(setq r (undo-tree-make-region-data)))))
     (setf (undo-tree-region-data-redo-end r) ,val)))



(cl-defstruct
  (undo-tree-visualizer-data
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor undo-tree-make-visualizer-data
		 (&optional lwidth cwidth rwidth marker))
   (:copier nil))
  lwidth cwidth rwidth marker)


(defmacro undo-tree-visualizer-data-p (v)
  (let ((len (length (undo-tree-make-visualizer-data))))
    `(and (vectorp ,v) (= (length ,v) ,len))))

(defun undo-tree-node-clear-visualizer-data (node)
  (let ((plist (undo-tree-node-meta-data node)))
    (if (eq (car plist) :visualizer)
	(setf (undo-tree-node-meta-data node) (nthcdr 2 plist))
      (while (and plist (not (eq (cadr plist) :visualizer)))
	(setq plist (cdr plist)))
      (if plist (setcdr plist (nthcdr 3 plist))))))

(defmacro undo-tree-node-lwidth (node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (when (undo-tree-visualizer-data-p v)
       (undo-tree-visualizer-data-lwidth v))))

(defmacro undo-tree-node-cwidth (node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (when (undo-tree-visualizer-data-p v)
       (undo-tree-visualizer-data-cwidth v))))

(defmacro undo-tree-node-rwidth (node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (when (undo-tree-visualizer-data-p v)
       (undo-tree-visualizer-data-rwidth v))))

(defmacro undo-tree-node-marker (node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (when (undo-tree-visualizer-data-p v)
       (undo-tree-visualizer-data-marker v))))


(gv-define-setter undo-tree-node-lwidth (val node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :visualizer
			(setq v (undo-tree-make-visualizer-data)))))
     (setf (undo-tree-visualizer-data-lwidth v) ,val)))

(gv-define-setter undo-tree-node-cwidth (val node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :visualizer
			(setq v (undo-tree-make-visualizer-data)))))
     (setf (undo-tree-visualizer-data-cwidth v) ,val)))

(gv-define-setter undo-tree-node-rwidth (val node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :visualizer
			(setq v (undo-tree-make-visualizer-data)))))
     (setf (undo-tree-visualizer-data-rwidth v) ,val)))

(gv-define-setter undo-tree-node-marker (val node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :visualizer
			(setq v (undo-tree-make-visualizer-data)))))
     (setf (undo-tree-visualizer-data-marker v) ,val)))



(cl-defstruct
  (undo-tree-register-data
   (:type vector)
   (:constructor nil)
   (:constructor undo-tree-make-register-data (buffer node)))
  buffer node)

(defun undo-tree-register-data-p (data)
  (and (vectorp data)
       (= (length data) 2)
       (undo-tree-node-p (undo-tree-register-data-node data))))

(defun undo-tree-register-data-print-func (data)
  (princ (format "an undo-tree state for buffer %s"
		 (undo-tree-register-data-buffer data))))

(defmacro undo-tree-node-register (node)
  `(plist-get (undo-tree-node-meta-data ,node) :register))

(gv-define-setter undo-tree-node-register (val node)
  `(setf (undo-tree-node-meta-data ,node)
	 (plist-put (undo-tree-node-meta-data ,node) :register ,val)))




;;; =====================================================================
;;;              Basic undo-tree data structure functions

(defun undo-tree-grow (undo)
  "Add an UNDO node to current branch of `buffer-undo-tree'."
  (let* ((current (undo-tree-current buffer-undo-tree))
         (new (undo-tree-make-node current undo)))
    (push new (undo-tree-node-next current))
    (setf (undo-tree-current buffer-undo-tree) new)))


(defun undo-tree-grow-backwards (node undo &optional redo)
  "Add new node *above* undo-tree NODE, and return new node.
Note that this will overwrite NODE's \"previous\" link, so should
only be used on a detached NODE, never on nodes that are already
part of `buffer-undo-tree'."
  (let ((new (undo-tree-make-node-backwards node undo redo)))
    (setf (undo-tree-node-previous node) new)
    new))


(defun undo-tree-splice-node (node splice)
  "Splice NODE into undo tree, below node SPLICE.
Note that this will overwrite NODE's \"next\" and \"previous\"
links, so should only be used on a detached NODE, never on nodes
that are already part of `buffer-undo-tree'."
  (setf (undo-tree-node-next node) (undo-tree-node-next splice)
	(undo-tree-node-branch node) (undo-tree-node-branch splice)
	(undo-tree-node-previous node) splice
	(undo-tree-node-next splice) (list node)
	(undo-tree-node-branch splice) 0)
  (dolist (n (undo-tree-node-next node))
    (setf (undo-tree-node-previous n) node)))


(defun undo-tree-snip-node (node)
  "Snip NODE out of undo tree."
  (let* ((parent (undo-tree-node-previous node))
	 position p)
    ;; if NODE is only child, replace parent's next links with NODE's
    (if (= (length (undo-tree-node-next parent)) 0)
	(setf (undo-tree-node-next parent) (undo-tree-node-next node)
	      (undo-tree-node-branch parent) (undo-tree-node-branch node))
      ;; otherwise...
      (setq position (undo-tree-position node (undo-tree-node-next parent)))
      (cond
       ;; if active branch used do go via NODE, set parent's branch to active
       ;; branch of NODE
       ((= (undo-tree-node-branch parent) position)
	(setf (undo-tree-node-branch parent)
	      (+ position (undo-tree-node-branch node))))
       ;; if active branch didn't go via NODE, update parent's branch to point
       ;; to same node as before
       ((> (undo-tree-node-branch parent) position)
	(cl-incf (undo-tree-node-branch parent)
		 (1- (length (undo-tree-node-next node))))))
      ;; replace NODE in parent's next list with NODE's entire next list
      (if (= position 0)
	  (setf (undo-tree-node-next parent)
		(nconc (undo-tree-node-next node)
		       (cdr (undo-tree-node-next parent))))
	(setq p (nthcdr (1- position) (undo-tree-node-next parent)))
	(setcdr p (nconc (undo-tree-node-next node) (cddr p)))))
    ;; update previous links of NODE's children
    (dolist (n (undo-tree-node-next node))
      (setf (undo-tree-node-previous n) parent))))


(defun undo-tree-mapc (--undo-tree-mapc-function-- node)
  ;; Apply FUNCTION to NODE and to each node below it.
  (let ((stack (list node))
	n)
    (while (setq n (pop stack))
      (funcall --undo-tree-mapc-function-- n)
      (setq stack (append (undo-tree-node-next n) stack)))))


(defmacro undo-tree-num-branches ()
  "Return number of branches at current undo tree node."
  '(length (undo-tree-node-next (undo-tree-current buffer-undo-tree))))


(defun undo-tree-position (node list)
  "Find the first occurrence of NODE in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with `eq'."
  (let ((i 0))
    (catch 'found
      (while (progn
               (when (eq node (car list)) (throw 'found i))
               (cl-incf i)
               (setq list (cdr list))))
      nil)))


(defvar *undo-tree-id-counter* 0)
(make-variable-buffer-local '*undo-tree-id-counter*)

(defmacro undo-tree-generate-id ()
  ;; Generate a new, unique id (uninterned symbol).
  ;; The name is made by appending a number to "undo-tree-id".
  ;; (Copied from CL package `gensym'.)
  `(let ((num (prog1 *undo-tree-id-counter*
		(cl-incf *undo-tree-id-counter*))))
     (make-symbol (format "undo-tree-id%d" num))))


(defun undo-tree-decircle (undo-tree)
  ;; Nullify PREVIOUS links of UNDO-TREE nodes, to make UNDO-TREE data
  ;; structure non-circular.
  (undo-tree-mapc
   (lambda (node)
     (dolist (n (undo-tree-node-next node))
       (setf (undo-tree-node-previous n) nil)))
   (undo-tree-root undo-tree)))


(defun undo-tree-recircle (undo-tree)
  ;; Recreate PREVIOUS links of UNDO-TREE nodes, to restore circular UNDO-TREE
  ;; data structure.
  (undo-tree-mapc
   (lambda (node)
     (dolist (n (undo-tree-node-next node))
       (setf (undo-tree-node-previous n) node)))
   (undo-tree-root undo-tree)))




;;; =====================================================================
;;;             Undo list and undo changeset utility functions

(defmacro undo-list-marker-elt-p (elt)
  `(markerp (car-safe ,elt)))

(defmacro undo-list-GCd-marker-elt-p (elt)
  ;; Return t if ELT is a marker element whose marker has been moved to the
  ;; object-pool, so may potentially have been garbage-collected.
  ;; Note: Valid marker undo elements should be uniquely identified as cons
  ;; cells with a symbol in the car (replacing the marker), and a number in
  ;; the cdr. However, to guard against future changes to undo element
  ;; formats, we perform an additional redundant check on the symbol name.
  `(and (car-safe ,elt)
	(symbolp (car ,elt))
	(let ((str (symbol-name (car ,elt))))
	  (and (> (length str) 12)
	       (string= (substring str 0 12) "undo-tree-id")))
	(numberp (cdr-safe ,elt))))


(defun undo-tree-move-GC-elts-to-pool (elt)
  ;; Move elements that can be garbage-collected into `buffer-undo-tree'
  ;; object pool, substituting a unique id that can be used to retrieve them
  ;; later. (Only markers require this treatment currently.)
  (when (undo-list-marker-elt-p elt)
    (let ((id (undo-tree-generate-id)))
      (puthash id (car elt) (undo-tree-object-pool buffer-undo-tree))
      (setcar elt id))))


(defun undo-tree-restore-GC-elts-from-pool (elt)
  ;; Replace object id's in ELT with corresponding objects from
  ;; `buffer-undo-tree' object pool and return modified ELT, or return nil if
  ;; any object in ELT has been garbage-collected.
  (if (undo-list-GCd-marker-elt-p elt)
      (when (setcar elt (gethash (car elt)
				 (undo-tree-object-pool buffer-undo-tree)))
	elt)
    elt))


(defun undo-list-clean-GCd-elts (undo-list)
  ;; Remove object id's from UNDO-LIST that refer to elements that have been
  ;; garbage-collected. UNDO-LIST is modified by side-effect.
  (while (undo-list-GCd-marker-elt-p (car undo-list))
    (unless (gethash (caar undo-list)
		     (undo-tree-object-pool buffer-undo-tree))
      (setq undo-list (cdr undo-list))))
  (let ((p undo-list))
    (while (cdr p)
      (when (and (undo-list-GCd-marker-elt-p (cadr p))
		 (null (gethash (car (cadr p))
				(undo-tree-object-pool buffer-undo-tree))))
	(setcdr p (cddr p)))
      (setq p (cdr p))))
  undo-list)


(defun undo-list-found-canary-p (undo-list)
  (or (eq (car undo-list) 'undo-tree-canary)
      (and (null (car undo-list))
	   (eq (cadr undo-list) 'undo-tree-canary))))


(defmacro undo-list-pop-changeset (undo-list &optional discard-pos)
  ;; Pop changeset from `undo-list'. If DISCARD-POS is non-nil, discard
  ;; any position entries from changeset.
  `(when (and ,undo-list (not (undo-list-found-canary-p ,undo-list)))
     (let (changeset)
       ;; discard initial undo boundary(ies)
       (while (null (car ,undo-list)) (setq ,undo-list (cdr ,undo-list)))
       ;; pop elements up to next undo boundary, discarding position entries
       ;; if DISCARD-POS is non-nil
       (while (null changeset)
	 (while (and ,undo-list (car ,undo-list)
		     (not (undo-list-found-canary-p ,undo-list)))
	   (if (and ,discard-pos (integerp (car ,undo-list)))
	       (setq ,undo-list (cdr ,undo-list))
	     (push (pop ,undo-list) changeset)
	     (undo-tree-move-GC-elts-to-pool (car changeset)))))
       (nreverse changeset))))


(defun undo-tree-copy-list (undo-list)
  ;; Return a deep copy of first changeset in `undo-list'. Object id's are
  ;; replaced by corresponding objects from `buffer-undo-tree' object-pool.
    (let (copy p)
      ;; if first element contains an object id, replace it with object from
      ;; pool, discarding element entirely if it's been GC'd
    (while (and undo-list (null copy))
	(setq copy
	      (undo-tree-restore-GC-elts-from-pool (pop undo-list))))
    (when copy
      (setq copy (list copy)
	    p copy)
      ;; copy remaining elements, replacing object id's with objects from
      ;; pool, or discarding them entirely if they've been GC'd
      (while undo-list
	(when (setcdr p (undo-tree-restore-GC-elts-from-pool
			 (undo-copy-list-1 (pop undo-list))))
	  (setcdr p (list (cdr p)))
	  (setq p (cdr p))))
      copy)))


(defvar undo-tree-gc-flag nil)

(defun undo-tree-post-gc ()
  (setq undo-tree-gc-flag t))


(defun undo-list-transfer-to-tree ()
  ;; Transfer entries accumulated in `undo-list' to `buffer-undo-tree'.

  ;; `undo-list-transfer-to-tree' should never be called when undo is disabled
  ;; (i.e. `buffer-undo-tree' is t)
  (cl-assert (not (eq buffer-undo-tree t)))

  ;; if `buffer-undo-tree' is empty, create initial undo-tree
  (when (null buffer-undo-tree) (setq buffer-undo-tree (make-undo-tree)))

  ;; garbage-collect then repeatedly try to deep-copy `buffer-undo-list' until
  ;; we succeed without GC running, in an attempt to mitigate race conditions
  ;; with garbage collector corrupting undo history (is this even a thing?!)
  (unless (or (null buffer-undo-list)
	      (undo-list-found-canary-p buffer-undo-list))
    (garbage-collect))
  (let (undo-list changeset)
    (setq undo-tree-gc-flag t)
    (while undo-tree-gc-flag
      (setq undo-tree-gc-flag nil
	    undo-list (copy-tree buffer-undo-list)))
    (setq buffer-undo-list (list nil 'undo-tree-canary))

    ;; create new node from first changeset in `undo-list', save old
    ;; `buffer-undo-tree' current node, and make new node the current node
    (when (setq changeset (undo-list-pop-changeset undo-list))
      (let* ((node (undo-tree-make-node nil changeset))
	     (splice (undo-tree-current buffer-undo-tree))
	     (size (undo-list-byte-size (undo-tree-node-undo node)))
	     (count 1))
	(setf (undo-tree-current buffer-undo-tree) node)
	;; grow tree fragment backwards using `undo-list' changesets
	(while (setq changeset (undo-list-pop-changeset undo-list))
	  (setq node (undo-tree-grow-backwards node changeset))
	  (cl-incf size (undo-list-byte-size (undo-tree-node-undo node)))
	  (cl-incf count))

	;; if no undo history has been discarded from `undo-list' since last
	;; transfer, splice new tree fragment onto end of old
	;; `buffer-undo-tree' current node
	(if (undo-list-found-canary-p undo-list)
	    (progn
	      (setf (undo-tree-node-previous node) splice)
	      (push node (undo-tree-node-next splice))
	      (setf (undo-tree-node-branch splice) 0)
	      (cl-incf (undo-tree-size buffer-undo-tree) size)
	      (cl-incf (undo-tree-count buffer-undo-tree) count))

	  ;; if undo history has been discarded, replace entire
	  ;; `buffer-undo-tree' with new tree fragment
	  (unless (= (undo-tree-size buffer-undo-tree) 0)
	    (message "Undo history discarded by Emacs (see `undo-limit') - rebuilding undo-tree"))
	  (setq node (undo-tree-grow-backwards node nil))
	  (setf (undo-tree-root buffer-undo-tree) node)
	  (setf (undo-tree-size buffer-undo-tree) size)
	  (setf (undo-tree-count buffer-undo-tree) count)))))

  ;; discard undo history if necessary
  (undo-tree-discard-history))


(defun undo-list-byte-size (undo-list)
  ;; Return size (in bytes) of UNDO-LIST
  (let ((size 0))
    (dolist (elt undo-list)
      (cl-incf size 8)  ; cons cells use up 8 bytes
      (when (stringp (car-safe elt))
        (cl-incf size (string-bytes (car elt)))))
    size))



(defun undo-list-rebuild-from-tree ()
  "Rebuild `buffer-undo-list' from information in `buffer-undo-tree'."
  (unless (eq buffer-undo-list t)
    (undo-list-transfer-to-tree)
    (setq buffer-undo-list nil)
    (when buffer-undo-tree
      (let ((stack (list (list (undo-tree-root buffer-undo-tree)))))
	(push (sort (mapcar 'identity (undo-tree-node-next (caar stack)))
		    (lambda (a b)
		      (time-less-p (undo-tree-node-timestamp a)
				   (undo-tree-node-timestamp b))))
	      stack)
	;; Traverse tree in depth-and-oldest-first order, but add undo records
	;; on the way down, and redo records on the way up.
	(while (or (car stack)
		   (not (eq (car (nth 1 stack))
			    (undo-tree-current buffer-undo-tree))))
	  (if (car stack)
	      (progn
		(setq buffer-undo-list
		      (append (undo-tree-node-undo (caar stack))
			      buffer-undo-list))
		(undo-boundary)
		(push (sort (mapcar 'identity
				    (undo-tree-node-next (caar stack)))
			    (lambda (a b)
			      (time-less-p (undo-tree-node-timestamp a)
					   (undo-tree-node-timestamp b))))
		      stack))
	    (pop stack)
	    (setq buffer-undo-list
		  (append (undo-tree-node-redo (caar stack))
			  buffer-undo-list))
	    (undo-boundary)
	    (pop (car stack))))))))




;;; =====================================================================
;;;                History discarding utility functions

(defun undo-tree-oldest-leaf (node)
  ;; Return oldest leaf node below NODE.
  (while (undo-tree-node-next node)
    (setq node
          (car (sort (mapcar 'identity (undo-tree-node-next node))
                     (lambda (a b)
                       (time-less-p (undo-tree-node-timestamp a)
                                    (undo-tree-node-timestamp b)))))))
  node)


(defun undo-tree-discard-node (node)
  ;; Discard NODE from `buffer-undo-tree', and return next in line for
  ;; discarding.

  ;; don't discard current node
  (unless (eq node (undo-tree-current buffer-undo-tree))

    ;; discarding root node...
    (if (eq node (undo-tree-root buffer-undo-tree))
        (cond
         ;; should always discard branches before root
         ((> (length (undo-tree-node-next node)) 1)
          (error "Trying to discard undo-tree root which still\
 has multiple branches"))
         ;; don't discard root if current node is only child
         ((eq (car (undo-tree-node-next node))
              (undo-tree-current buffer-undo-tree))
	  nil)
	 ;; discard root
         (t
	  ;; clear any register referring to root
	  (let ((r (undo-tree-node-register node)))
	    (when (and r (eq (get-register r) node))
	      (set-register r nil)))
          ;; make child of root into new root
          (setq node (setf (undo-tree-root buffer-undo-tree)
                           (car (undo-tree-node-next node))))
	  ;; update undo-tree size
	  (cl-decf (undo-tree-size buffer-undo-tree)
		   (+ (undo-list-byte-size (undo-tree-node-undo node))
		      (undo-list-byte-size (undo-tree-node-redo node))))
	  (cl-decf (undo-tree-count buffer-undo-tree))
	  ;; discard new root's undo data and PREVIOUS link
	  (setf (undo-tree-node-undo node) nil
		(undo-tree-node-redo node) nil
		(undo-tree-node-previous node) nil)
          ;; if new root has branches, or new root is current node, next node
          ;; to discard is oldest leaf, otherwise it's new root
          (if (or (> (length (undo-tree-node-next node)) 1)
                  (eq (car (undo-tree-node-next node))
                      (undo-tree-current buffer-undo-tree)))
              (undo-tree-oldest-leaf node)
            node)))

      ;; discarding leaf node...
      (let* ((parent (undo-tree-node-previous node))
             (current (nth (undo-tree-node-branch parent)
                           (undo-tree-node-next parent))))
	;; clear any register referring to the discarded node
	(let ((r (undo-tree-node-register node)))
	  (when (and r (eq (get-register r) node))
	    (set-register r nil)))
	;; update undo-tree size
	(cl-decf (undo-tree-size buffer-undo-tree)
		 (+ (undo-list-byte-size (undo-tree-node-undo node))
		    (undo-list-byte-size (undo-tree-node-redo node))))
	(cl-decf (undo-tree-count buffer-undo-tree))
	;; discard leaf
        (setf (undo-tree-node-next parent)
                (delq node (undo-tree-node-next parent))
              (undo-tree-node-branch parent)
                (undo-tree-position current (undo-tree-node-next parent)))
        ;; if parent has branches, or parent is current node, next node to
        ;; discard is oldest leaf, otherwise it's the parent itself
        (if (or (eq parent (undo-tree-current buffer-undo-tree))
                (and (undo-tree-node-next parent)
                     (or (not (eq parent (undo-tree-root buffer-undo-tree)))
                         (> (length (undo-tree-node-next parent)) 1))))
            (undo-tree-oldest-leaf parent)
          parent)))))



(defun undo-tree-discard-history ()
  "Discard undo history until we're within memory usage limits
set by `undo-limit', `undo-strong-limit' and `undo-outer-limit'."

  (when (> (undo-tree-size buffer-undo-tree) undo-limit)
    ;; if there are no branches off root, first node to discard is root;
    ;; otherwise it's leaf node at botom of oldest branch
    (let ((node (if (> (length (undo-tree-node-next
                                (undo-tree-root buffer-undo-tree))) 1)
                    (undo-tree-oldest-leaf (undo-tree-root buffer-undo-tree))
                  (undo-tree-root buffer-undo-tree)))
	  discarded)

      ;; discard nodes until memory use is within `undo-strong-limit'
      (while (and node
                  (> (undo-tree-size buffer-undo-tree) undo-strong-limit))
        (setq node (undo-tree-discard-node node)
	      discarded t))

      ;; discard nodes until next node to discard would bring memory use
      ;; within `undo-limit'
      (while (and node
		  ;; check first if last discard has brought us within
		  ;; `undo-limit', in case we can avoid more expensive
		  ;; `undo-strong-limit' calculation
		  ;; Note: this assumes undo-strong-limit > undo-limit;
		  ;;       if not, effectively undo-strong-limit = undo-limit
		  (> (undo-tree-size buffer-undo-tree) undo-limit)
                  (> (- (undo-tree-size buffer-undo-tree)
			;; if next node to discard is root, the memory we
			;; free-up comes from discarding changesets from its
			;; only child...
			(if (eq node (undo-tree-root buffer-undo-tree))
			    (+ (undo-list-byte-size
				(undo-tree-node-undo
				 (car (undo-tree-node-next node))))
			       (undo-list-byte-size
				(undo-tree-node-redo
				 (car (undo-tree-node-next node)))))
			  ;; ...otherwise, it comes from discarding changesets
			  ;; from along with the node itself
			  (+ (undo-list-byte-size (undo-tree-node-undo node))
			     (undo-list-byte-size (undo-tree-node-redo node)))
			  ))
                     undo-limit))
        (setq node (undo-tree-discard-node node)
	      discarded t))

      (when discarded
	(message "Undo history discarded by undo-tree (see `undo-tree-limit')"))

      ;; if we're still over the `undo-outer-limit', discard entire history
      (when (and undo-outer-limit
		 (> (undo-tree-size buffer-undo-tree) undo-outer-limit))
        ;; query first if `undo-ask-before-discard' is set
        (if undo-ask-before-discard
            (when (yes-or-no-p
                   (format
                    "Buffer `%s' undo info is %d bytes long;  discard it? "
                    (buffer-name) (undo-tree-size buffer-undo-tree)))
              (setq buffer-undo-tree nil))
          ;; otherwise, discard and display warning
          (display-warning
           '(undo discard-info)
           (concat
            (format "Buffer `%s' undo info was %d bytes long.\n"
                    (buffer-name) (undo-tree-size buffer-undo-tree))
            "The undo info was discarded because it exceeded\
 `undo-outer-limit'.

This is normal if you executed a command that made a huge change
to the buffer. In that case, to prevent similar problems in the
future, set `undo-outer-limit' to a value that is large enough to
cover the maximum size of normal changes you expect a single
command to make, but not so large that it might exceed the
maximum memory allotted to Emacs.

If you did not execute any such command, the situation is
probably due to a bug and you should report it.

You can disable the popping up of this buffer by adding the entry
\(undo discard-info) to the user option `warning-suppress-types',
which is defined in the `warnings' library.\n")
           :warning)
          (setq buffer-undo-tree nil)))

      ;; if currently displaying the visualizer, redraw it
      (when (and buffer-undo-tree
		 discarded
		 (or (eq major-mode 'undo-tree-visualizer-mode)
		     undo-tree-visualizer-parent-buffer
		     (get-buffer undo-tree-visualizer-buffer-name)))
	(let ((undo-tree buffer-undo-tree))
	  (with-current-buffer undo-tree-visualizer-buffer-name
	    (undo-tree-draw-tree undo-tree)
	    (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff)))))
      )))




;;; =====================================================================
;;;                   Visualizer utility functions

(defun undo-tree-compute-widths (node)
  "Recursively compute widths for nodes below NODE."
  (let ((stack (list node))
        res)
    (while stack
      ;; try to compute widths for node at top of stack
      (if (undo-tree-node-p
           (setq res (undo-tree-node-compute-widths (car stack))))
          ;; if computation fails, it returns a node whose widths still need
          ;; computing, which we push onto the stack
          (push res stack)
        ;; otherwise, store widths and remove it from stack
        (setf (undo-tree-node-lwidth (car stack)) (aref res 0)
              (undo-tree-node-cwidth (car stack)) (aref res 1)
              (undo-tree-node-rwidth (car stack)) (aref res 2))
        (pop stack)))))


(defun undo-tree-node-compute-widths (node)
  ;; Compute NODE's left-, centre-, and right-subtree widths. Returns widths
  ;; (in a vector) if successful. Otherwise, returns a node whose widths need
  ;; calculating before NODE's can be calculated.
  (let ((num-children (length (undo-tree-node-next node)))
        (lwidth 0) (cwidth 0) (rwidth 0) p)
    (catch 'need-widths
      (cond
       ;; leaf nodes have 0 width
       ((= 0 num-children)
        (setf cwidth 1
              (undo-tree-node-lwidth node) 0
              (undo-tree-node-cwidth node) 1
              (undo-tree-node-rwidth node) 0))

       ;; odd number of children
       ((= (mod num-children 2) 1)
        (setq p (undo-tree-node-next node))
        ;; compute left-width
        (dotimes (_ (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (cl-incf lwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            ;; if child's widths haven't been computed, return that child
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        (if (undo-tree-node-lwidth (car p))
            (cl-incf lwidth (undo-tree-node-lwidth (car p)))
          (throw 'need-widths (car p)))
        ;; centre-width is inherited from middle child
        (setf cwidth (undo-tree-node-cwidth (car p)))
        ;; compute right-width
        (cl-incf rwidth (undo-tree-node-rwidth (car p)))
        (setq p (cdr p))
        (dotimes (_ (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (cl-incf rwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p))))

       ;; even number of children
       (t
        (setq p (undo-tree-node-next node))
        ;; compute left-width
        (dotimes (_ (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (cl-incf lwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        ;; centre-width is 0 when number of children is even
        (setq cwidth 0)
        ;; compute right-width
        (dotimes (_ (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (cl-incf rwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))))

      ;; return left-, centre- and right-widths
      (vector lwidth cwidth rwidth))))


(defun undo-tree-clear-visualizer-data (tree)
  ;; Clear visualizer data below NODE.
  (undo-tree-mapc
   (lambda (n) (undo-tree-node-clear-visualizer-data n))
   (undo-tree-root tree)))


(defun undo-tree-node-unmodified-p (node &optional mtime)
  ;; Return non-nil if NODE corresponds to a buffer state that once upon a
  ;; time was unmodified. If a file modification time MTIME is specified,
  ;; return non-nil if the corresponding buffer state really is unmodified.
  (let* ((changeset
	  (or (undo-tree-node-redo node)
	      (and (setq changeset (car (undo-tree-node-next node)))
		   (undo-tree-node-undo changeset))))
	 (ntime
	  (let ((elt (car (last changeset))))
	    (and (consp elt) (eq (car elt) t) (consp (cdr elt))
		 (cdr elt)))))
    (and ntime
	 (or (null mtime)
	     ;; high-precision timestamps
	     (if (listp (cdr ntime))
		 (equal ntime mtime)
	       ;; old-style timestamps
	       (and (= (car ntime) (car mtime))
		    (= (cdr ntime) (cadr mtime))))))))




;;; =====================================================================
;;;                  Undo-in-region utility functions

;; `undo-elt-in-region' uses this as a dynamically-scoped variable
(defvar undo-adjusted-markers nil)


(defun undo-tree-pull-undo-in-region-branch (start end)
  ;; Pull out entries from undo changesets to create a new undo-in-region
  ;; branch, which undoes changeset entries lying between START and END first,
  ;; followed by remaining entries from the changesets, before rejoining the
  ;; existing undo tree history. Repeated calls will, if appropriate, extend
  ;; the current undo-in-region branch rather than creating a new one.

  ;; if we're just reverting the last redo-in-region, we don't need to
  ;; manipulate the undo tree at all
  (if (undo-tree-reverting-redo-in-region-p start end)
      t  ; return t to indicate success

    ;; We build the `region-changeset' and `delta-list' lists forwards, using
    ;; pointers `r' and `d' to the penultimate element of the list. So that we
    ;; don't have to treat the first element differently, we prepend a dummy
    ;; leading nil to the lists, and have the pointers point to that
    ;; initially.
    ;; Note: using '(nil) instead of (list nil) in the `let*' results in
    ;;       errors when the code is byte-compiled, presumably because the
    ;;       Lisp reader generates a single cons, and that same cons gets used
    ;;       each call.
    (let* ((region-changeset (list nil))
	   (r region-changeset)
	   (delta-list (list nil))
	   (d delta-list)
	   (node (undo-tree-current buffer-undo-tree))
	   (repeated-undo-in-region
	    (undo-tree-repeated-undo-in-region-p start end))
	   undo-adjusted-markers  ; `undo-elt-in-region' expects this
	   fragment splice original-fragment original-splice original-current
	   got-visible-elt undo-list elt)

      ;; --- initialisation ---
      (cond
       ;; if this is a repeated undo in the same region, start pulling changes
       ;; from NODE at which undo-in-region branch is attached, and detatch
       ;; the branch, using it as initial FRAGMENT of branch being constructed
       (repeated-undo-in-region
	(setq original-current node
	      fragment (car (undo-tree-node-next node))
	      splice node)
	;; undo up to node at which undo-in-region branch is attached
	;; (recognizable as first node with more than one branch)
	(let ((mark-active nil))
	  (while (= (length (undo-tree-node-next node)) 1)
	    (undo-tree-undo-1)
	    (setq fragment node
		  node (undo-tree-current buffer-undo-tree))))
	(when (eq splice node) (setq splice nil))
	;; detatch undo-in-region branch
	(setf (undo-tree-node-next node)
	      (delq fragment (undo-tree-node-next node))
	      (undo-tree-node-previous fragment) nil
	      original-fragment fragment
	      original-splice node))

       ;; if this is a new undo-in-region, initial FRAGMENT is a copy of all
       ;; nodes below the current one in the active branch
       ((undo-tree-node-next node)
	(setq fragment (undo-tree-make-node nil nil)
	      splice fragment)
	(while (setq node (nth (undo-tree-node-branch node)
			       (undo-tree-node-next node)))
	  (push (undo-tree-make-node
		 splice
		 (undo-copy-list (undo-tree-node-undo node))
		 (undo-copy-list (undo-tree-node-redo node)))
		(undo-tree-node-next splice))
	  (setq splice (car (undo-tree-node-next splice))))
	(setq fragment (car (undo-tree-node-next fragment))
	      splice nil
	      node (undo-tree-current buffer-undo-tree))))


      ;; --- pull undo-in-region elements into branch ---
      ;; work backwards up tree, pulling out undo elements within region until
      ;; we've got one that undoes a visible change (insertion or deletion)
      (catch 'abort
	(while (and (not got-visible-elt) node (undo-tree-node-undo node))
	  ;; we cons a dummy nil element on the front of the changeset so that
	  ;; we can conveniently remove the first (real) element from the
	  ;; changeset if we need to; the leading nil is removed once we're
	  ;; done with this changeset
	  (setq undo-list (cons nil (undo-copy-list (undo-tree-node-undo node)))
		elt (cadr undo-list))
	  (if fragment
	      (progn
		(setq fragment (undo-tree-grow-backwards fragment undo-list))
		(unless splice (setq splice fragment)))
	    (setq fragment (undo-tree-make-node nil undo-list))
	    (setq splice fragment))

	  (while elt
	    (cond
	     ;; keep elements within region
	     ((undo-elt-in-region elt start end)
	      ;; set flag if kept element is visible (insertion or deletion)
	      (when (and (consp elt)
			 (or (stringp (car elt)) (integerp (car elt))))
		(setq got-visible-elt t))
	      ;; adjust buffer positions in elements previously undone before
	      ;; kept element, as kept element will now be undone first
	      (undo-tree-adjust-elements-to-elt splice elt)
	      ;; move kept element to undo-in-region changeset, adjusting its
	      ;; buffer position as it will now be undone first
	      (setcdr r (list (undo-tree-apply-deltas elt (cdr delta-list))))
	      (setq r (cdr r))
	      (setcdr undo-list (cddr undo-list)))

	     ;; discard "was unmodified" elements
	     ;; FIXME: deal properly with these
	     ((and (consp elt) (eq (car elt) t))
	      (setcdr undo-list (cddr undo-list)))

	     ;; if element crosses region, we can't pull any more elements
	     ((undo-elt-crosses-region elt start end)
	      ;; if we've found a visible element, it must be earlier in
	      ;; current node's changeset; stop pulling elements (null
	      ;; `undo-list' and non-nil `got-visible-elt' cause loop to exit)
	      (if got-visible-elt
		  (setq undo-list nil)
		;; if we haven't found a visible element yet, pulling
		;; undo-in-region branch has failed
		(setq region-changeset nil)
		(throw 'abort t)))

	     ;; if rejecting element, add its delta (if any) to the list
	     (t
	      (let ((delta (undo-delta elt)))
		(when (/= 0 (cdr delta))
		  (setcdr d (list delta))
		  (setq d (cdr d))))
	      (setq undo-list (cdr undo-list))))

	    ;; process next element of current changeset
	    (setq elt (cadr undo-list)))

	  ;; if there are remaining elements in changeset, remove dummy nil
	  ;; from front
	  (if (cadr (undo-tree-node-undo fragment))
	      (pop (undo-tree-node-undo fragment))
	    ;; otherwise, if we've kept all elements in changeset, discard
	    ;; empty changeset
	    (when (eq splice fragment) (setq splice nil))
	    (setq fragment (car (undo-tree-node-next fragment))))
	  ;; process changeset from next node up the tree
	  (setq node (undo-tree-node-previous node))))

      ;; pop dummy nil from front of `region-changeset'
      (setq region-changeset (cdr region-changeset))


      ;; --- integrate branch into tree ---
      ;; if no undo-in-region elements were found, restore undo tree
      (if (null region-changeset)
	  (when original-current
	    (push original-fragment (undo-tree-node-next original-splice))
	    (setf (undo-tree-node-branch original-splice) 0
		  (undo-tree-node-previous original-fragment) original-splice)
	    (let ((mark-active nil))
	      (while (not (eq (undo-tree-current buffer-undo-tree)
			      original-current))
		(undo-tree-redo-1)))
	    nil)  ; return nil to indicate failure

	;; otherwise...
	;; need to undo up to node where new branch will be attached, to
	;; ensure redo entries are populated, and then redo back to where we
	;; started
	(let ((mark-active nil)
	      (current (undo-tree-current buffer-undo-tree)))
	  (while (not (eq (undo-tree-current buffer-undo-tree) node))
	    (undo-tree-undo-1))
	  (while (not (eq (undo-tree-current buffer-undo-tree) current))
	    (undo-tree-redo-1)))

	(cond
	 ;; if there's no remaining fragment, just create undo-in-region node
	 ;; and attach it to parent of last node from which elements were
	 ;; pulled
	 ((null fragment)
	  (setq fragment (undo-tree-make-node node region-changeset))
	  (push fragment (undo-tree-node-next node))
	  (setf (undo-tree-node-branch node) 0)
	  ;; set current node to undo-in-region node
	  (setf (undo-tree-current buffer-undo-tree) fragment))

	 ;; if no splice point has been set, add undo-in-region node to top of
	 ;; fragment and attach it to parent of last node from which elements
	 ;; were pulled
	 ((null splice)
	  (setq fragment (undo-tree-grow-backwards fragment region-changeset))
	  (push fragment (undo-tree-node-next node))
	  (setf (undo-tree-node-branch node) 0
		(undo-tree-node-previous fragment) node)
	  ;; set current node to undo-in-region node
	  (setf (undo-tree-current buffer-undo-tree) fragment))

	 ;; if fragment contains nodes, attach fragment to parent of last node
	 ;; from which elements were pulled, and splice in undo-in-region node
	 (t
	  (setf (undo-tree-node-previous fragment) node)
	  (push fragment (undo-tree-node-next node))
	  (setf (undo-tree-node-branch node) 0)
	  ;; if this is a repeated undo-in-region, then we've left the current
	  ;; node at the original splice-point; we need to set the current
	  ;; node to the equivalent node on the undo-in-region branch and redo
	  ;; back to where we started
	  (when repeated-undo-in-region
	    (setf (undo-tree-current buffer-undo-tree)
		  (undo-tree-node-previous original-fragment))
	    (let ((mark-active nil))
	      (while (not (eq (undo-tree-current buffer-undo-tree) splice))
		(undo-tree-redo-1 nil 'preserve-undo))))
	  ;; splice new undo-in-region node into fragment
	  (setq node (undo-tree-make-node nil region-changeset))
	  (undo-tree-splice-node node splice)
	  ;; set current node to undo-in-region node
	  (setf (undo-tree-current buffer-undo-tree) node)))

	;; update undo-tree size
	(setq node (undo-tree-node-previous fragment))
	(while (progn
		 (and (setq node (car (undo-tree-node-next node)))
		      (not (eq node original-fragment))
		      (cl-incf (undo-tree-count buffer-undo-tree))
		      (cl-incf (undo-tree-size buffer-undo-tree)
			       (+ (undo-list-byte-size (undo-tree-node-undo node))
				  (undo-list-byte-size (undo-tree-node-redo node)))))))
	t)  ; indicate undo-in-region branch was successfully pulled
      )))



(defun undo-tree-pull-redo-in-region-branch (start end)
  ;; Pull out entries from redo changesets to create a new redo-in-region
  ;; branch, which redoes changeset entries lying between START and END first,
  ;; followed by remaining entries from the changesets. Repeated calls will,
  ;; if appropriate, extend the current redo-in-region branch rather than
  ;; creating a new one.

  ;; if we're just reverting the last undo-in-region, we don't need to
  ;; manipulate the undo tree at all
  (if (undo-tree-reverting-undo-in-region-p start end)
      t  ; return t to indicate success

    ;; We build the `region-changeset' and `delta-list' lists forwards, using
    ;; pointers `r' and `d' to the penultimate element of the list. So that we
    ;; don't have to treat the first element differently, we prepend a dummy
    ;; leading nil to the lists, and have the pointers point to that
    ;; initially.
    ;; Note: using '(nil) instead of (list nil) in the `let*' causes bizarre
    ;;       errors when the code is byte-compiled, where parts of the lists
    ;;       appear to survive across different calls to this function.  An
    ;;       obscure byte-compiler bug, perhaps?
    (let* ((region-changeset (list nil))
	   (r region-changeset)
	   (delta-list (list nil))
	   (d delta-list)
	   (node (undo-tree-current buffer-undo-tree))
	   (repeated-redo-in-region
	    (undo-tree-repeated-redo-in-region-p start end))
	   undo-adjusted-markers  ; `undo-elt-in-region' expects this
	   fragment splice got-visible-elt redo-list elt)

      ;; --- inisitalisation ---
      (cond
       ;; if this is a repeated redo-in-region, detach fragment below current
       ;; node
       (repeated-redo-in-region
	(when (setq fragment (car (undo-tree-node-next node)))
	  (setf (undo-tree-node-previous fragment) nil
		(undo-tree-node-next node)
		(delq fragment (undo-tree-node-next node)))))
       ;; if this is a new redo-in-region, initial fragment is a copy of all
       ;; nodes below the current one in the active branch
       ((undo-tree-node-next node)
	(setq fragment (undo-tree-make-node nil nil)
	      splice fragment)
	(while (setq node (nth (undo-tree-node-branch node)
			       (undo-tree-node-next node)))
	  (push (undo-tree-make-node
		 splice nil
		 (undo-copy-list (undo-tree-node-redo node)))
		(undo-tree-node-next splice))
	  (setq splice (car (undo-tree-node-next splice))))
	(setq fragment (car (undo-tree-node-next fragment)))))


      ;; --- pull redo-in-region elements into branch ---
      ;; work down fragment, pulling out redo elements within region until
      ;; we've got one that redoes a visible change (insertion or deletion)
      (setq node fragment)
      (catch 'abort
	(while (and (not got-visible-elt) node (undo-tree-node-redo node))
	  ;; we cons a dummy nil element on the front of the changeset so that
	  ;; we can conveniently remove the first (real) element from the
	  ;; changeset if we need to; the leading nil is removed once we're
	  ;; done with this changeset
	  (setq redo-list (push nil (undo-tree-node-redo node))
		elt (cadr redo-list))
	  (while elt
	    (cond
	     ;; keep elements within region
	     ((undo-elt-in-region elt start end)
	      ;; set flag if kept element is visible (insertion or deletion)
	      (when (and (consp elt)
			 (or (stringp (car elt)) (integerp (car elt))))
		(setq got-visible-elt t))
	      ;; adjust buffer positions in elements previously redone before
	      ;; kept element, as kept element will now be redone first
	      (undo-tree-adjust-elements-to-elt fragment elt t)
	      ;; move kept element to redo-in-region changeset, adjusting its
	      ;; buffer position as it will now be redone first
	      (setcdr r (list (undo-tree-apply-deltas elt (cdr delta-list) -1)))
	      (setq r (cdr r))
	      (setcdr redo-list (cddr redo-list)))

	     ;; discard "was unmodified" elements
	     ;; FIXME: deal properly with these
	     ((and (consp elt) (eq (car elt) t))
	      (setcdr redo-list (cddr redo-list)))

	     ;; if element crosses region, we can't pull any more elements
	     ((undo-elt-crosses-region elt start end)
	      ;; if we've found a visible element, it must be earlier in
	      ;; current node's changeset; stop pulling elements (null
	      ;; `redo-list' and non-nil `got-visible-elt' cause loop to exit)
	      (if got-visible-elt
		  (setq redo-list nil)
		;; if we haven't found a visible element yet, pulling
		;; redo-in-region branch has failed
		(setq region-changeset nil)
		(throw 'abort t)))

	     ;; if rejecting element, add its delta (if any) to the list
	     (t
	      (let ((delta (undo-delta elt)))
		(when (/= 0 (cdr delta))
		  (setcdr d (list delta))
		  (setq d (cdr d))))
	      (setq redo-list (cdr redo-list))))

	    ;; process next element of current changeset
	    (setq elt (cadr redo-list)))

	  ;; if there are remaining elements in changeset, remove dummy nil
	  ;; from front
	  (if (cadr (undo-tree-node-redo node))
	      (pop (undo-tree-node-undo node))
	    ;; otherwise, if we've kept all elements in changeset, discard
	    ;; empty changeset
	    (if (eq fragment node)
		(setq fragment (car (undo-tree-node-next fragment)))
	      (undo-tree-snip-node node)))
	  ;; process changeset from next node in fragment
	  (setq node (car (undo-tree-node-next node)))))

      ;; pop dummy nil from front of `region-changeset'
      (setq region-changeset (cdr region-changeset))


      ;; --- integrate branch into tree ---
      (setq node (undo-tree-current buffer-undo-tree))
      ;; if no redo-in-region elements were found, restore undo tree
      (if (null (car region-changeset))
	  (when (and repeated-redo-in-region fragment)
	    (push fragment (undo-tree-node-next node))
	    (setf (undo-tree-node-branch node) 0
		  (undo-tree-node-previous fragment) node)
	    nil)  ; return nil to indicate failure

	;; otherwise, add redo-in-region node to top of fragment, and attach
	;; it below current node
	(setq fragment
	      (if fragment
		  (undo-tree-grow-backwards fragment nil region-changeset)
		(undo-tree-make-node nil nil region-changeset)))
	(push fragment (undo-tree-node-next node))
	(setf (undo-tree-node-branch node) 0
	      (undo-tree-node-previous fragment) node)
	;; update undo-tree size
	(unless repeated-redo-in-region
	  (setq node fragment)
	  (while (and (setq node (car (undo-tree-node-next node)))
		      (cl-incf (undo-tree-count buffer-undo-tree))
		      (cl-incf (undo-tree-size buffer-undo-tree)
			       (undo-list-byte-size
				(undo-tree-node-redo node))))))
	(cl-incf (undo-tree-size buffer-undo-tree)
		 (undo-list-byte-size (undo-tree-node-redo fragment)))
	t)  ; indicate redo-in-region branch was successfully pulled
      )))



(defun undo-tree-adjust-elements-to-elt (node undo-elt &optional below)
  "Adjust buffer positions of undo elements, starting at NODE's
and going up the tree (or down the active branch if BELOW is
non-nil) and through the nodes' undo elements until we reach
UNDO-ELT.  UNDO-ELT must appear somewhere in the undo changeset
of either NODE itself or some node above it in the tree."
  (let ((delta (list (undo-delta undo-elt)))
	(undo-list (undo-tree-node-undo node)))
    ;; adjust elements until we reach UNDO-ELT
    (while (and (car undo-list)
		(not (eq (car undo-list) undo-elt)))
      (setcar undo-list
	      (undo-tree-apply-deltas (car undo-list) delta -1))
      ;; move to next undo element in list, or to next node if we've run out
      ;; of elements
      (unless (car (setq undo-list (cdr undo-list)))
	(if below
	    (setq node (nth (undo-tree-node-branch node)
			    (undo-tree-node-next node)))
	  (setq node (undo-tree-node-previous node)))
	(setq undo-list (undo-tree-node-undo node))))))



(defun undo-tree-apply-deltas (undo-elt deltas &optional sgn)
  ;; Apply DELTAS in order to UNDO-ELT, multiplying deltas by SGN
  ;; (only useful value for SGN is -1).
  (let (position offset)
    (dolist (delta deltas)
      (setq position (car delta)
	    offset (* (cdr delta) (or sgn 1)))
      (cond
       ;; POSITION
       ((integerp undo-elt)
	(when (>= undo-elt position)
	  (setq undo-elt (- undo-elt offset))))
       ;; nil (or any other atom)
       ((atom undo-elt))
       ;; (TEXT . POSITION)
       ((stringp (car undo-elt))
	(let ((text-pos (abs (cdr undo-elt)))
	      (point-at-end (< (cdr undo-elt) 0)))
	  (if (>= text-pos position)
	      (setcdr undo-elt (* (if point-at-end -1 1)
				  (- text-pos offset))))))
       ;; (BEGIN . END)
       ((integerp (car undo-elt))
	(when (>= (car undo-elt) position)
	  (setcar undo-elt (- (car undo-elt) offset))
	  (setcdr undo-elt (- (cdr undo-elt) offset))))
       ;; (nil PROPERTY VALUE BEG . END)
       ((null (car undo-elt))
	(let ((tail (nthcdr 3 undo-elt)))
	  (when (>= (car tail) position)
	    (setcar tail (- (car tail) offset))
	    (setcdr tail (- (cdr tail) offset)))))
       ))
    undo-elt))



(defun undo-tree-repeated-undo-in-region-p (start end)
  ;; Return non-nil if undo-in-region between START and END is a repeated
  ;; undo-in-region
  (let ((node (undo-tree-current buffer-undo-tree)))
    (and (setq node
	       (nth (undo-tree-node-branch node) (undo-tree-node-next node)))
	 (eq (undo-tree-node-undo-beginning node) start)
	 (eq (undo-tree-node-undo-end node) end))))


(defun undo-tree-repeated-redo-in-region-p (start end)
  ;; Return non-nil if undo-in-region between START and END is a repeated
  ;; undo-in-region
  (let ((node (undo-tree-current buffer-undo-tree)))
    (and (eq (undo-tree-node-redo-beginning node) start)
	 (eq (undo-tree-node-redo-end node) end))))


;; Return non-nil if undo-in-region between START and END is simply
;; reverting the last redo-in-region
(defalias 'undo-tree-reverting-undo-in-region-p
  'undo-tree-repeated-undo-in-region-p)


;; Return non-nil if redo-in-region between START and END is simply
;; reverting the last undo-in-region
(defalias 'undo-tree-reverting-redo-in-region-p
  'undo-tree-repeated-redo-in-region-p)




;;; =====================================================================
;;;                        Undo-tree commands

(defvar undo-tree-timer nil)

;;;###autoload
(define-minor-mode undo-tree-mode
  "Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}"

  nil                       ; init value
  undo-tree-mode-lighter    ; lighter
  undo-tree-map             ; keymap

  (cond
   (undo-tree-mode  ; enabling `undo-tree-mode'
    (set (make-local-variable 'undo-limit)
	 (if undo-tree-limit
	     (max undo-limit undo-tree-limit)
	   most-positive-fixnum))
    (set (make-local-variable 'undo-strong-limit)
	 (if undo-tree-limit
	     (max undo-strong-limit undo-tree-strong-limit)
	   most-positive-fixnum))
    (set (make-local-variable 'undo-outer-limit)  ; null `undo-outer-limit' means no limit
	 (when (and undo-tree-limit undo-outer-limit undo-outer-limit)
	   (max undo-outer-limit undo-tree-outer-limit)))
    (when (null undo-tree-limit)
      (setq undo-tree-timer
	    (run-with-idle-timer 5 'repeat #'undo-list-transfer-to-tree)))
    (add-hook 'post-gc-hook #'undo-tree-post-gc nil))

   (t  ; disabling `undo-tree-mode'
    ;; rebuild `buffer-undo-list' from tree so Emacs undo can work
    (undo-list-rebuild-from-tree)
    (setq buffer-undo-tree nil)
    (remove-hook 'post-gc-hook #'undo-tree-post-gc 'local)
    (when (timerp undo-tree-timer) (cancel-timer undo-tree-timer))
    (kill-local-variable 'undo-limit)
    (kill-local-variable 'undo-strong-limit)
    (kill-local-variable 'undo-outer-limit))))


(defun turn-on-undo-tree-mode (&optional print-message)
  "Enable `undo-tree-mode' in the current buffer, when appropriate.
Some major modes implement their own undo system, which should
not normally be overridden by `undo-tree-mode'. This command does
not enable `undo-tree-mode' in such buffers. If you want to force
`undo-tree-mode' to be enabled regardless, use (undo-tree-mode 1)
instead.

The heuristic used to detect major modes in which
`undo-tree-mode' should not be used is to check whether either
the `undo' command has been remapped, or the default undo
keybindings (C-/ and C-_) have been overridden somewhere other
than in the global map. In addition, `undo-tree-mode' will not be
enabled if the buffer's `major-mode' appears in
`undo-tree-incompatible-major-modes'."
  (interactive "p")
  (if (or (key-binding [remap undo])
	  (undo-tree-overridden-undo-bindings-p)
	  (memq major-mode undo-tree-incompatible-major-modes))
      (when print-message
	(message "Buffer does not support undo-tree-mode;\
 undo-tree-mode NOT enabled"))
    (undo-tree-mode 1)))


(defun undo-tree-overridden-undo-bindings-p ()
  "Returns t if default undo bindings are overridden, nil otherwise.
Checks if either of the default undo key bindings (\"C-/\" or
\"C-_\") are overridden in the current buffer by any keymap other
than the global one. (So global redefinitions of the default undo
key bindings do not count.)"
  (let ((binding1 (lookup-key (current-global-map) [?\C-/]))
	(binding2 (lookup-key (current-global-map) [?\C-_])))
    (global-set-key [?\C-/] 'undo)
    (global-set-key [?\C-_] 'undo)
    (unwind-protect
	(or (and (key-binding [?\C-/])
		 (not (eq (key-binding [?\C-/]) 'undo)))
	    (and (key-binding [?\C-_])
		 (not (eq (key-binding [?\C-_]) 'undo))))
      (global-set-key [?\C-/] binding1)
      (global-set-key [?\C-_] binding2))))


;;;###autoload
(define-globalized-minor-mode global-undo-tree-mode
  undo-tree-mode turn-on-undo-tree-mode)



(defun undo-tree-undo (&optional arg)
  "Undo changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

In Transient Mark mode when the mark is active, only undo changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits undo to
changes within the current region."
  (interactive "*P")
  (unless undo-tree-mode
    (user-error "Undo-tree mode not enabled in buffer"))
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  (undo-tree-undo-1 arg)
  ;; inform user if at branch point
  (when (> (undo-tree-num-branches) 1) (message "Undo branch point!")))


(defun undo-tree-undo-1 (&optional arg preserve-redo preserve-timestamps)
  ;; Internal undo function. An active mark in `transient-mark-mode', or
  ;; non-nil ARG otherwise, enables undo-in-region. Non-nil PRESERVE-REDO
  ;; causes the existing redo record to be preserved, rather than replacing it
  ;; with the new one generated by undoing. Non-nil PRESERVE-TIMESTAMPS
  ;; disables updating of timestamps in visited undo-tree nodes. (This latter
  ;; should *only* be used when temporarily visiting another undo state and
  ;; immediately returning to the original state afterwards. Otherwise, it
  ;; could cause history-discarding errors.)
  (let ((undo-in-progress t)
	(undo-in-region (and undo-tree-enable-undo-in-region
			     (or (region-active-p)
				 (and arg (not (numberp arg))))))
	pos current)
    ;; transfer entries accumulated in `buffer-undo-list' to
    ;; `buffer-undo-tree'
    (undo-list-transfer-to-tree)

    (dotimes (_ (or (and (numberp arg) (prefix-numeric-value arg)) 1))
      ;; check if at top of undo tree
      (unless (undo-tree-node-previous (undo-tree-current buffer-undo-tree))
	(user-error "No further undo information"))

      ;; if region is active, or a non-numeric prefix argument was supplied,
      ;; try to pull out a new branch of changes affecting the region
      (when (and undo-in-region
		 (not (undo-tree-pull-undo-in-region-branch
		       (region-beginning) (region-end))))
	(user-error "No further undo information for region"))

      ;; remove any GC'd elements from node's undo list
      (setq current (undo-tree-current buffer-undo-tree))
      (cl-decf (undo-tree-size buffer-undo-tree)
	       (undo-list-byte-size (undo-tree-node-undo current)))
      (setf (undo-tree-node-undo current)
	    (undo-list-clean-GCd-elts (undo-tree-node-undo current)))
      (cl-incf (undo-tree-size buffer-undo-tree)
	       (undo-list-byte-size (undo-tree-node-undo current)))
      ;; undo one record from undo tree
      (when undo-in-region
	(setq pos (set-marker (make-marker) (point)))
	(set-marker-insertion-type pos t))
      (primitive-undo 1 (undo-tree-copy-list (undo-tree-node-undo current)))
      (undo-boundary)

      ;; if preserving old redo record, discard new redo entries that
      ;; `primitive-undo' has added to `buffer-undo-list', and remove any GC'd
      ;; elements from node's redo list
      (if preserve-redo
	  (progn
	    (undo-list-pop-changeset buffer-undo-list)
	    (cl-decf (undo-tree-size buffer-undo-tree)
		     (undo-list-byte-size (undo-tree-node-redo current)))
	    (setf (undo-tree-node-redo current)
		  (undo-list-clean-GCd-elts (undo-tree-node-redo current)))
	    (cl-incf (undo-tree-size buffer-undo-tree)
		     (undo-list-byte-size (undo-tree-node-redo current))))
	;; otherwise, record redo entries that `primitive-undo' has added to
	;; `buffer-undo-list' in current node's redo record, replacing
	;; existing entry if one already exists
	(cl-decf (undo-tree-size buffer-undo-tree)
		 (undo-list-byte-size (undo-tree-node-redo current)))
	(setf (undo-tree-node-redo current)
	      (undo-list-pop-changeset buffer-undo-list 'discard-pos))
	(cl-incf (undo-tree-size buffer-undo-tree)
		 (undo-list-byte-size (undo-tree-node-redo current))))

      ;; rewind current node and update timestamp
      (setf (undo-tree-current buffer-undo-tree)
	    (undo-tree-node-previous (undo-tree-current buffer-undo-tree)))
      (unless preserve-timestamps
	(setf (undo-tree-node-timestamp (undo-tree-current buffer-undo-tree))
	      (current-time)))

      ;; if undoing-in-region, record current node, region and direction so we
      ;; can tell if undo-in-region is repeated, and re-activate mark if in
      ;; `transient-mark-mode'; if not, erase any leftover data
      (if (not undo-in-region)
	  (undo-tree-node-clear-region-data current)
	(goto-char pos)
	;; note: we deliberately want to store the region information in the
	;; node *below* the now current one
	(setf (undo-tree-node-undo-beginning current) (region-beginning)
	      (undo-tree-node-undo-end current) (region-end))
	(set-marker pos nil)))

    ;; undo deactivates mark unless undoing-in-region
    (setq deactivate-mark (not undo-in-region))))



(defun undo-tree-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count.

In Transient Mark mode when the mark is active, only redo changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits redo to
changes within the current region."
  (interactive "*P")
  (unless undo-tree-mode
    (user-error "Undo-tree mode not enabled in buffer"))
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  (undo-tree-redo-1 arg)
  ;; inform user if at branch point
  (when (> (undo-tree-num-branches) 1) (message "Undo branch point!")))


(defun undo-tree-redo-1 (&optional arg preserve-undo preserve-timestamps)
  ;; Internal redo function. An active mark in `transient-mark-mode', or
  ;; non-nil ARG otherwise, enables undo-in-region. Non-nil PRESERVE-UNDO
  ;; causes the existing redo record to be preserved, rather than replacing it
  ;; with the new one generated by undoing. Non-nil PRESERVE-TIMESTAMPS
  ;; disables updating of timestamps in visited undo-tree nodes. (This latter
  ;; should *only* be used when temporarily visiting another undo state and
  ;; immediately returning to the original state afterwards. Otherwise, it
  ;; could cause history-discarding errors.)
  (let ((undo-in-progress t)
	(redo-in-region (and undo-tree-enable-undo-in-region
			     (or (region-active-p)
				 (and arg (not (numberp arg))))))
	pos current)
    ;; transfer entries accumulated in `buffer-undo-list' to
    ;; `buffer-undo-tree'
    (undo-list-transfer-to-tree)

    (dotimes (_ (or (and (numberp arg) (prefix-numeric-value arg)) 1))
      ;; check if at bottom of undo tree
      (when (null (undo-tree-node-next (undo-tree-current buffer-undo-tree)))
	(user-error "No further redo information"))

      ;; if region is active, or a non-numeric prefix argument was supplied,
      ;; try to pull out a new branch of changes affecting the region
      (when (and redo-in-region
		 (not (undo-tree-pull-redo-in-region-branch
		       (region-beginning) (region-end))))
	(user-error "No further redo information for region"))

      ;; get next node (but DON'T advance current node in tree yet, in case
      ;; redoing fails)
      (setq current (undo-tree-current buffer-undo-tree)
	    current (nth (undo-tree-node-branch current)
			 (undo-tree-node-next current)))
      ;; remove any GC'd elements from node's redo list
      (cl-decf (undo-tree-size buffer-undo-tree)
	       (undo-list-byte-size (undo-tree-node-redo current)))
      (setf (undo-tree-node-redo current)
	    (undo-list-clean-GCd-elts (undo-tree-node-redo current)))
      (cl-incf (undo-tree-size buffer-undo-tree)
	       (undo-list-byte-size (undo-tree-node-redo current)))
      ;; redo one record from undo tree
      (when redo-in-region
	(setq pos (set-marker (make-marker) (point)))
	(set-marker-insertion-type pos t))
      (primitive-undo 1 (undo-tree-copy-list (undo-tree-node-redo current)))
      (undo-boundary)
      ;; advance current node in tree
      (setf (undo-tree-current buffer-undo-tree) current)

      ;; if preserving old undo record, discard new undo entries that
      ;; `primitive-undo' has added to `buffer-undo-list', and remove any GC'd
      ;; elements from node's redo list
      (if preserve-undo
	  (progn
	    (undo-list-pop-changeset buffer-undo-list)
	    (cl-decf (undo-tree-size buffer-undo-tree)
		     (undo-list-byte-size (undo-tree-node-undo current)))
	    (setf (undo-tree-node-undo current)
		  (undo-list-clean-GCd-elts (undo-tree-node-undo current)))
	    (cl-incf (undo-tree-size buffer-undo-tree)
		     (undo-list-byte-size (undo-tree-node-undo current))))
	;; otherwise, record undo entries that `primitive-undo' has added to
	;; `buffer-undo-list' in current node's undo record, replacing
	;; existing entry if one already exists
	(cl-decf (undo-tree-size buffer-undo-tree)
		 (undo-list-byte-size (undo-tree-node-undo current)))
	(setf (undo-tree-node-undo current)
	      (undo-list-pop-changeset buffer-undo-list 'discard-pos))
	(cl-incf (undo-tree-size buffer-undo-tree)
		 (undo-list-byte-size (undo-tree-node-undo current))))

      ;; update timestamp
      (unless preserve-timestamps
	(setf (undo-tree-node-timestamp current) (current-time)))

      ;; if redoing-in-region, record current node, region and direction so we
      ;; can tell if redo-in-region is repeated, and re-activate mark if in
      ;; `transient-mark-mode'
      (if (not redo-in-region)
	  (undo-tree-node-clear-region-data current)
	(goto-char pos)
	(setf (undo-tree-node-redo-beginning current) (region-beginning)
	      (undo-tree-node-redo-end current) (region-end))
	(set-marker pos nil)))

    ;; redo deactivates the mark unless redoing-in-region
    (setq deactivate-mark (not redo-in-region))))



(defun undo-tree-switch-branch (branch)
  "Switch to a different BRANCH of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo'."
  (interactive (list (or (and prefix-arg (prefix-numeric-value prefix-arg))
                         (and (not (eq buffer-undo-list t))
			      (undo-list-transfer-to-tree)
			      (let ((b (undo-tree-node-branch
					(undo-tree-current
					 buffer-undo-tree))))
				(cond
				 ;; switch to other branch if only 2
				 ((= (undo-tree-num-branches) 2) (- 1 b))
				 ;; prompt if more than 2
				 ((> (undo-tree-num-branches) 2)
				  (read-number
				   (format "Branch (0-%d, on %d): "
					   (1- (undo-tree-num-branches)) b)))
				 ))))))
  (unless undo-tree-mode
    (user-error "Undo-tree mode not enabled in buffer"))
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  ;; sanity check branch number
  (when (<= (undo-tree-num-branches) 1)
    (user-error "Not at undo branch point"))
  (when (or (< branch 0) (> branch (1- (undo-tree-num-branches))))
    (user-error "Invalid branch number"))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; switch branch
  (setf (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
	branch)
  (message "Switched to branch %d" branch))


(defun undo-tree-set (node &optional preserve-timestamps)
  ;; Set buffer to state corresponding to NODE. Returns intersection point
  ;; between path back from current node and path back from selected NODE.
  ;; Non-nil PRESERVE-TIMESTAMPS disables updating of timestamps in visited
  ;; undo-tree nodes. (This should *only* be used when temporarily visiting
  ;; another undo state and immediately returning to the original state
  ;; afterwards. Otherwise, it could cause history-discarding errors.)
  (let ((path (make-hash-table :test 'eq))
        (n node))
    (puthash (undo-tree-root buffer-undo-tree) t path)
    ;; build list of nodes leading back from selected node to root, updating
    ;; branches as we go to point down to selected node
    (while (progn
             (puthash n t path)
             (when (undo-tree-node-previous n)
               (setf (undo-tree-node-branch (undo-tree-node-previous n))
                     (undo-tree-position
                      n (undo-tree-node-next (undo-tree-node-previous n))))
               (setq n (undo-tree-node-previous n)))))
    ;; work backwards from current node until we intersect path back from
    ;; selected node
    (setq n (undo-tree-current buffer-undo-tree))
    (while (not (gethash n path))
      (setq n (undo-tree-node-previous n)))
    ;; ascend tree until intersection node
    (while (not (eq (undo-tree-current buffer-undo-tree) n))
      (undo-tree-undo-1 nil nil preserve-timestamps))
    ;; descend tree until selected node
    (while (not (eq (undo-tree-current buffer-undo-tree) node))
      (undo-tree-redo-1 nil nil preserve-timestamps))
    n))  ; return intersection node



(defun undo-tree-save-state-to-register (register)
  "Store current undo-tree state to REGISTER.
The saved state can be restored using
`undo-tree-restore-state-from-register'.
Argument is a character, naming the register."
  (interactive "cUndo-tree state to register: ")
  (unless undo-tree-mode
    (user-error "Undo-tree mode not enabled in buffer"))
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; save current node to REGISTER
  (set-register
   register (registerv-make
	     (undo-tree-make-register-data
	      (current-buffer) (undo-tree-current buffer-undo-tree))
	     :print-func 'undo-tree-register-data-print-func))
  ;; record REGISTER in current node, for visualizer
  (setf (undo-tree-node-register (undo-tree-current buffer-undo-tree))
	register))



(defun undo-tree-restore-state-from-register (register)
  "Restore undo-tree state from REGISTER.
The state must be saved using `undo-tree-save-state-to-register'.
Argument is a character, naming the register."
  (interactive "*cRestore undo-tree state from register: ")
  (unless undo-tree-mode
    (user-error "Undo-tree mode not enabled in buffer"))
  ;; throw error if undo is disabled in buffer, or if register doesn't contain
  ;; an undo-tree node
  (let ((data (registerv-data (get-register register))))
    (cond
     ((eq buffer-undo-list t)
      (user-error "No undo information in this buffer"))
     ((not (undo-tree-register-data-p data))
      (user-error "Register doesn't contain undo-tree state"))
     ((not (eq (current-buffer) (undo-tree-register-data-buffer data)))
      (user-error "Register contains undo-tree state for a different buffer")))
    ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
    (undo-list-transfer-to-tree)
    ;; restore buffer state corresponding to saved node
    (undo-tree-set (undo-tree-register-data-node data))))




;;; =====================================================================
;;;                       Undo-tree menu bar

(defvar undo-tree-old-undo-menu-item nil)

(defun undo-tree-update-menu-bar ()
  "Update `undo-tree-mode' Edit menu items."
  (if undo-tree-mode
      (progn
	;; save old undo menu item, and install undo/redo menu items
	(setq undo-tree-old-undo-menu-item
	      (cdr (assq 'undo (lookup-key global-map [menu-bar edit]))))
	(define-key (lookup-key global-map [menu-bar edit])
	  [undo] '(menu-item "Undo" undo-tree-undo
			     :enable (and undo-tree-mode
					  (not buffer-read-only)
					  (not (eq t buffer-undo-list))
					  (not (eq nil buffer-undo-tree))
					  (undo-tree-node-previous
					   (undo-tree-current buffer-undo-tree)))
			     :help "Undo last operation"))
	(define-key-after (lookup-key global-map [menu-bar edit])
	  [redo] '(menu-item "Redo" undo-tree-redo
			     :enable (and undo-tree-mode
					  (not buffer-read-only)
					  (not (eq t buffer-undo-list))
					  (not (eq nil buffer-undo-tree))
					  (undo-tree-node-next
					   (undo-tree-current buffer-undo-tree)))
			     :help "Redo last operation")
	  'undo))
    ;; uninstall undo/redo menu items
    (define-key (lookup-key global-map [menu-bar edit])
      [undo] undo-tree-old-undo-menu-item)
    (define-key (lookup-key global-map [menu-bar edit])
      [redo] nil)))

(add-hook 'menu-bar-update-hook 'undo-tree-update-menu-bar)




;;; =====================================================================
;;;                    Persistent storage commands

(defvar undo-tree-save-format-version 1
  "Undo-tree history file format version.")


(defun undo-tree-make-history-save-file-name (file)
  "Create the undo history file name for FILE.
Normally this is the file's name with \".\" prepended and
\".~undo-tree~\" appended.

A match for FILE is sought in `undo-tree-history-directory-alist'
\(see the documentation of that variable for details\). If the
directory for the backup doesn't exist, it is created."
  (let* ((backup-directory-alist undo-tree-history-directory-alist)
	 (name (make-backup-file-name-1 file)))
    (concat (file-name-directory name) "." (file-name-nondirectory name)
	    ".~undo-tree~")))


(defun undo-tree-serialize (tree)
  "Serialise undo-tree TREE to current buffer."
  ;; write root
  (let ((data (undo-tree-copy-node-save-data (undo-tree-root tree))))
    (when (eq (undo-tree-root tree) (undo-tree-current tree))
      (setf (undo-tree-node-next data) 'current))
    (prin1 data (current-buffer)))
  (terpri (current-buffer))
  ;; Note: We serialise in breadth-first order, as undo-trees are typically
  ;;       much deeper than they are wide, so this is more memory-efficient.
  (let ((queue (make-queue)))
    (queue-enqueue queue (undo-tree-root tree))
    (while (not (queue-empty queue))
      (prin1 (mapcar
	      (lambda (n)
		(queue-enqueue queue n)
		(let ((data (undo-tree-copy-node-save-data n)))
		  ;; use empty next field to mark current node
		  (when (eq n (undo-tree-current tree))
		    (setf (undo-tree-node-next data) 'current))
		  data))
	      (undo-tree-node-next (queue-dequeue queue)))
	     (current-buffer))
      (terpri (current-buffer)))))


(defun undo-tree-deserialize ()
  "Deserialize and return undo-tree from current buffer."
  (let ((tree (make-undo-tree))
	(queue (make-queue))
	node)
    ;; read root
    (setf (undo-tree-root tree) (read (current-buffer)))
    (queue-enqueue queue (undo-tree-root tree))
    ;; reconstruct tree in breadth-first order
    (while (not (queue-empty queue))
      (setq node (queue-dequeue queue))
      (when (eq (undo-tree-node-next node) 'current)
	(setf (undo-tree-current tree) node))
      (setf (undo-tree-node-next node) (read (current-buffer)))
      (mapc (lambda (n) (queue-enqueue queue n))
	    (undo-tree-node-next node)))
    ;; restore parent links
    (undo-tree-recircle tree)
    tree))


(defun undo-tree-serialize-old-format (tree)
  ;; make tmp copy of TREE
  (setq tree (undo-tree-copy tree))
  ;; decircle and discard object pool before saving
  (undo-tree-decircle tree)
  (setf (undo-tree-object-pool tree) nil)
  ;; run pre-save transformer functions
  (when undo-tree-pre-save-element-functions
    (undo-tree-mapc
     (lambda (node)
       (let ((changeset (undo-tree-node-undo node)))
	 (run-hook-wrapped
	  'undo-tree-pre-save-element-functions
	  (lambda (fun)
	    (setq changeset (delq nil (mapcar fun changeset)))
	    nil))
	 (setf (undo-tree-node-undo node) changeset))
       (let ((changeset (undo-tree-node-redo node)))
	 (run-hook-wrapped
	  'undo-tree-pre-save-element-functions
	  (lambda (fun)
	    (setq changeset (delq nil (mapcar fun changeset)))
	    nil))
	 (setf (undo-tree-node-redo node) changeset)))
     (undo-tree-root tree)))
  ;; write tree
  (let ((print-circle t)) (prin1 tree (current-buffer))))


(defun undo-tree-deserialize-old-format ()
  ;; read tree
  (let ((tree (read (current-buffer))))
    ;; run post-load transformer functions
    (when undo-tree-post-load-element-functions
      (undo-tree-mapc
       (lambda (node)
	 (let ((changeset (undo-tree-node-undo node)))
	   (run-hook-wrapped
	    'undo-tree-post-load-element-functions
	    (lambda (fun)
	      (setq changeset (delq nil (mapcar fun changeset)))))
	   (setf (undo-tree-node-undo node) changeset))
	 (let ((changeset (undo-tree-node-redo node)))
	   (run-hook-wrapped
	    'undo-tree-post-load-element-functions
	    (lambda (fun)
	      (setq changeset (delq nil (mapcar fun changeset)))))
	   (setf (undo-tree-node-redo node) changeset)))
       (undo-tree-root tree)))
    ;; initialise empty undo-tree object pool
    (setf (undo-tree-object-pool tree)
	  (make-hash-table :test 'eq :weakness 'value))
    ;; restore parent links
    (undo-tree-recircle tree)
    tree))



(defun undo-tree-save-history (&optional filename overwrite)
  "Store undo-tree history to file.

If optional argument FILENAME is omitted, default save file is
\".<buffer-file-name>.~undo-tree\" if buffer is visiting a file.
Otherwise, prompt for one.

If OVERWRITE is non-nil, any existing file will be overwritten
without asking for confirmation."
  (interactive)
  (unless undo-tree-mode
    (user-error "Undo-tree mode not enabled in buffer"))
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  (undo-list-transfer-to-tree)
  (when (and buffer-undo-tree (not (eq buffer-undo-tree t)))
    ;; (undo-tree-kill-visualizer)
    ;; ;; should be cleared already by killing the visualizer, but writes
    ;; ;; unreasable data if not for some reason, so just in case...
    ;; (undo-tree-clear-visualizer-data buffer-undo-tree)
    (let ((buff (current-buffer))
	  (tree buffer-undo-tree))
      ;; get filename
      (unless filename
	(setq filename
	      (if buffer-file-name
		  (undo-tree-make-history-save-file-name buffer-file-name)
		(expand-file-name (read-file-name "File to save in: ") nil))))
      (when (or (not (file-exists-p filename))
		overwrite
		(yes-or-no-p (format "Overwrite \"%s\"? " filename)))

	;; print undo-tree to file
	;; Note: We use `with-temp-buffer' instead of `with-temp-file' to
	;;       allow `auto-compression-mode' to take effect, in case user
	;;       has overridden or advised the default
	;;       `undo-tree-make-history-save-file-name' to add a compressed
	;;       file extension.
	(with-temp-buffer
	  ;; write version number; (original save file format (version 0) has no version string)
	  (unless (= undo-tree-save-format-version 0)
	    (prin1 (cons 'undo-tree-save-format-version undo-tree-save-format-version)
		   (current-buffer))
	    (terpri (current-buffer)))
	  ;; write hash
	  (prin1 (sha1 buff) (current-buffer))
	  (terpri (current-buffer))
	  ;; write tree
	  (cl-case undo-tree-save-format-version
	    (0 (undo-tree-serialize-old-format tree))
	    (1 (undo-tree-serialize tree))
	    (t (error "Unknown `undo-tree-save-format-version'; undo-tree history *not* saved")))
	  ;; write file
	  (with-auto-compression-mode
	    (write-region nil nil filename)))))))


(defmacro undo-tree--catch-load-history-error (error-fmt &rest body)
  `(condition-case nil
       (progn ,@body)
     (error
      (kill-buffer nil)
      (funcall (if noerror #'message #'user-error) ,error-fmt filename)
      (throw 'load-error nil))))


(defun undo-tree-load-history (&optional filename noerror)
  "Load undo-tree history from file, for the current buffer.

If optional argument FILENAME is null, default load file is
\".<buffer-file-name>.~undo-tree\" if buffer is visiting a file.
Otherwise, prompt for one.

If optional argument NOERROR is non-nil, return nil instead of
signaling an error if file is not found.

Note this will overwrite any existing undo history."
  (interactive)
  (unless undo-tree-mode
    (user-error "Undo-tree mode not enabled in buffer"))
  ;; get filename
  (unless filename
    (setq filename
	  (if buffer-file-name
	      (undo-tree-make-history-save-file-name buffer-file-name)
	    (expand-file-name (read-file-name "File to load from: ") nil))))

  ;; attempt to read undo-tree
  (catch 'load-error
    (unless (file-exists-p filename)
      (if noerror
	  (throw 'load-error nil)
	(user-error "File \"%s\" does not exist; could not load undo-tree history"
		    filename)))

    ;; read file contents
    (let ((buff (current-buffer))
	  version hash tree)
	(with-temp-buffer
	  (with-auto-compression-mode (insert-file-contents filename))
	  (goto-char (point-min))

	  (undo-tree--catch-load-history-error
	   "Error reading undo-tree history from \"%s\""
	   ;; read version number
	   (setq version (read (current-buffer)))
	   ;; read hash
	   (cond
	    ((eq (car-safe version) 'undo-tree-save-format-version)
	     (setq version (cdr version))
	     (setq hash (read (current-buffer))))
	    ;; original save file format (version 0) has no version string
	    ((stringp version)
	     (setq hash version
		   version 0))
	    (t (error "Error"))))

	  ;; check hash
	  (undo-tree--catch-load-history-error
	    "Buffer has been modified since undo-tree history was saved to
	  \"%s\"; could not load undo-tree history"
	    (unless (string= (sha1 buff) hash) (error "Error")))

	  ;; read tree
	  (undo-tree--catch-load-history-error
	   "Error reading undo-tree history from \"%s\""
	   (setq tree
		 (cl-case version
		   (0 (undo-tree-deserialize-old-format))
		   (1 (undo-tree-deserialize))
		   (t (error "Error")))))
	  (kill-buffer nil))

	(setq buffer-undo-tree tree
	      buffer-undo-list (list nil 'undo-tree-canary)))))



;; Versions of save/load functions for use in hooks
(defun undo-tree-save-history-from-hook ()
  (when (and undo-tree-mode undo-tree-auto-save-history
	     (not (eq buffer-undo-list t))
	     buffer-file-name
	     (file-writable-p
	      (undo-tree-make-history-save-file-name buffer-file-name)))
    (undo-tree-save-history nil 'overwrite) nil))

(define-obsolete-function-alias
  'undo-tree-save-history-hook 'undo-tree-save-history-from-hook
  "`undo-tree-save-history-hook' is obsolete since undo-tree
 version 0.6.6. Use `undo-tree-save-history-from-hook' instead.")


(defun undo-tree-load-history-from-hook ()
  (when (and undo-tree-mode undo-tree-auto-save-history
	     (not (eq buffer-undo-list t))
	     (not revert-buffer-in-progress-p))
    (undo-tree-load-history nil 'noerror)))

(define-obsolete-function-alias
  'undo-tree-load-history-hook 'undo-tree-load-history-from-hook
  "`undo-tree-load-history-hook' is obsolete since undo-tree
 version 0.6.6. Use `undo-tree-load-history-from-hook' instead.")


;; install history-auto-save hooks
(add-hook 'write-file-functions #'undo-tree-save-history-from-hook)
(add-hook 'kill-buffer-hook #'undo-tree-save-history-from-hook)
(add-hook 'find-file-hook #'undo-tree-load-history-from-hook)




;;; =====================================================================
;;;                    Visualizer drawing functions

(defun undo-tree-visualize ()
  "Visualize the current buffer's undo tree."
  (interactive "*")
  (unless undo-tree-mode
    (user-error "Undo-tree mode not enabled in buffer"))
  (deactivate-mark)
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; add hook to kill visualizer buffer if original buffer is changed
  (add-hook 'before-change-functions 'undo-tree-kill-visualizer nil t)
  ;; prepare *undo-tree* buffer, then draw tree in it
  (let ((undo-tree buffer-undo-tree)
        (buff (current-buffer))
	(display-buffer-mark-dedicated 'soft))
    (switch-to-buffer-other-window
     (get-buffer-create undo-tree-visualizer-buffer-name))
    (setq undo-tree-visualizer-parent-buffer buff)
    (setq undo-tree-visualizer-parent-mtime
	  (and (buffer-file-name buff)
	       (nth 5 (file-attributes (buffer-file-name buff)))))
    (setq undo-tree-visualizer-initial-node (undo-tree-current undo-tree))
    (setq undo-tree-visualizer-spacing
	  (undo-tree-visualizer-calculate-spacing))
    (setq buffer-undo-tree undo-tree)
    (undo-tree-visualizer-mode)
    (setq buffer-undo-tree undo-tree)
    (set (make-local-variable 'undo-tree-visualizer-lazy-drawing)
	 (or (eq undo-tree-visualizer-lazy-drawing t)
	     (and (numberp undo-tree-visualizer-lazy-drawing)
		  (>= (undo-tree-count undo-tree)
		      undo-tree-visualizer-lazy-drawing))))
    (when undo-tree-visualizer-diff (undo-tree-visualizer-show-diff))
    (let ((inhibit-read-only t)) (undo-tree-draw-tree undo-tree))))


(defun undo-tree-kill-visualizer (&rest _dummy)
  ;; Kill visualizer. Added to `before-change-functions' hook of original
  ;; buffer when visualizer is invoked.
  (unless (or undo-tree-inhibit-kill-visualizer
	      (null (get-buffer undo-tree-visualizer-buffer-name)))
    (with-current-buffer undo-tree-visualizer-buffer-name
      (undo-tree-visualizer-quit))))



(defun undo-tree-draw-tree (undo-tree)
  ;; Draw undo-tree in current buffer starting from NODE (or root if nil).
  (let ((inhibit-read-only t)
	(node (if undo-tree-visualizer-lazy-drawing
		  (undo-tree-current undo-tree)
		(undo-tree-root undo-tree))))
    (erase-buffer)
    (setq undo-tree-visualizer-needs-extending-down nil
	  undo-tree-visualizer-needs-extending-up nil)
    (undo-tree-clear-visualizer-data undo-tree)
    (undo-tree-compute-widths node)
    ;; lazy drawing starts vertically centred and displaced horizontally to
    ;; the left (window-width/4), since trees will typically grow right
    (if undo-tree-visualizer-lazy-drawing
	(progn
	  (undo-tree-move-down (/ (window-height) 2))
	  (undo-tree-move-forward (max 2 (/ (window-width) 4)))) ; left margin
      ;; non-lazy drawing starts in centre at top of buffer
      (undo-tree-move-down 1)  ; top margin
      (undo-tree-move-forward
       (max (/ (window-width) 2)
	    (+ (undo-tree-node-char-lwidth node)
	       ;; add space for left part of left-most time-stamp
	       (if undo-tree-visualizer-timestamps
		   (/ (- undo-tree-visualizer-spacing 4) 2)
		 0)
	       2))))  ; left margin
    ;; link starting node to its representation in visualizer
    (setf (undo-tree-node-marker node) (make-marker))
    (set-marker-insertion-type (undo-tree-node-marker node) nil)
    (move-marker (undo-tree-node-marker node) (point))
    ;; draw undo-tree
    (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face)
	  node-list)
      (if (not undo-tree-visualizer-lazy-drawing)
	  (undo-tree-extend-down node t)
	(undo-tree-extend-down node)
	(undo-tree-extend-up node)
	(setq node-list undo-tree-visualizer-needs-extending-down
	      undo-tree-visualizer-needs-extending-down nil)
	(while node-list (undo-tree-extend-down (pop node-list)))))
    ;; highlight active branch
    (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
      (undo-tree-highlight-active-branch
       (or undo-tree-visualizer-needs-extending-up
	   (undo-tree-root undo-tree))))
    ;; highlight current node
    (undo-tree-draw-node (undo-tree-current undo-tree) 'current)))


(defun undo-tree-extend-down (node &optional bottom)
  ;; Extend tree downwards starting from NODE and point. If BOTTOM is t,
  ;; extend all the way down to the leaves. If BOTTOM is a node, extend down
  ;; as far as that node. If BOTTOM is an integer, extend down as far as that
  ;; line. Otherwise, only extend visible portion of tree. NODE is assumed to
  ;; already have a node marker. Returns non-nil if anything was actually
  ;; extended.
  (let ((extended nil)
	(cur-stack (list node))
	next-stack)
    ;; don't bother extending if BOTTOM specifies an already-drawn node
    (unless (and (undo-tree-node-p bottom) (undo-tree-node-marker bottom))
      ;; draw nodes layer by layer
      (while (or cur-stack
		 (prog1 (setq cur-stack next-stack)
		   (setq next-stack nil)))
	(setq node (pop cur-stack))
	;; if node is within range being drawn...
	(if (or (eq bottom t)
		(and (undo-tree-node-p bottom)
		     (not (eq (undo-tree-node-previous node) bottom)))
		(and (integerp bottom)
		     (>= bottom (line-number-at-pos
				 (undo-tree-node-marker node))))
		(and (null bottom)
		     (pos-visible-in-window-p (undo-tree-node-marker node)
					      nil t)))
	    ;; ...draw one layer of node's subtree (if not already drawn)
	    (progn
	      (unless (and (undo-tree-node-next node)
			   (undo-tree-node-marker
			    (nth (undo-tree-node-branch node)
				 (undo-tree-node-next node))))
		(goto-char (undo-tree-node-marker node))
		(undo-tree-draw-subtree node)
		(setq extended t))
	      (setq next-stack
		    (append (undo-tree-node-next node) next-stack)))
	  ;; ...otherwise, postpone drawing until later
	  (push node undo-tree-visualizer-needs-extending-down))))
    extended))


(defun undo-tree-extend-up (node &optional top)
  ;; Extend tree upwards starting from NODE. If TOP is t, extend all the way
  ;; to root. If TOP is a node, extend up as far as that node. If TOP is an
  ;; integer, extend up as far as that line. Otherwise, only extend visible
  ;; portion of tree. NODE is assumed to already have a node marker. Returns
  ;; non-nil if anything was actually extended.
  (let ((extended nil) parent)
    ;; don't bother extending if TOP specifies an already-drawn node
    (unless (and (undo-tree-node-p top) (undo-tree-node-marker top))
      (while node
	(setq parent (undo-tree-node-previous node))
	;; if we haven't reached root...
	(if parent
	    ;; ...and node is within range being drawn...
	    (if (or (eq top t)
		    (and (undo-tree-node-p top) (not (eq node top)))
		    (and (integerp top)
			 (< top (line-number-at-pos
				 (undo-tree-node-marker node))))
		    (and (null top)
			 ;; NOTE: we check point in case window-start is outdated
			 (< (min (line-number-at-pos (point))
				 (line-number-at-pos (window-start)))
			    (line-number-at-pos
			     (undo-tree-node-marker node)))))
		;; ...and it hasn't already been drawn
		(when (not (undo-tree-node-marker parent))
		  ;; link parent node to its representation in visualizer
		  (undo-tree-compute-widths parent)
		  (undo-tree-move-to-parent node)
		  (setf (undo-tree-node-marker parent) (make-marker))
		  (set-marker-insertion-type
		   (undo-tree-node-marker parent) nil)
		  (move-marker (undo-tree-node-marker parent) (point))
		  ;; draw subtree beneath parent
		  (setq undo-tree-visualizer-needs-extending-down
			(nconc (delq node (undo-tree-draw-subtree parent))
			       undo-tree-visualizer-needs-extending-down))
		  (setq extended t))
	      ;; ...otherwise, postpone drawing for later and exit
	      (setq undo-tree-visualizer-needs-extending-up (when parent node)
		    parent nil))

	  ;; if we've reached root, stop extending and add top margin
	  (setq undo-tree-visualizer-needs-extending-up nil)
	  (goto-char (undo-tree-node-marker node))
	  (undo-tree-move-up 1)  ; top margin
	  (delete-region (point-min) (line-beginning-position)))
	;; next iteration
	(setq node parent)))
    extended))


(defun undo-tree-expand-down (from &optional to)
  ;; Expand tree downwards. FROM is the node to start expanding from. Stop
  ;; expanding at TO if specified. Otherwise, just expand visible portion of
  ;; tree and highlight active branch from FROM.
  (when undo-tree-visualizer-needs-extending-down
    (let ((inhibit-read-only t)
	  node-list extended)
      ;; extend down as far as TO node
      (when to
	(setq extended (undo-tree-extend-down from to))
	(goto-char (undo-tree-node-marker to))
	(redisplay t))  ; force redisplay to scroll buffer if necessary
      ;; extend visible portion of tree downwards
      (setq node-list undo-tree-visualizer-needs-extending-down
	    undo-tree-visualizer-needs-extending-down nil)
      (when node-list
	(dolist (n node-list)
	  (when (undo-tree-extend-down n) (setq extended t)))
	;; highlight active branch in newly-extended-down portion, if any
	(when extended
	  (let ((undo-tree-insert-face
		 'undo-tree-visualizer-active-branch-face))
	    (undo-tree-highlight-active-branch from)))))))


(defun undo-tree-expand-up (from &optional to)
  ;; Expand tree upwards. FROM is the node to start expanding from, TO is the
  ;; node to stop expanding at. If TO node isn't specified, just expand visible
  ;; portion of tree and highlight active branch down to FROM.
  (when undo-tree-visualizer-needs-extending-up
    (let ((inhibit-read-only t)
	  extended node-list)
      ;; extend up as far as TO node
      (when to
	(setq extended (undo-tree-extend-up from to))
	(goto-char (undo-tree-node-marker to))
	;; simulate auto-scrolling if close to top of buffer
	(when (<= (line-number-at-pos (point)) scroll-margin)
	  (undo-tree-move-up (if (= scroll-conservatively 0)
				 (/ (window-height) 2) 3))
	  (when (undo-tree-extend-up to) (setq extended t))
	  (goto-char (undo-tree-node-marker to))
	  (unless (= scroll-conservatively 0) (recenter scroll-margin))))
      ;; extend visible portion of tree upwards
      (and undo-tree-visualizer-needs-extending-up
	   (undo-tree-extend-up undo-tree-visualizer-needs-extending-up)
	   (setq extended t))
      ;; extend visible portion of tree downwards
      (setq node-list undo-tree-visualizer-needs-extending-down
	    undo-tree-visualizer-needs-extending-down nil)
      (dolist (n node-list) (undo-tree-extend-down n))
      ;; highlight active branch in newly-extended-up portion, if any
      (when extended
	(let ((undo-tree-insert-face
	       'undo-tree-visualizer-active-branch-face))
	  (undo-tree-highlight-active-branch
	   (or undo-tree-visualizer-needs-extending-up
	       (undo-tree-root buffer-undo-tree))
	   from))))))



(defun undo-tree-highlight-active-branch (node &optional end)
  ;; Draw highlighted active branch below NODE in current buffer. Stop
  ;; highlighting at END node if specified.
  (let ((stack (list node)))
    ;; draw active branch
    (while stack
      (setq node (pop stack))
      (unless (or (eq node end)
		  (memq node undo-tree-visualizer-needs-extending-down))
	(goto-char (undo-tree-node-marker node))
	(setq node (undo-tree-draw-subtree node 'active)
	      stack (nconc stack node))))))


(defun undo-tree-draw-node (node &optional current)
  ;; Draw symbol representing NODE in visualizer. If CURRENT is non-nil, node
  ;; is current node.
  (goto-char (undo-tree-node-marker node))
  (when undo-tree-visualizer-timestamps
    (undo-tree-move-backward (/ undo-tree-visualizer-spacing 2)))

  (let* ((undo-tree-insert-face (and undo-tree-insert-face
				     (or (and (consp undo-tree-insert-face)
					      undo-tree-insert-face)
					 (list undo-tree-insert-face))))
	 (register (undo-tree-node-register node))
	 (unmodified (if undo-tree-visualizer-parent-mtime
			 (undo-tree-node-unmodified-p
			  node undo-tree-visualizer-parent-mtime)
		       (undo-tree-node-unmodified-p node)))
	node-string)
    ;; check node's register (if any) still stores appropriate undo-tree state
    (unless (and register
		 (undo-tree-register-data-p
		  (registerv-data (get-register register)))
		 (eq node (undo-tree-register-data-node
			   (registerv-data (get-register register)))))
      (setq register nil))
    ;; represent node by different symbols, depending on whether it's the
    ;; current node, is saved in a register, or corresponds to an unmodified
    ;; buffer
    (setq node-string
	    (cond
	     (undo-tree-visualizer-timestamps
	        (undo-tree-timestamp-to-string
	         (undo-tree-node-timestamp node)
		 undo-tree-visualizer-relative-timestamps
		 current register))
	     (register (char-to-string register))
	     (unmodified "s")
	     (current "x")
	     (t "o"))
	  undo-tree-insert-face
	    (nconc
	     (cond
	      (current    (list 'undo-tree-visualizer-current-face))
	      (unmodified (list 'undo-tree-visualizer-unmodified-face))
	      (register   (list 'undo-tree-visualizer-register-face)))
	     undo-tree-insert-face))
    ;; draw node and link it to its representation in visualizer
    (undo-tree-insert node-string)
    (undo-tree-move-backward (if undo-tree-visualizer-timestamps
				 (1+ (/ undo-tree-visualizer-spacing 2))
			       1))
    (move-marker (undo-tree-node-marker node) (point))
    (put-text-property (point) (1+ (point)) 'undo-tree-node node)))


(defun undo-tree-draw-subtree (node &optional active-branch)
  ;; Draw subtree rooted at NODE. The subtree will start from point.
  ;; If ACTIVE-BRANCH is non-nil, just draw active branch below NODE. Returns
  ;; list of nodes below NODE.
  (let ((num-children (length (undo-tree-node-next node)))
        node-list pos trunk-pos n)
    ;; draw node itself
    (undo-tree-draw-node node)

    (cond
     ;; if we're at a leaf node, we're done
     ((= num-children 0))

     ;; if node has only one child, draw it (not strictly necessary to deal
     ;; with this case separately, but as it's by far the most common case
     ;; this makes the code clearer and more efficient)
     ((= num-children 1)
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (undo-tree-move-backward 1)
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (undo-tree-move-backward 1)
      (undo-tree-move-down 1)
      (setq n (car (undo-tree-node-next node)))
      ;; link next node to its representation in visualizer
      (unless (markerp (undo-tree-node-marker n))
        (setf (undo-tree-node-marker n) (make-marker))
        (set-marker-insertion-type (undo-tree-node-marker n) nil))
      (move-marker (undo-tree-node-marker n) (point))
      ;; add next node to list of nodes to draw next
      (push n node-list))

     ;; if node has multiple children, draw branches
     (t
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (undo-tree-move-backward 1)
      (move-marker (setq trunk-pos (make-marker)) (point))
      ;; left subtrees
      (undo-tree-move-backward
       (- (undo-tree-node-char-lwidth node)
          (undo-tree-node-char-lwidth
           (car (undo-tree-node-next node)))))
      (move-marker (setq pos (make-marker)) (point))
      (setq n (cons nil (undo-tree-node-next node)))
      (dotimes (_ (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (undo-tree-node-branch node)
                           (undo-tree-node-next node))))
          (undo-tree-move-forward 2)
          (undo-tree-insert ?_ (- trunk-pos pos 2))
          (goto-char pos)
          (undo-tree-move-forward 1)
          (undo-tree-move-down 1)
          (undo-tree-insert ?/)
          (undo-tree-move-backward 2)
          (undo-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (undo-tree-node-marker (car n)))
            (setf (undo-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (undo-tree-node-marker (car n)) nil))
          (move-marker (undo-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (undo-tree-move-forward
         (+ (undo-tree-node-char-rwidth (car n))
            (undo-tree-node-char-lwidth (cadr n))
            undo-tree-visualizer-spacing 1))
        (move-marker pos (point)))
      ;; middle subtree (only when number of children is odd)
      (when (= (mod num-children 2) 1)
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (undo-tree-node-branch node)
                           (undo-tree-node-next node))))
          (undo-tree-move-down 1)
          (undo-tree-insert ?|)
          (undo-tree-move-backward 1)
          (undo-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (undo-tree-node-marker (car n)))
            (setf (undo-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (undo-tree-node-marker (car n)) nil))
          (move-marker (undo-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (undo-tree-move-forward
         (+ (undo-tree-node-char-rwidth (car n))
            (if (cadr n) (undo-tree-node-char-lwidth (cadr n)) 0)
            undo-tree-visualizer-spacing 1))
        (move-marker pos (point)))
      ;; right subtrees
      (move-marker trunk-pos (1+ trunk-pos))
      (dotimes (_ (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (undo-tree-node-branch node)
                           (undo-tree-node-next node))))
          (goto-char trunk-pos)
          (undo-tree-insert ?_ (- pos trunk-pos 1))
          (goto-char pos)
          (undo-tree-move-backward 1)
          (undo-tree-move-down 1)
          (undo-tree-insert ?\\)
          (undo-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (undo-tree-node-marker (car n)))
            (setf (undo-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (undo-tree-node-marker (car n)) nil))
          (move-marker (undo-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (when (cdr n)
          (goto-char pos)
          (undo-tree-move-forward
           (+ (undo-tree-node-char-rwidth (car n))
              (if (cadr n) (undo-tree-node-char-lwidth (cadr n)) 0)
              undo-tree-visualizer-spacing 1))
          (move-marker pos (point))))
      ))
    ;; return list of nodes to draw next
    (nreverse node-list)))


(defun undo-tree-node-char-lwidth (node)
  ;; Return left-width of NODE measured in characters.
  (if (= (length (undo-tree-node-next node)) 0) 0
    (- (* (+ undo-tree-visualizer-spacing 1) (undo-tree-node-lwidth node))
       (if (= (undo-tree-node-cwidth node) 0)
           (1+ (/ undo-tree-visualizer-spacing 2)) 0))))


(defun undo-tree-node-char-rwidth (node)
  ;; Return right-width of NODE measured in characters.
  (if (= (length (undo-tree-node-next node)) 0) 0
    (- (* (+ undo-tree-visualizer-spacing 1) (undo-tree-node-rwidth node))
       (if (= (undo-tree-node-cwidth node) 0)
           (1+ (/ undo-tree-visualizer-spacing 2)) 0))))


(defun undo-tree-insert (str &optional arg)
  ;; Insert character or string STR ARG times, overwriting, and using
  ;; `undo-tree-insert-face'.
  (unless arg (setq arg 1))
  (when (characterp str)
    (setq str (make-string arg str))
    (setq arg 1))
  (dotimes (_ arg) (insert str))
  (setq arg (* arg (length str)))
  (undo-tree-move-forward arg)
  ;; make sure mark isn't active, otherwise `backward-delete-char' might
  ;; delete region instead of single char if transient-mark-mode is enabled
  (setq mark-active nil)
  (backward-delete-char arg)
  (when undo-tree-insert-face
    (put-text-property (- (point) arg) (point) 'face undo-tree-insert-face)))


(defun undo-tree-move-down (&optional arg)
  ;; Move down, extending buffer if necessary.
  (let ((row (line-number-at-pos))
        (col (current-column))
        line)
    (unless arg (setq arg 1))
    (forward-line arg)
    (setq line (line-number-at-pos))
    ;; if buffer doesn't have enough lines, add some
    (when (/= line (+ row arg))
      (cond
       ((< arg 0)
	(insert (make-string (- line row arg) ?\n))
	(forward-line (+ arg (- row line))))
       (t (insert (make-string (- arg (- line row)) ?\n)))))
    (undo-tree-move-forward col)))


(defun undo-tree-move-up (&optional arg)
  ;; Move up, extending buffer if necessary.
  (unless arg (setq arg 1))
  (undo-tree-move-down (- arg)))


(defun undo-tree-move-forward (&optional arg)
  ;; Move forward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (let (n)
    (cond
     ((>= arg 0)
      (setq n (- (line-end-position) (point)))
      (if (> n arg)
	  (forward-char arg)
	(end-of-line)
	(insert (make-string (- arg n) ? ))))
     ((< arg 0)
      (setq arg (- arg))
      (setq n (- (point) (line-beginning-position)))
      (when (< (- n 2) arg)  ; -2 to create left-margin
	;; no space left - shift entire buffer contents right!
	(let ((pos (move-marker (make-marker) (point))))
	  (set-marker-insertion-type pos t)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (insert-before-markers (make-string (- arg -2 n) ? ))
	    (forward-line 1))
	  (goto-char pos)))
      (backward-char arg)))))


(defun undo-tree-move-backward (&optional arg)
  ;; Move backward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (undo-tree-move-forward (- arg)))


(defun undo-tree-move-to-parent (node)
  ;; Move to position of parent of NODE, extending buffer if necessary.
  (let* ((parent (undo-tree-node-previous node))
	 (n (undo-tree-node-next parent))
	 (l (length n)) p)
    (goto-char (undo-tree-node-marker node))
    (unless (= l 1)
      ;; move horizontally
      (setq p (undo-tree-position node n))
      (cond
       ;; node in centre subtree: no horizontal movement
       ((and (= (mod l 2) 1) (= p (/ l 2))))
       ;; node in left subtree: move right
       ((< p (/ l 2))
	(setq n (nthcdr p n))
	(undo-tree-move-forward
	 (+ (undo-tree-node-char-rwidth (car n))
	    (/ undo-tree-visualizer-spacing 2) 1))
	(dotimes (_ (- (/ l 2) p 1))
	  (setq n (cdr n))
	  (undo-tree-move-forward
	   (+ (undo-tree-node-char-lwidth (car n))
	      (undo-tree-node-char-rwidth (car n))
	      undo-tree-visualizer-spacing 1)))
	(when (= (mod l 2) 1)
	  (setq n (cdr n))
	  (undo-tree-move-forward
	   (+ (undo-tree-node-char-lwidth (car n))
	      (/ undo-tree-visualizer-spacing 2) 1))))
       (t ;; node in right subtree: move left
	(setq n (nthcdr (/ l 2) n))
	(when (= (mod l 2) 1)
	  (undo-tree-move-backward
	   (+ (undo-tree-node-char-rwidth (car n))
	      (/ undo-tree-visualizer-spacing 2) 1))
	  (setq n (cdr n)))
	(dotimes (_ (- p (/ l 2) (mod l 2)))
	  (undo-tree-move-backward
	   (+ (undo-tree-node-char-lwidth (car n))
	      (undo-tree-node-char-rwidth (car n))
	      undo-tree-visualizer-spacing 1))
	  (setq n (cdr n)))
	(undo-tree-move-backward
	 (+ (undo-tree-node-char-lwidth (car n))
	    (/ undo-tree-visualizer-spacing 2) 1)))))
    ;; move vertically
    (undo-tree-move-up 3)))


(defun undo-tree-timestamp-to-string
  (timestamp &optional relative current register)
  ;; Convert TIMESTAMP to string (either absolute or RELATVE time), indicating
  ;; if it's the CURRENT node and/or has an associated REGISTER.
  (if relative
      ;; relative time
      (let ((time (floor (float-time
			  (time-subtract (current-time) timestamp))))
	    n)
	(setq time
	      ;; years
	      (if (> (setq n (/ time 315360000)) 0)
		  (if (> n 999) "-ages" (format "-%dy" n))
		(setq time (% time 315360000))
		;; days
		(if (> (setq n (/ time 86400)) 0)
		    (format "-%dd" n)
		  (setq time (% time 86400))
		  ;; hours
		  (if (> (setq n (/ time 3600)) 0)
		      (format "-%dh" n)
		    (setq time (% time 3600))
		    ;; mins
		    (if (> (setq n (/ time 60)) 0)
			(format "-%dm" n)
		      ;; secs
		      (format "-%ds" (% time 60)))))))
	(setq time (concat
		    (if current "*" " ")
		    time
		    (if register (concat "[" (char-to-string register) "]")
		      "   ")))
	(setq n (length time))
	(if (< n 9)
	    (concat (make-string (- 9 n) ? ) time)
	  time))
    ;; absolute time
    (concat (if current " *" "  ")
	    (format-time-string "%H:%M:%S" timestamp)
	    (if register
		(concat "[" (char-to-string register) "]")
	      "   "))))




;;; =====================================================================
;;;                        Visualizer modes

(define-derived-mode
  undo-tree-visualizer-mode special-mode "undo-tree-visualizer"
  "Major mode used in undo-tree visualizer.

The undo-tree visualizer can only be invoked from a buffer in
which `undo-tree-mode' is enabled. The visualizer displays the
undo history tree graphically, and allows you to browse around
the undo history, undoing or redoing the corresponding changes in
the parent buffer.

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  (setq cursor-type nil)
  (setq undo-tree-visualizer-selected-node nil)
  (make-local-variable 'undo-tree-visualizer-timestamps)
  (make-local-variable 'undo-tree-visualizer-diff))


(define-minor-mode undo-tree-visualizer-selection-mode
  "Toggle mode to select nodes in undo-tree visualizer."
  :lighter "Select"
  :keymap undo-tree-visualizer-selection-mode-map
  :group undo-tree
  (cond
   ;; enable selection mode
   (undo-tree-visualizer-selection-mode
    (setq cursor-type 'box)
    (setq undo-tree-visualizer-selected-node
	  (undo-tree-current buffer-undo-tree))
    ;; erase diff (if any), as initially selected node is identical to current
    (when undo-tree-visualizer-diff
      (let ((buff (get-buffer undo-tree-diff-buffer-name))
	    (inhibit-read-only t))
	(when buff (with-current-buffer buff (erase-buffer))))))
   (t ;; disable selection mode
    (setq cursor-type nil)
    (setq undo-tree-visualizer-selected-node nil)
    (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
    (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff)))
   ))




;;; =====================================================================
;;;                        Visualizer commands

(defun undo-tree-visualize-undo (&optional arg)
  "Undo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (let ((old (undo-tree-current buffer-undo-tree))
	current)
    ;; undo in parent buffer
    (switch-to-buffer-other-window undo-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
	(let ((undo-tree-inhibit-kill-visualizer t)) (undo-tree-undo-1 arg))
      (setq current (undo-tree-current buffer-undo-tree))
      (switch-to-buffer-other-window undo-tree-visualizer-buffer-name)
      ;; unhighlight old current node
      (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face)
	    (inhibit-read-only t))
	(undo-tree-draw-node old))
      ;; when using lazy drawing, extend tree upwards as required
      (when undo-tree-visualizer-lazy-drawing
	(undo-tree-expand-up old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (undo-tree-draw-node current 'current))
      ;; update diff display, if any
      (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff)))))


(defun undo-tree-visualize-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (let ((old (undo-tree-current buffer-undo-tree))
	current)
    ;; redo in parent buffer
    (switch-to-buffer-other-window undo-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
	(let ((undo-tree-inhibit-kill-visualizer t)) (undo-tree-redo-1 arg))
      (setq current (undo-tree-current buffer-undo-tree))
      (switch-to-buffer-other-window undo-tree-visualizer-buffer-name)
      ;; unhighlight old current node
      (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face)
	    (inhibit-read-only t))
	(undo-tree-draw-node old))
      ;; when using lazy drawing, extend tree downwards as required
      (when undo-tree-visualizer-lazy-drawing
	(undo-tree-expand-down old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (undo-tree-draw-node current 'current))
      ;; update diff display, if any
      (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff)))))


(defun undo-tree-visualize-switch-branch-right (arg)
  "Switch to next branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo' or `undo-tree-visualizer-redo'."
  (interactive "p")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  ;; un-highlight old active branch below current node
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face)
	(inhibit-read-only t))
    (undo-tree-highlight-active-branch (undo-tree-current buffer-undo-tree)))
  ;; increment branch
  (let ((branch (undo-tree-node-branch (undo-tree-current buffer-undo-tree))))
    (setf (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
	  (cond
	   ((>= (+ branch arg) (undo-tree-num-branches))
	    (1- (undo-tree-num-branches)))
	   ((<= (+ branch arg) 0) 0)
	   (t (+ branch arg))))
    (let ((inhibit-read-only t))
      ;; highlight new active branch below current node
      (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
      (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
	(undo-tree-highlight-active-branch (undo-tree-current buffer-undo-tree)))
      ;; re-highlight current node
      (undo-tree-draw-node (undo-tree-current buffer-undo-tree) 'current))))


(defun undo-tree-visualize-switch-branch-left (arg)
  "Switch to previous branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo' or `undo-tree-visualizer-redo'."
  (interactive "p")
  (undo-tree-visualize-switch-branch-right (- arg)))


(defun undo-tree-visualizer-quit ()
  "Quit the undo-tree visualizer."
  (interactive)
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (undo-tree-clear-visualizer-data buffer-undo-tree)
  ;; remove kill visualizer hook from parent buffer
  (unwind-protect
      (with-current-buffer undo-tree-visualizer-parent-buffer
	(remove-hook 'before-change-functions 'undo-tree-kill-visualizer t))
    ;; kill diff buffer, if any
    (when undo-tree-visualizer-diff (undo-tree-visualizer-hide-diff))
    (let ((parent undo-tree-visualizer-parent-buffer)
	  window)
      ;; kill visualizer buffer
      (kill-buffer nil)
      ;; switch back to parent buffer
      (unwind-protect
	  (if (setq window (get-buffer-window parent))
	      (select-window window)
	    (switch-to-buffer parent))))))


(defun undo-tree-visualizer-abort ()
  "Quit the undo-tree visualizer and return buffer to original state."
  (interactive)
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (let ((node undo-tree-visualizer-initial-node))
    (undo-tree-visualizer-quit)
    (undo-tree-set node)))


(defun undo-tree-visualizer-set (&optional pos)
  "Set buffer to state corresponding to undo tree node
at POS, or point if POS is nil."
  (interactive)
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (unless pos (setq pos (point)))
  (let ((node (get-text-property pos 'undo-tree-node)))
    (when node
      ;; set parent buffer to state corresponding to node at POS
      (switch-to-buffer-other-window undo-tree-visualizer-parent-buffer)
      (let ((undo-tree-inhibit-kill-visualizer t)) (undo-tree-set node))
      (switch-to-buffer-other-window undo-tree-visualizer-buffer-name)
      ;; re-draw undo tree
      (let ((inhibit-read-only t)) (undo-tree-draw-tree buffer-undo-tree))
      (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff)))))


(defun undo-tree-visualizer-mouse-set (pos)
  "Set buffer to state corresponding to undo tree node
at mouse event POS."
  (interactive "@e")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (undo-tree-visualizer-set (event-start (nth 1 pos))))


(defun undo-tree-visualize-undo-to-x (&optional x)
  "Undo to last branch point, register, or saved state.
If X is the symbol `branch', undo to last branch point. If X is
the symbol `register', undo to last register. If X is the symbol
`saved', undo to last saved state. If X is null, undo to first of
these that's encountered.

Interactively, a single \\[universal-argument] specifies
`branch', a double \\[universal-argument] \\[universal-argument]
specifies `saved', and a negative prefix argument specifies
`register'."
  (interactive "P")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (when (and (called-interactively-p 'any) x)
    (setq x (prefix-numeric-value x)
	  x (cond
	     ((< x 0)  'register)
	     ((<= x 4) 'branch)
	     (t        'saved))))
  (let ((current (if undo-tree-visualizer-selection-mode
		     undo-tree-visualizer-selected-node
		   (undo-tree-current buffer-undo-tree)))
	(diff undo-tree-visualizer-diff)
	r)
    (undo-tree-visualizer-hide-diff)
    (while (and (undo-tree-node-previous current)
		(or (if undo-tree-visualizer-selection-mode
			(progn
			  (undo-tree-visualizer-select-previous)
			  (setq current undo-tree-visualizer-selected-node))
		      (undo-tree-visualize-undo)
		      (setq current (undo-tree-current buffer-undo-tree)))
		    t)
		         ;; branch point
		(not (or (and (or (null x) (eq x 'branch))
			      (> (undo-tree-num-branches) 1))
			 ;; register
			 (and (or (null x) (eq x 'register))
			      (setq r (undo-tree-node-register current))
			      (undo-tree-register-data-p
			       (setq r (registerv-data (get-register r))))
			      (eq current (undo-tree-register-data-node r)))
			 ;; saved state
			 (and (or (null x) (eq x 'saved))
			      (undo-tree-node-unmodified-p current))
			 ))))
    ;; update diff display, if any
    (when diff
      (undo-tree-visualizer-show-diff
       (when undo-tree-visualizer-selection-mode
	 undo-tree-visualizer-selected-node)))))


(defun undo-tree-visualize-redo-to-x (&optional x)
  "Redo to last branch point, register, or saved state.
If X is the symbol `branch', redo to last branch point. If X is
the symbol `register', redo to last register. If X is the sumbol
`saved', redo to last saved state. If X is null, redo to first of
these that's encountered.

Interactively, a single \\[universal-argument] specifies
`branch', a double \\[universal-argument] \\[universal-argument]
specifies `saved', and a negative prefix argument specifies
`register'."
  (interactive "P")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (when (and (called-interactively-p 'any) x)
    (setq x (prefix-numeric-value x)
	  x (cond
	     ((< x 0)  'register)
	     ((<= x 4) 'branch)
	     (t        'saved))))
  (let ((current (if undo-tree-visualizer-selection-mode
		     undo-tree-visualizer-selected-node
		   (undo-tree-current buffer-undo-tree)))
	(diff undo-tree-visualizer-diff)
	r)
    (undo-tree-visualizer-hide-diff)
    (while (and (undo-tree-node-next current)
		(or (if undo-tree-visualizer-selection-mode
			(progn
			  (undo-tree-visualizer-select-next)
			  (setq current undo-tree-visualizer-selected-node))
		      (undo-tree-visualize-redo)
		      (setq current (undo-tree-current buffer-undo-tree)))
		    t)
		         ;; branch point
		(not (or (and (or (null x) (eq x 'branch))
			      (> (undo-tree-num-branches) 1))
			 ;; register
			 (and (or (null x) (eq x 'register))
			      (setq r (undo-tree-node-register current))
			      (undo-tree-register-data-p
			       (setq r (registerv-data (get-register r))))
			      (eq current (undo-tree-register-data-node r)))
			 ;; saved state
			 (and (or (null x) (eq x 'saved))
			      (undo-tree-node-unmodified-p current))
			 ))))
    ;; update diff display, if any
    (when diff
      (undo-tree-visualizer-show-diff
       (when undo-tree-visualizer-selection-mode
	 undo-tree-visualizer-selected-node)))))


(defun undo-tree-visualizer-toggle-timestamps ()
  "Toggle display of time-stamps."
  (interactive)
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (setq undo-tree-visualizer-timestamps (not undo-tree-visualizer-timestamps))
  (setq undo-tree-visualizer-spacing (undo-tree-visualizer-calculate-spacing))
  ;; redraw tree
  (let ((inhibit-read-only t)) (undo-tree-draw-tree buffer-undo-tree)))


(defun undo-tree-visualizer-scroll-left (&optional arg)
  (interactive "p")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (scroll-left (or arg 1) t))


(defun undo-tree-visualizer-scroll-right (&optional arg)
  (interactive "p")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (scroll-right (or arg 1) t))


(defun undo-tree-visualizer-scroll-up (&optional arg)
  (interactive "P")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (if (or (and (numberp arg) (< arg 0)) (eq arg '-))
      (undo-tree-visualizer-scroll-down arg)
    ;; scroll up and expand newly-visible portion of tree
    (unwind-protect
	(scroll-up-command arg)
      (undo-tree-expand-down
       (nth (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
	    (undo-tree-node-next (undo-tree-current buffer-undo-tree)))))
    ;; signal error if at eob
    (when (and (not undo-tree-visualizer-needs-extending-down) (eobp))
      (scroll-up))))


(defun undo-tree-visualizer-scroll-down (&optional arg)
  (interactive "P")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (if (or (and (numberp arg) (< arg 0)) (eq arg '-))
      (undo-tree-visualizer-scroll-up arg)
    ;; ensure there's enough room at top of buffer to scroll
    (let ((scroll-lines
	   (or arg (- (window-height) next-screen-context-lines)))
	  (window-line (1- (line-number-at-pos (window-start)))))
      (when (and undo-tree-visualizer-needs-extending-up
		 (< window-line scroll-lines))
	(let ((inhibit-read-only t))
	  (goto-char (point-min))
	  (undo-tree-move-up (- scroll-lines window-line)))))
    ;; scroll down and expand newly-visible portion of tree
    (unwind-protect
	(scroll-down-command arg)
      (undo-tree-expand-up
       (undo-tree-node-previous (undo-tree-current buffer-undo-tree))))
    ;; signal error if at bob
    (when (and (not undo-tree-visualizer-needs-extending-down) (bobp))
      (scroll-down))))




;;; =====================================================================
;;;                 Visualizer selection mode commands

(defun undo-tree-visualizer-select-previous (&optional arg)
  "Move to previous node."
  (interactive "p")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (let ((node undo-tree-visualizer-selected-node))
    (catch 'top
      (dotimes (_ (or arg 1))
	(unless (undo-tree-node-previous node) (throw 'top t))
	(setq node (undo-tree-node-previous node))))
    ;; when using lazy drawing, extend tree upwards as required
    (when undo-tree-visualizer-lazy-drawing
      (undo-tree-expand-up undo-tree-visualizer-selected-node node))
    ;; update diff display, if any
    (when (and undo-tree-visualizer-diff
	       (not (eq node undo-tree-visualizer-selected-node)))
      (undo-tree-visualizer-update-diff node))
    ;; move to selected node
    (goto-char (undo-tree-node-marker node))
    (setq undo-tree-visualizer-selected-node node)))


(defun undo-tree-visualizer-select-next (&optional arg)
  "Move to next node."
  (interactive "p")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (let ((node undo-tree-visualizer-selected-node))
    (catch 'bottom
      (dotimes (_ (or arg 1))
	(unless (nth (undo-tree-node-branch node) (undo-tree-node-next node))
	  (throw 'bottom t))
	(setq node
	      (nth (undo-tree-node-branch node) (undo-tree-node-next node)))))
    ;; when using lazy drawing, extend tree downwards as required
    (when undo-tree-visualizer-lazy-drawing
      (undo-tree-expand-down undo-tree-visualizer-selected-node node))
    ;; update diff display, if any
    (when (and undo-tree-visualizer-diff
	       (not (eq node undo-tree-visualizer-selected-node)))
      (undo-tree-visualizer-update-diff node))
    ;; move to selected node
    (goto-char (undo-tree-node-marker node))
    (setq undo-tree-visualizer-selected-node node)))


(defun undo-tree-visualizer-select-right (&optional arg)
  "Move right to a sibling node."
  (interactive "p")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (let ((node undo-tree-visualizer-selected-node)
	end)
    (goto-char (undo-tree-node-marker undo-tree-visualizer-selected-node))
    (setq end (line-end-position))
    (catch 'end
      (dotimes (_ arg)
	(while (or (null node) (eq node undo-tree-visualizer-selected-node))
	  (forward-char)
	  (setq node (get-text-property (point) 'undo-tree-node))
	  (when (= (point) end) (throw 'end t)))))
    (goto-char (undo-tree-node-marker
		(or node undo-tree-visualizer-selected-node)))
    (when (and undo-tree-visualizer-diff node
	       (not (eq node undo-tree-visualizer-selected-node)))
      (undo-tree-visualizer-update-diff node))
    (when node (setq undo-tree-visualizer-selected-node node))))


(defun undo-tree-visualizer-select-left (&optional arg)
  "Move left to a sibling node."
  (interactive "p")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (let ((node (get-text-property (point) 'undo-tree-node))
	beg)
    (goto-char (undo-tree-node-marker undo-tree-visualizer-selected-node))
    (setq beg (line-beginning-position))
    (catch 'beg
      (dotimes (_ arg)
	(while (or (null node) (eq node undo-tree-visualizer-selected-node))
	  (backward-char)
	  (setq node (get-text-property (point) 'undo-tree-node))
	  (when (= (point) beg) (throw 'beg t)))))
    (goto-char (undo-tree-node-marker
		(or node undo-tree-visualizer-selected-node)))
    (when (and undo-tree-visualizer-diff node
	       (not (eq node undo-tree-visualizer-selected-node)))
      (undo-tree-visualizer-update-diff node))
    (when node (setq undo-tree-visualizer-selected-node node))))


(defun undo-tree-visualizer-select (pos)
  (let ((node (get-text-property pos 'undo-tree-node)))
    (when node
      ;; select node at POS
      (goto-char (undo-tree-node-marker node))
      ;; when using lazy drawing, extend tree up and down as required
      (when undo-tree-visualizer-lazy-drawing
	(undo-tree-expand-up undo-tree-visualizer-selected-node node)
	(undo-tree-expand-down undo-tree-visualizer-selected-node node))
      ;; update diff display, if any
      (when (and undo-tree-visualizer-diff
		 (not (eq node undo-tree-visualizer-selected-node)))
	(undo-tree-visualizer-update-diff node))
      ;; update selected node
      (setq undo-tree-visualizer-selected-node node)
      )))


(defun undo-tree-visualizer-mouse-select (pos)
  "Select undo tree node at mouse event POS."
  (interactive "@e")
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (undo-tree-visualizer-select (event-start (nth 1 pos))))




;;; =====================================================================
;;;                      Visualizer diff display

(defun undo-tree-visualizer-toggle-diff ()
  "Toggle diff display in undo-tree visualizer."
  (interactive)
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (if undo-tree-visualizer-diff
      (undo-tree-visualizer-hide-diff)
    (undo-tree-visualizer-show-diff)))


(defun undo-tree-visualizer-selection-toggle-diff ()
  "Toggle diff display in undo-tree visualizer selection mode."
  (interactive)
  (unless (eq major-mode 'undo-tree-visualizer-mode)
    (user-error "Undo-tree mode not enabled in buffer"))
  (if undo-tree-visualizer-diff
      (undo-tree-visualizer-hide-diff)
    (let ((node (get-text-property (point) 'undo-tree-node)))
      (when node (undo-tree-visualizer-show-diff node)))))


(defun undo-tree-visualizer-show-diff (&optional node)
  ;; show visualizer diff display
  (setq undo-tree-visualizer-diff t)
  (let ((buff (with-current-buffer undo-tree-visualizer-parent-buffer
		(undo-tree-diff node)))
	(display-buffer-mark-dedicated 'soft)
	win)
    (setq win (split-window))
    (set-window-buffer win buff)
    (shrink-window-if-larger-than-buffer win)))


(defun undo-tree-visualizer-hide-diff ()
  ;; hide visualizer diff display
  (setq undo-tree-visualizer-diff nil)
  (let ((win (get-buffer-window undo-tree-diff-buffer-name)))
    (when win (with-selected-window win (kill-buffer-and-window)))))


(defun undo-tree-diff (&optional node)
  ;; Create diff between NODE and current state (or previous state and current
  ;; state, if NODE is null). Returns buffer containing diff.
  (let (tmpfile buff)
    ;; generate diff
    (let ((undo-tree-inhibit-kill-visualizer t)
	  (current (undo-tree-current buffer-undo-tree)))
      (undo-tree-set (or node (undo-tree-node-previous current) current)
		     'preserve-timestamps)
      (setq tmpfile (diff-file-local-copy (current-buffer)))
      (undo-tree-set current 'preserve-timestamps))
    (setq buff (diff-no-select
		tmpfile (current-buffer) nil 'noasync
		(get-buffer-create undo-tree-diff-buffer-name)))
    ;; delete process messages and useless headers from diff buffer
    (let ((inhibit-read-only t))
      (with-current-buffer buff
	(goto-char (point-min))
	(delete-region (point) (1+ (line-end-position 3)))
	(goto-char (point-max))
	(forward-line -2)
	(delete-region (point) (point-max))
	(setq cursor-type nil)
	(setq buffer-read-only t)))
    buff))


(defun undo-tree-visualizer-update-diff (&optional node)
  ;; update visualizer diff display to show diff between current state and
  ;; NODE (or previous state, if NODE is null)
  (with-current-buffer undo-tree-visualizer-parent-buffer
    (undo-tree-diff node))
  (let ((win (get-buffer-window undo-tree-diff-buffer-name)))
    (when win
      (balance-windows)
      (shrink-window-if-larger-than-buffer win))))



(provide 'undo-tree)

;;; undo-tree.el ends here
