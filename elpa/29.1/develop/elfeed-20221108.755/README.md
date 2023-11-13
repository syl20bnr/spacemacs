# Elfeed Emacs Web Feed Reader

Elfeed is an extensible web feed reader for Emacs, supporting both
Atom and RSS. It requires Emacs 24.3 and is available for download
from [MELPA](http://melpa.milkbox.net/) or
[el-get](https://github.com/dimitri/el-get). Elfeed was inspired by
[notmuch](http://notmuchmail.org/).

For a longer overview,

 * [Introducing Elfeed, an Emacs Web Feed Reader](http://nullprogram.com/blog/2013/09/04/).
 * [Tips and Tricks](http://nullprogram.com/blog/2013/11/26/)
 * [Read your RSS feeds in Emacs with Elfeed
](http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/)
 * [Scoring Elfeed articles](http://kitchingroup.cheme.cmu.edu/blog/2017/01/05/Scoring-elfeed-articles/)
 * [Using Emacs 29](https://www.youtube.com/watch?v=pOFqzK1Ymr4),
   [30](https://www.youtube.com/watch?v=tjnK1rkO7RU),
   [31](https://www.youtube.com/watch?v=5zuSUbAHH8c)
 * [Take Elfeed everywhere: Mobile rss reading Emacs-style (for free/cheap)](http://babbagefiles.blogspot.com/2017/03/take-elfeed-everywhere-mobile-rss.html)
 * [Elfeed Rules!](https://noonker.github.io/posts/2020-04-22-elfeed/) ([reddit](https://old.reddit.com/r/emacs/comments/g6oowz/elfeed_rules/))
 * [Elfeed with Tiny Tiny RSS](https://codingquark.com/emacs/2020/04/19/elfeed-protocol-ttrss.html) ([hn](https://news.ycombinator.com/item?id=22915200))
 * [Open Emacs elfeed links in the background](http://xenodium.com/open-emacs-elfeed-links-in-background/)
 * [Using Emacs 72](https://cestlaz.github.io/post/using-emacs-72-customizing-elfeed/)
 * [Lazy Elfeed](https://karthinks.com/blog/lazy-elfeed/)
 * [Using Elfeed to View Videos](https://joshrollinswrites.com/help-desk-head-desk/20200611/)
 * [Manage podcasts in Emacs with Elfeed and Bongo](https://protesilaos.com/codelog/2020-09-11-emacs-elfeed-bongo/)
 * [... more ...](http://nullprogram.com/tags/elfeed/)
 * [... and more ...](http://pragmaticemacs.com/category/elfeed/)

[![](http://i.imgur.com/kxgF5AH.png)](http://i.imgur.com/kxgF5AH.png)

The database format is stable and is never expected to change.

## Prerequisites

**It is *strongly* recommended you have cURL installed**, either in
your PATH or configured via `elfeed-curl-program-name`. Elfeed will
prefer it to Emacs' own URL-fetching mechanism, `url-retrieve`. It's
also essential for running Elfeed on Windows, where `url-retrieve` is
broken. Updates using cURL are significantly faster than the built-in
method, both for you and the feed hosts.

If this is giving you problems, fetching with cURL can be disabled by
setting `elfeed-use-curl` to nil.

## Extensions

These projects extend Elfeed with additional features:

* [elfeed-org](https://github.com/remyhonig/elfeed-org)
* [elfeed-goodies](https://github.com/algernon/elfeed-goodies)
* [elfeed-protocol](https://github.com/fasheng/elfeed-protocol)
* [elfeed-score](https://github.com/sp1ff/elfeed-score)
* [Elfeed Android interface](https://github.com/areina/elfeed-cljsrn)
  ([Google Play](https://play.google.com/store/apps/details?id=com.elfeedcljsrn))
* [elfeed-dashboard](https://github.com/Manoj321/elfeed-dashboard)

## Getting Started

Elfeed is broken into a multiple source files, so if you manually
install it you will need to add the Elfeed package directory to your
`load-path`. If installed via package.el or el-get, this will be done
automatically.

It is recommended that you make a global binding for `elfeed`.

```el
(global-set-key (kbd "C-x w") 'elfeed)
```

Running the interactive function `elfeed` will pop up the
`*elfeed-search*` buffer, which will display feed items.

 * <kbd>g</kbd>: refresh view of the feed listing
 * <kbd>G</kbd>: fetch feed updates from the servers
 * <kbd>s</kbd>: update the search filter (see tags)
 * <kbd>c</kbd>: clear the search filter

This buffer will be empty until you add your feeds to the
`elfeed-feeds` list and initiate an update with `M-x elfeed-update`
(or <kbd>G</kbd> in the Elfeed buffer). This will populate the Elfeed
database with entries.

```el
;; Somewhere in your .emacs file
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "https://planet.emacslife.com/atom.xml"))
```

Another option for providing a feed list is with an OPML file. Running
`M-x elfeed-load-opml` will fill `elfeed-feeds` with feeds listed in
an OPML file. When `elfeed-load-opml` is called interactively, it will
automatically save the feedlist to your customization file, so you
will only need to do this once.

If there are a lot of feeds, the initial update will take noticeably
longer than normal operation because of the large amount of
information being written the database. Future updates will only need
to write new or changed data. If updating feeds slows down Emacs too
much for you, reduce the number of concurrent fetches via
`elfeed-set-max-connections`.

If you're getting many "Queue timeout exceeded" errors, increase the
fetch timeout via `elfeed-set-timeout`.

~~~el
(setf url-queue-timeout 30)
~~~

From the search buffer there are a number of ways to interact with
entries. Entries are selected by placing the point over an entry.
Multiple entries are selected at once by using an active region.

 * <kbd>RET</kbd>: view selected entry in a buffer
 * <kbd>b</kbd>: open selected entries in your browser (`browse-url`)
 * <kbd>y</kbd>: copy selected entries URL to the clipboard
 * <kbd>r</kbd>: mark selected entries as read
 * <kbd>u</kbd>: mark selected entries as unread
 * <kbd>+</kbd>: add a specific tag to selected entries
 * <kbd>-</kbd>: remove a specific tag from selected entries

## Tags

Elfeed maintains a list of arbitrary tags -- symbols attached to an
entry. The tag `unread` is treated specially by default, with unread
entries appearing in bold.

### Autotagging

Tags can automatically be applied to entries discovered in specific
feeds through extra syntax in `elfeed-feeds`. Normally this is a list
of strings, but an item can also be a list, providing set of
"autotags" for a feed's entries.

```el
(setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        "http://www.50ply.com/atom.xml"  ; no autotagging
        ("http://nedroid.com/feed/" webcomic)))
```

### Filter Syntax

To make tags useful, the Elfeed entry listing buffer can be filtered
by tags. Use `elfeed-search-set-filter` (or <kbd>s</kbd>) to update
the filter. Use `elfeed-search-clear-filter` to restore the default.

Any component of the search string beginning with a `+` or
a `-` is treated like a tag. `+` means the tag is required, `-` means
the tag must not be present.

A component beginning with a `@` indicates an age or a date range. An
age is a relative time expression or an absolute date expression.
Entries older than this age are filtered out. The age description
accepts plain English, but cannot have spaces, so use dashes. For
example, `"@2-years-old"`, `"@3-days-ago"` or `"@2019-06-24"`. A date
range are two ages seperated by a `--`, e.g.
`"@2019-06-20--2019-06-24"` or `"@5-days-ago--1-day-ago"`. The entry
must be newer than the first expression but older than the second. The
database is date-oriented, so **filters that include an age
restriction are significantly more efficient.**

A component beginning with a `!` is treated as an "inverse" regular
expression. This means that any entry matching this regular expression
will be filtered out. The regular expression begins *after* the `!`
character. You can read this as "entry not matching `foo`".

A component beginning with a `#` limits the total number of entries
displayed to the number immediately following the symbol. For example,
to limit the display to 20 entries: `#20`.

A component beginning with a `=` is a regular expression matching the
entry's feed (title or URL). Only entries belonging to a feed that
matches at least one of the `=` expressions will be shown.

A component beginning with a `~` is a regular expression matching the
entry's feed (title or URL). Only entries belonging to a feed that
matches none of the `~` expressions will be shown.

All other components are treated as a regular expression, and only
entries matching it (title or URL) will be shown.

Here are some example filters.

 * `@6-months-ago +unread`

Only show unread entries of the last six months. This is the default filter.

 * `linu[xs] @1-year-old`

Only show entries about Linux or Linus from the last year.

 * `-unread +youtube #10`

Only show the most recent 10 previously-read entries tagged as
`youtube`.

 * `+unread !x?emacs`

Only show unread entries not having `emacs` or `xemacs` in the title
or link.

* `+emacs =http://example.org/feed/`

Only show entries tagged as `emacs` from a specific feed.

#### Default Search Filter

You can set your default search filter by changing the default value
of `elfeed-search-filter`. It only changes buffer-locally when you're
adjusting the filter within Elfeed. For example, some users prefer to
have a space on the end for easier quick searching.

    (setq-default elfeed-search-filter "@1-week-ago +unread ")

### Tag Hooks

The last example assumes you've tagged posts with `youtube`. You
probably want to do this sort of thing automatically, either through
the "autotags" feature mentioned above, or with the
`elfeed-new-entry-hook`. Functions in this hook are called with new
entries, allowing them to be manipulated, such as adding tags.

```el
;; Mark all YouTube entries
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))
```

Avoiding tagging old entries as `unread`:

```el
;; Entries older than 2 weeks are marked as read
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))
```

Or building your own subset feeds:

```el
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "example\\.com"
                              :entry-title '(not "something interesting")
                              :add 'junk
                              :remove 'unread))
```

Use `M-x elfeed-apply-hooks-now` to apply `elfeed-new-entry-hook` to
all existing entries. Otherwise hooks will only apply to new entries
on discovery.

### Custom Tag Faces

By default, entries marked `unread` will have bolded titles in the
`*elfeed-search*` listing. You can customize how tags affect an
entry's appearance by customizing `elfeed-search-face-alist`. For
example, this configuration makes entries tagged `important` stand out
in red.

~~~el
(defface important-elfeed-entry
  '((t :foreground "#f77"))
  "Marks an important Elfeed entry.")

(push '(important important-elfeed-entry)
      elfeed-search-face-alist)
~~~

All faces from all tags will be applied to the entry title. The faces
will be ordered as they appear in `elfeed-search-face-alist`.

## Bookmarks

Filters can be saved and restored using Emacs' built-in [bookmarks
feature][bm]. While in the search buffer, use `M-x bookmark-set` to
save the current filter, and `M-x bookmark-jump` to restore a saved
filter. Emacs automatically persists bookmarks across sessions.

[bm]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Bookmarks.html

## Metadata Plist

All feed and entry objects have plist where you can store your own
arbitrary, [readable values][rd]. These values are automatically
persisted in the database. This metadata is accessed using the
polymorphic `elfeed-meta` function. It's setf-able.

~~~el
(setf (elfeed-meta entry :rating) 4)
(elfeed-meta entry :rating)
;; => 4

(setf (elfeed-meta feed :title) "My Better Title")
~~~

Elfeed itself adds some entries to this plist, some for your use, some
for its own use. Here are the properties that Elfeed uses:

* `:authors` : A list of author plists (`:name`, `:uri`, `:email`).
* `:canonical-url` : The final URL for the feed after all redirects.
* `:categories` : The feed-supplied categories for this entry.
* `:etag` : HTTP Etag header, for conditional GETs.
* `:failures` : Number of times this feed has failed to update.
* `:last-modified` : HTTP Last-Modified header, for conditional GETs.
* `:title` : Overrides the feed-supplied title for display purposes,
  both for feeds and entries. See also `elfeed-search-set-feed-title`
  and `elfeed-search-set-entry-title`.

This list will grow in time, so you might consider namespacing your
own properties to avoid collisions (e.g. `:xyz/rating`), or simply not
using keywords as keys. Elfeed will always use keywords without a
slash.

[rd]: http://nullprogram.com/blog/2013/12/30/

## Hooks

A number of hooks are available to customize the behavior of Elfeed at
key points without resorting to advice.

* `elfeed-new-entry-hook` : Called each time a new entry it added to
  the database, allowing for automating tagging and such.
* `elfeed-new-entry-parse-hook` : Called with each new entry and the
  full XML structure from which it was parsed, allowing for additional
  information to be drawn from the original feed XML.
* `elfeed-http-error-hooks` : Allows for special behavior when HTTP
  errors occur, beyond simply logging the error to `*elfeed-log*` .
* `elfeed-parse-error-hooks` : Allows for special behavior when feed
  parsing fails, beyond logging.
* `elfeed-db-update-hook` : Called any time the database has had a
  major modification.

## Viewing Entries

Entries are viewed locally in Emacs by typing `RET` while over an
entry in the search listing. The content will be displayed in a
separate buffer using `elfeed-show-mode`, rendered using Emacs'
built-in shr package. This requires an Emacs compiled with `libxml2`
bindings, which provides the necessary HTML parser.

Sometimes displaying images can slow down or even crash Emacs. Set
`shr-inhibit-images` to disable images if this is a problem.

## Web Interface

Elfeed includes a demonstration/toy web interface for remote network
access. It's a single-page web application that follows the database
live as new entries arrive. It's packaged separately as `elfeed-web`.
To fire it up, run `M-x elfeed-web-start` and visit
http://localhost:8080/elfeed/ (check your `httpd-port`) with a
browser. See the `elfeed-web.el` header for endpoint documentation if
you'd like to access the Elfeed database through the web API.

It's rough and unfinished -- no keyboard shortcuts, read-only, no
authentication, and a narrow entry viewer. This is basically Elfeed's
"mobile" interface. Patches welcome.

## Platform Support

Summary: Install cURL and most problems disappear for all platforms.

I personally only use Elfeed on Linux, but it's occasionally tested on
Windows. Unfortunately the Windows port of Emacs is a bit too unstable
for parallel feed downloads with `url-retrieve`, not to mention the
[tiny, hard-coded, 512 open descriptor limitation][files], so it
limits itself to one feed at a time on this platform.

[files]: http://msdn.microsoft.com/en-us/library/kdfaxaay%28vs.71%29.aspx

If you fetch HTTPS feeds without cURL on *any* platform, it's
essential that Emacs is built with the `--with-gnutls` option.
Otherwise Emacs runs gnutls in an inferior process, which rarely works
well.

## Database Management

The database should keep itself under control without any manual
intervention, but steps can be taken to minimize the database size if
desired. The simplest option is to run the `elfeed-db-compact`
command, which will pack the loose-file content database into a single
compressed file. This function works well in `kill-emacs-hook`.

Going further, a function could be added to `elfeed-new-entry-hook` to
strip unwanted/unneeded content from select entries before being
stored in the database. For example, for YouTube videos only the entry
link is of interest and the regularly-changing entry content could be
tossed to save time and storage.

## Status and Roadmap

Elfeed is to the point where it can serve 100% of my own web feed
needs. My personal selection of about 150 feeds has been acting as my
test case as I optimize and add features.

Some things I still might want to add:

* Database synchronization between computers
* Parallel feed fetching via separate Emacs subprocesses

## Motivation

As far as I know, outside of Elfeed there does not exist an
extensible, text-file configured, power-user web feed client that can
handle a reasonable number of feeds. The existing clients I've tried
are missing some important capability that limits its usefulness to
me.
