# Search Engine contribution layer for Spacemacs

![logo_searchengine](img/searchengine.jpg)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Search Engine contribution layer for Spacemacs](#search-engine-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->
    
## Description

This layer adds support for the [Search Engine][] package.

Supported search engines:

- [Amazon][]
- [Duck Duck Go][]
- [Google][]
- [Google Images][]
- [GitHub][]
- [Google Maps][]
- [Twitter][]
- [Project Gutemberg][]
- [Youtube][]
- [Stack Overflow][]
- [Spacemacs Issues][]
- [Wikipedia][]
- [Wolfram Alpha][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(search-engine))
```
    
## Key Bindings

      Evil         |         Holy         |                Command
-------------------|----------------------|------------------------------------
<kbd>SPC a /</kbd> |   <kbd>C-c / </kbd>  |    Summon a Helm buffer to select any engine

## Customize it!

If you'd rather have emacs use chrome, or firefox or any other thing (`eww`) you
can have that customization. For example for google chrome you can put this in
your `dotspacemacs/config`:

```elisp
  (setq browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome")
```

Also if you want more search engines, just push them (do this in `dotspacemacs/config`)


```elisp
(push '(custom1
         :name "Custom Search Engine 1"
         :url "http://www.domain.com/s/stuff_sutff_remember_to_replace_search_candidate_with_%s")
        extra-search-engines-alist)

```


If you'd rather not use helm but would want an specific search engine, remember
the function generated is always `engine/search-(the name of the search engine
lower-case and hyphen instead-of-spaces-for-separation)` so you can bind that to any key binding you want

[Search Engine]: https://github.com/hrs/engine-mode/engine-mode.el
[Amazon]: http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s
[Duck Duck Go]: https://duckduckgo.com/?q=%s
[Google]: http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s
[Google Images]: http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s
[Github]: https://github.com/search?ref=simplesearch&q=%s
[Google Maps]: http://maps.google.com/maps?q=%s
[Twitter]: https://twitter.com/search?q=%s
[Project Gutemberg]: http://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=&query=%s
[Youtube]: http://www.youtube.com/results?aq=f&oq=&search_query=%s
[Stack Overflow]: https://stackoverflow.com/search?q=%s
[Spacemacs Issues]: https://github.com/syl20bnr/spacemacs/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+%s
[Wikipedia]: http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s
[Wolfram Alpha]: http://www.wolframalpha.com/input/?i=%s
