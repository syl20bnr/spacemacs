1 SVG Library
═════════════

  svg-lib is an Emacs library that allows to create and display various
  SVG objects, namely tags, icons, buttons, progress bars, progress pies
  and dates. Each object is guaranteed to fit nicely in a text buffer
  ensuring width is a multiple of character width and height a multiple
  of character height.


2 Installation
══════════════

  ┌────
  │ M-x package-install RET svg-lib RET
  └────


3 Quick start
═════════════

  For the impatient, evaluate the following expression:

  ┌────
  │ M-: (insert-image (svg-lib-tag "TODO"))
  └────

  This should insert a SVG tag displaying "TODO" in a rounded box whose
  size is exactly 5 characters wide (because of padding).


4 Usage
═══════

4.1 Objects
───────────

  • `svg-lib-tag LABEL'
  • `svg-lib-icon ICON'
  • `svg-lib-icon+tag ICON LABEL'
  • `svg-lib-button LABEL HOOK'
  • `svg-lib-progress-bar VALUE'
  • `svg-lib-progress-pie VALUE'
  • `svg-lib-date DATE'


4.2 Styling
───────────

  Each library object can be styled using a style property list that
  defines:

  • foreground color
  • background color
  • internal padding (tag and icon)
  • external margin (in char)
  • stroke width (in pixels)
  • corner radius (in pixels) for the rounded box
  • horizontal alignment (0 to 1) inside margins
  • width (in characters)
  • height as a scale of line height
  • scale (for icon)
  • ascent (for text)
  • crop-left to crop object on the left
  • crop-right to crop object on the right
  • collection to use for icon
  • font family
  • font size
  • font weight


5 Icon repositories
═══════════════════

  Icons can be created by parsing remote collections whose license are
  compatibles with GNU Emacs. The default size of an icon is exactly 2x1
  characters such that it can be inserted inside a text without
  disturbing alignment.

  Each icon is cached locally to speed-up loading the next time you use
  it. If for some reason the cache is corrupted you can force reload
  using the svg-icon-get-data function. If you want to add new
  collections (i.e. URL), make sure the icons are monochrome, that their
  size is consistent and that they include a 'viewBox' node.


5.1 [Material Design] (7447 icons)
──────────────────────────────────

  Open-source iconography for designers and developers.

  • Version: 7.4.47 (December 2023)
  • Licence: Apache 2.0
  • Number of icons: 7447
  • Sources: <https://github.com/Templarian/MaterialDesign>
  • Collection: `material'


[Material Design] <https://pictogrammers.com/library/mdi/>


5.2 [Simple icons] (2926 icons)
───────────────────────────────

  Over 2900 Free SVG icons for popular brands.

  • Version: 10.4.0 (December 2023)
  • Licence: CC0-1.0 license
  • Number of icons: 2926
  • Sources: <https://github.com/twbs/icons>
  • Collection: `simple'


[Simple icons] <https://simpleicons.org/>


5.3 [Bootstrap] (> 2000 icons)
──────────────────────────────

  Official open source SVG icon library for Bootstrap.

  • Version: 1.11.2 (December 2023)
  • Licence: MIT License
  • Number of icons: > 2000
  • Sources: <https://github.com/twbs/icons>
  • Collection: `bootstrap'


[Bootstrap] <https://icons.getbootstrap.com/>


5.4 [Boxicons] (1634 icons)
───────────────────────────

  Simple Open Source icons carefully crafted for designers & developers.

  • Version: 2.1.4 (September 2022)
  • Licence: MIT License
  • Number of icons: 1634
  • Sources: <https://github.com/atisawd/boxicons>.
  • Collection: `boxicons'


[Boxicons] <https://boxicons.com/>


5.5 [Octicons] (322 icons)
──────────────────────────

  Octicons are a set of SVG icons built by GitHub for GitHub.

  • Version: 19.8.0 (September 2023)
  • Licence: MIT License
  • Number of icons: 322
  • Sources: <https://github.com/primer/octicons>
  • Collection: `octicons'


[Octicons] <https://primer.style/octicons>


5.6 [VSCode] (209 icons)
────────────────────────

  Icons used in Visual Studio Code.

  • Version: (September 2020)
  • Licence: CC-BY 4.0
  • Number of icons: 209
  • Sources: <https://github.com/microsoft/vscode-icons>
  • Collection: `vscode'


[VSCode]
<https://www.figma.com/community/file/768673354734944365/visual-studio-code-icons>


6 Screenshots
═════════════

  <file:screenshot.png>

  <file:screenshot-2.png>
