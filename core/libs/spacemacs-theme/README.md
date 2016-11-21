# Spacemacs-theme

[![MELPA](http://melpa.org/packages/spacemacs-theme-badge.svg)](http://melpa.org/#/spacemacs-theme) ![Made with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)
 [![Join the chat at https://gitter.im/nashamri/spacemacs-theme](https://badges.gitter.im/Join%20Chat.svg)] (https://gitter.im/nashamri/spacemacs-theme?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

![spacemacs-theme](/../screenshots/spacemacs-theme.png)

Spacemacs theme is an Emacs color theme that started as a theme for [spacemacs](https://github.com/syl20bnr/spacemacs).
The theme comes with dark and light variants and it should work well with 256 color terminals.

## Screenshots

![spacemacs-theme-preview](/../screenshots/preview.png)

## Highlights

The theme has good support for org mode.

![spacemacs-theme-org](/../screenshots/org.png)

## Installation

You can install it from MELPA by:

```
M-x package-install RET spacemacs-theme
```

## Supported modes

Some of the supported modes are:

* company
* ein
* erc
* gnus
* helm
* ido
* info
* magit
* neotree
* org
* and others :) more are coming!

## Customizations

The theme has some options that can be tweaked via `M-x customize`:

* `spacemacs-theme-comment-bg`:

This toggles a background color for the comment lines.

* `spacemacs-theme-org-agenda-height`:

This toggles the use of varying org agenda heights.

* `spacemacs-theme-org-height`:

This toggles the use of varying org headings heights.

* `spacemacs-theme-org-highlight`:

This toggles highlighting of org headings.

* `spacemacs-theme-custom-colors`:

This allows for specifying a list of custom colors to override spacemacs theme colors. More details in the next section.

### Override theme's colors

The theme can be customized by overriding one of the theme local variables by setting a list in the `spacemacs-theme-custom-colors` variable.
Here's a list of all the local variables and roles:

| var           | role                                                                                              |
|---------------|---------------------------------------------------------------------------------------------------|
| act1          | One of mode-line's active colors.                                                                 |
| act2          | The other active color of mode-line.                                                              |
| base          | The basic color of normal text.                                                                   |
| base-dim      | A dimmer version of the normal text color.                                                        |
| bg1           | The background color.                                                                             |
| bg2           | A darker background color. Used to highlight current line.                                        |
| bg3           | Yet another darker shade of the background color.                                                 |
| bg4           | The darkest background color.                                                                     |
| border        | A border line color. Used in mode-line borders.                                                   |
| cblk          | A code block color. Used in org's code blocks.                                                    |
| cblk-bg       | The background color of a code block.                                                             |
| cblk-ln       | A code block header line.                                                                         |
| cblk-ln-bg    | The background of a code block header line.                                                       |
| cursor        | The cursor/point color.                                                                           |
| const         | A constant.                                                                                       |
| comment       | A comment.                                                                                        |
| comment-bg    | The background color of a comment. To disable this, `customize` `spacemacs-theme-comment-bg`.     |
| comp          | A complementary color.                                                                            |
| err           | errors.                                                                                           |
| func          | functions.                                                                                        |
| head1         | Level 1 of a heading. Used in org's headings.                                                     |
| head1-bg      | The background of level 2 headings. To disable this, `customize` `spacemacs-theme-org-highlight`. |
| head2         | Level 2 headings.                                                                                 |
| head2-bg      | Level 2 headings background.                                                                      |
| head3         | Level 3 headings.                                                                                 |
| head3-bg      | Level 3 headings background.                                                                      |
| head4         | Level 4 headings.                                                                                 |
| head4-bg      | Level 4 headings background.                                                                      |
| highlight     | A highlighted area.                                                                               |
| highlight-dim | A dimmer highlighted area.                                                                        |
| keyword       | A keyword or a builtin color.                                                                     |
| lnum          | Line numbers.                                                                                     |
| mat           | A matched color. Used in matching parens, brackets and tags.                                      |
| meta          | A meta line. Used in org's meta line.                                                             |
| str           | A string.                                                                                         |
| suc           | To indicate success. Opposite of error.                                                           |
| ttip          | Tooltip color.                                                                                    |
| ttip-sl       | Tooltip selection color.                                                                          |
| ttip-bg       | Tooltip background color.                                                                         |
| type          | A type color.                                                                                     |
| var           | A variable color.                                                                                 |
| war           | A warning color.                                                                                  |


There is also explicit colors variables that can be customized:

* aqua
* aqua-bg
* green
* green-bg
* green-bg-s
* cyan
* red
* red-bg
* red-bg-s
* blue
* blue-bg
* violet
* yellow
* yellow-bg

The `green` and `red` colors have two background versions. The `green-bg` and  `red-bg` are normal light background colors.
The `green-bg-s` and `red-bg-s` are a stronger version and are used in `ediff` and places were text is added or deleted.

Here are some screenshots of the various variables:

![spacemacs-theme-guide-generic](/../screenshots/guide-generic.png)

![spacemacs-theme-guide-org](/../screenshots/guide-org.png)

![spacemacs-theme-guide-extra](/../screenshots/guide-extra.png)

If you are using [spacemacs](https://github.com/syl20bnr/spacemacs), you can put this snippet in your `dotspacemacs/user-init` to override these colors:

```
  (custom-set-variables '(spacemacs-theme-custom-colors
                          '((act1 . "#ff0000")
                            (act2 . "#0000ff")
                            (base . "#ffffff"))))
```

This will override `act1`, `act1` and `base` to use the specified colors.

# Like the theme and want to use it in other places?

Then check out this project [base16-builder](https://github.com/auduchinok/base16-builder).
