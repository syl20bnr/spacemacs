# RCIRC contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [RCIRC contribution layer for Spacemacs](#rcirc-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Features](#features)
    - [Install](#install)
        - [Layer](#layer)
        - [Configuration](#configuration)
        - [Plain rcirc configuration](#plain-rcirc-configuration)
        - [Authenticate with authinfo](#authenticate-with-authinfo)
        - [Use a ZNC bouncer](#use-a-znc-bouncer)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer provide support for [rcirc][] with optional support for authinfo
and ZNC.

## Features

- Store channel logs into `~/.emacs.d/.cache/rcirc-logs/<channel>`
- Support for credentials stored in `~/.authinfo.gpg` (need to have gnutls) 
- Support ZNC support (with optional `~/.authinfo.gpg`)
- Colored nicknames
- WIP: Real time change when people use /s/foo/bar in the chats

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(rcirc))
```

### Configuration

There are 3 possible ways to configure the layer depending on which feature
you are using.

### Plain rcirc configuration

You can read the rcirc manual [here][rcirc].

Example of configuration you can store in the `dotspacemacs/config` of your
dotfile:

```elisp
(setq rcirc-server-alist
  '(("irc.freenode.net"
      :user "spacemacs_user"
      :port "1337"
      :password "le_passwd"
      :channels ("#emacs"))))
```

### Authenticate with authinfo

1. If you want to use authinfo.gpg you have to enable the support for it
by setting `rcirc-enable-authinfo-support` to `t` in your dotfile:

```elisp
(setq-default dotspacemacs-configuration-layers '(
  (rcirc :variables rcirc-enable-authinfo-support t)))
```

2. In your `~/.authinfo.gpg` file store your credentials like this:
```
machine irc.freenode.net port nickserv user <user> password <passwd>
```

3. At last you need to provide your servers configuration in the
`dotspacemacs/config` function of your dotfile:

```elisp
(setq rcirc-server-alist
  '(("irc.freenode.net"
      :user "spacemacs_user"
      :port "1337"
      :channels ("#emacs"))))
  ```

### Use a ZNC bouncer

**Disclaimer**
This assumes that you are familiar with ZNC and you have a bouncer properly
setup. If it is not the case then it is recommended to read about ZNC
[here][znc]. There is also an installation guide for ubuntu [here][znc guide].

**Note**
For now authinfo is mandatory to use the ZNC configuration.

1. To enable ZNC support set the variable `rcirc-enable-znc-support` to `t` in
your dotfile:

```elisp
(setq-default dotspacemacs-configuration-layers '(
  (rcirc :variables rcirc-enable-authinfo-support t)))
```

2. In your `~/.authinfo.gpg` file store your credentials like this:

```
machine freenode.spacemacsserver.me port irc user spacemacs_user/freenode password ZNC_PASSWORD
machine geekshed.spacemacsserver.me port irc user spacemacs_user/geekshed password ZNC_PASSWORD
```

**Important** `port` must be set to `irc`. This is a convention of the layer
to retrieve the credentials for the ZNC configuration.

3. Then setup your servers configuration using subdomains in the
`dotspacemacs/config` function of your dotfile. The `:auth` keyword arguments
will be replaced by the credentials stored in your `~/.authinfo.gpg`. 

```elisp
(setq rcirc-server-alist
 ;; This will replace :auth with the correct thing, see the doc for that function
 '(("freenode"
     :host "freenode.spacemacsserver.me"
     :port "1337"
     :auth "spacemacs_user/freenode"
     :channels ("#emacs"))
 ("geekshed"
     :host "geekshed.spacemacsserver.me"
     :port "1337"
     :auth "spacemacs_user/geekshed"
     :channels ("#jupiterbroadcasting"))))
```

## Key Bindings

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC a i</kbd>    | Open rcirc
<kbd>SPC L o i</kbd>  | Open rcirc in a custom perspective "@RICRC" (need perspectives layer enabled)

[rcirc]: http://www.gnu.org/software/emacs/manual/html_mono/rcirc.html
[znc]: http://wiki.znc.in/ZNC
[znc guide]: https://www.digitalocean.com/community/tutorials/how-to-install-znc-an-irc-bouncer-on-an-ubuntu-vps
[source]: http://git.tapoueh.org/?p=emacs.d.git
