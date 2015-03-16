# RCIRC contribution layer for Spacemacs

This will enable spacemacs users to have a basic configuration for RCIRC

## Features

* Logging into ~/.emacs.d/.cache/rcirc-logs/<channel>
* (Optional) No more passwords on your git repo!(security *improved*) by using
  credentials stored in ~/.authinfo.gpg (need to have gnutls) 
* Color nicknames
* ZNC ready (with ~/.authinfo.gpg)
* WIP. real time change when people use /s/foo/bar in the chats

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(rcirc))
```

## Key Bindings

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC a i</kbd>    | Open rcirc
<kbd>SPC P o i</kbd>  | Open rcirc in a custom perspective "@RICRC" (need perspectives layer enabled)

## Setting up

This method has three ways of connecting

### The normal way

Read the [manual](http://www.gnu.org/software/emacs/manual/html_mono/rcirc.html)
You may need to set up the rcirc like this

In `dotspacemacs/config`
```elisp
      (setq rcirc-server-alist
            '(("irc.freenode.net"
               :user "spacemacs_user"
               :port "1337"
               :password "le_passwd"
               :channels ("#emacs"))))
```

If you want to use authinfo.gpg you have to enable `rcirc-authinfo-support` in
your `dotspacemacs/init`.

In `dotspacemacs/init`
```elisp
(setq rcirc-authinfo-support t)
```

And have the following setup (you don't need to expose your password in your
configuration file anymore)

In `~/.authinfo.gpg`
```
machine irc.freenode.net port nickserv user spacemacs_user password le_passwd
```

In `dotspacemacs/config`
```elisp
(setq rcirc-server-alist
    '(("irc.freenode.net"
        :user "spacemacs_user"
        :port "1337"
        :channels ("#emacs"))))
```

### The ZNC way

DISCLAIMER: THIS IS ONLY VALID FOR ZNC USERS LEARN MORE ABOUT ZNC
[HERE](http://wiki.znc.in/ZNC) if you want to install it you can guide
yourself with
[this](https://www.digitalocean.com/community/tutorials/how-to-install-znc-an-irc-bouncer-on-an-ubuntu-vps)

If you use a ZNC server, like me, to use irc, then you know the common way of
doing your connection would be. 

```elisp
(setq rcirc-server-alist
    ;; You need different subdomains (freenode.spacemacsserver.me and w/e)
    ;; because rcirc can't tell apart if you have multiple hosts.
    ;; Although there is some code I copy pasted from http://git.tapoueh.org/?p=emacs.d.git
    ;; So if you understand this better than me, feel free to PR
    '(("freenode"
            :host "freenode.spacemacsserver.me"
            :port "1337"
            :password "spacemacs_user/freenode:super_secret_password" )
            :channels ("#emacs"))
        ("geekshed"
            :host "geekshed.spacemacsserver.me"
            :port "1337"
            :password "spacemacs_user/geekshed:super_secret_password")
            :channels ("#jupiterbroadcasting"))
```

The way to enable ZNC with authinfo support is to have this on your
`dotspacemacs/init` function.

In `dotspacemacs/init`
```elisp
(setq rcirc-enable-authinfo-support t
      rcirc-enable-znc-support t)
```

In `dotspacemacs/config`
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
This layer will replace the `:auth` part with whatever password you have in
your ~/.authinfo.gpg. remember to define them with `port irc` because that's
what we use to search the credentials.

```
machine freenode.spacemacsserver.me port irc user spacemacs_user/freenode password ZNC_PASSWORD
machine geekshed.spacemacsserver.me port irc user spacemacs_user/geekshed password ZNC_PASSWORD
```

Code for make this work shamelessly copy/pasted from
[here](http://git.tapoueh.org/?p=emacs.d.git) licensed with WTFPL.
