# Gnus contribution layer for Spacemacs

![gnus](img/gnus.gif)


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Gnus contribution layer for Spacemacs](#gnus-contribution-layer-for-spacemacs)
    - [Basic Concepts](#basic-concepts)
    - [Adding news sources](#adding-news-sources)
    - [Configuring gmail](#configuring-gmail)
    - [Org MIME integration](#org-mime-integration)
    - [Keybindings](#keybindings)

<!-- markdown-toc end -->

## Basic Concepts

Gnus is a news reading application. The Gnus terminology can be confusing for new users so the basics are listed here:

* Group - A Newsgroup but can also be a RSS Feed or a mail directory
* Topic - Newsgroups can be assigned to topics which will be used to structure the Group Buffer if Topic Mode is enabled(default).

## Adding news sources

Adding news sources can be done in your ```.spacemacs``` file by adding the following:

```elisp
    ;; Get email, and store in nnml
    (setq gnus-secondary-select-methods '(
                                        (nntp "gmane"
                                                (nntp-address "news.gmane.org"))
                                        (nntp "news.eternal-september.org")
                                        (nntp "nntp.aioe.org")
                                        (nntp "news.gwene.org")
                                        ))
```

For adding RSS Feeds please see the [keybindings section](#Keybindings).

## Configuring gmail

To configure Gnus with gmail support you can add the following to your ```.spacemacs``` file. 
```elisp
    ;; Get email, and store in nnml
    (setq gnus-secondary-select-methods '(
                                        (nnimap "gmail"
                                                (nnimap-address
                                                 "imap.gmail.com")
                                                (nnimap-server-port 993)
                                                (nnimap-stream ssl))
                                        ))

    ; Send email via Gmail:
    (setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com")

    ; Archive outgoing email in Sent folder on imap.gmail.com:
    (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail")

    ; set return email address based on incoming email address
    (setq gnus-posting-styles
        '(((header "to" "address@outlook.com")
           (address "address@outlook.com"))
	  ((header "to" "address@gmail.com")
         (address "address@gmail.com"))))

    ; store email in ~/gmail directory
    (setq nnml-directory "~/gmail")
    (setq message-directory "~/gmail")
```

Authentication for your gmail account is best stored in an ```authinfo``` or ```authinfo.pgp``` file.
It must be of the form:
```
machine smtp.gmail.com login name@gmail.com password SUPER_SECRET_PASS
machine imap.gmail.com login name@gmail.com port 993 password SUPER_SECRET_PASS
```
If you use two-step verification the password has to be an [application specific password](https://support.google.com/accounts/answer/185833?hl=en). 

## Org MIME integration

It is possible to send beautiful HTML emails using org mode.

Pressing <kbd>SPC m o</kbd> in a message buffer will convert the current message from org mode to html.
A org mode buffer can be sent via html email by pressing <kbd>SPC m h</kbd> in any org mode buffer.

## Keybindings

Gnus has very modal default keybindings. Please see the [manual](http://www.gnus.org/manual.html) for a complete list.

Basic and Spacemacs specific keybindings can be found in the following table.

Key Binding  | Gnus mode - Description
-------------|------------------------------------------------------------
`<SPC> a g`  | Starts Gnus
`m`          | New Message
`G R`        | Group Buffer - Add RSS feed
`^`          | Open Server Buffer. Browse Newsgroups.
`T n`        | Group Buffer - new Topic
`T m`        | Group Buffer - Move Group to Topic
`K`          | Article Buffer - Previous article
`J`          | Article Buffer - Next article
`<RET>`      | Summary Buffer(RSS) - Open article Link in browser 
`<TAB>`      | Summary Buffer(RSS) - Open article and switch to it
`<SPC> m o`  | Message Buffer - Use org mode to convert into html email
`<SPC> m h`  | Org Mode - Send current buffer as email message


