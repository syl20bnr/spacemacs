## -*- mode: emacs-lisp -*-
### Dockerfile --- Spacemacs-docker onbuild file
##
## Copyright (c) 2012-2017 Sylvain Benner & Contributors
##
## Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
## URL: https://github.com/syl20bnr/spacemacs
##
## This file is not part of GNU Emacs.
##
### License: GPLv3

FROM spacemacs/spacemacs-docker:latest

MAINTAINER JAremko <w3techplaygound@gmail.com>

ONBUILD RUN ($ `("echo SPACEMACS REVISION: \"%s\"" , (getenv "GIT_REV")))

# Prefix "_" prevents ARG values from being overridden by ENV values
ONBUILD ARG _UNAME="spacemacser"
ONBUILD ARG _GNAME="spacemacsians"
ONBUILD ARG _UHOME="/home/spacemacs"
ONBUILD ARG _UID="1000"
ONBUILD ARG _GID="1000"
ONBUILD ARG _WORKSPACE="/mnt/workspace"

# Update ENV variables with ARG values
ONBUILD ENV UNAME="${_UNAME}"         \
            GNAME="${_GNAME}"         \
            UHOME="${_UHOME}"         \
            UID="${_UID}"             \
            GID="${_GID}"             \
            WORKSPACE="${_WORKSPACE}"

ONBUILD LABEL USER="${UNAME}"            \
              HOME="${UHOME}"            \
              GIT_REPO="${GIT_REPO}"     \
              GIT_BRANCH="${GIT_BRANCH}" \
              GIT_REV="${GIT_REV}"       \
              WORKSPACE="${WORKSPACE}"

# Store some global variables in /etc/environment
# Good place in general and easy to parse.
ONBUILD RUN (let ((envs '("UNAME" "GNAME" "UHOME" "UID" "GID" "WORKSPACE"))) \
              (dolist (env envs) (set-glob-env env (getenv env))))

# user/group/ssh
ONBUILD RUN ($ ["echo"                                                    \
                "${UNAME}:x:${UID}:${GID}:${UNAME},,,:${UHOME}:/bin/bash" \
                ">> /etc/passwd"]                                         \
               "echo \"${UNAME}::17032:0:99999:7:::\" >> /etc/shadow"     \
               "echo \"${GNAME}:x:${GID}:${UNAME}\"   >> /etc/group"      \
               ["echo \"${UNAME} ALL=(ALL) NOPASSWD: ALL\""               \
                "> /etc/sudoers.d/${UNAME}"]                              \
               "chmod 0440 /etc/sudoers.d/${UNAME}"                       \
               "mkdir -p \"${UHOME}\""                                    \
               "rm ~/.emacs.d/.cache/recentf"                             \
               `("mv ~/.emacs.d %s" ,(dir $UHOME))                        \
               `("mv ~/.spacemacs %s" ,(dir $UHOME))                      \
               "chown ${UID}:${GID} -R \"${UHOME}\" \"${WORKSPACE}\""     \
               "chown ${UID}:${GID} -R /usr/local/spacemacs/")

ONBUILD COPY .spacemacs "${UHOME}/.spacemacs"
ONBUILD COPY pub-keys /etc/pub-keys
ONBUILD COPY private "${UHOME}/.emacs.d/private"

# Install dependencies
ONBUILD ARG VERBOSE_INSTALL
ONBUILD RUN (progn                                                        \
              (set-glob-env "VERBOSE_INSTALL" (getenv "VERBOSE_INSTALL")) \
              (dolist (name (directory-files-recursively                  \
                             "/usr/local/spacemacs/deps-installers/"      \
                             "install.el"))                               \
                ($ `("%s; true" ,name)))                                  \
              ($ "chown ${UID}:${GID} -R \"${UHOME}\"")                   \
              (rm "/tmp/*")                                               \
              (mkdirp "/tmp/.X11-unix"))

ONBUILD USER ${UNAME}

# Run tests
ONBUILD RUN ($ "sudo chown ${UID}:${GID} ${UHOME}/.spacemacs" \
               "emacs -batch -u ${UNAME} -kill"               \
               "sdruntests")

RUN ($ `("echo SPACEMACS REVISION: \"%s\"" , (getenv "GIT_REV")))
