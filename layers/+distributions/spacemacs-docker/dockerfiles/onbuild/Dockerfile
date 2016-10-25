## -*- mode: emacs-lisp -*-
### Dockerfile --- Spacemacs-docker onbuild file
##
## Copyright (c) 2012-2016 Sylvain Benner & Contributors
##
## Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
## URL: https://github.com/syl20bnr/spacemacs
##
## This file is not part of GNU Emacs.
##
### License: GPLv3

FROM jare/docker-emacs:latest

MAINTAINER JAremko <w3techplaygound@gmail.com>

# We get actual values from "./hooks/build" if run with DockerHub autobuild
ARG GIT_REPO="https://github.com/syl20bnr/spacemacs.git"
ARG GIT_BRANCH="develop"
ARG GIT_REV

# Make ARGs available in the children dockerfiles
ENV GIT_REPO="${GIT_REPO}"     \
    GIT_BRANCH="${GIT_BRANCH}" \
    GIT_REV="${GIT_REV}"

# "We do what we must because we can." - Aperture Science
RUN git clone "${GIT_REPO}" -b "${GIT_BRANCH}" "${UHOME}/.emacs.d"       && \
    chown ${UID}:${GID} -R "${UHOME}"                                    && \
    cp -rf "${UHOME}/.emacs.d/layers/+distributions/spacemacs-docker/\
dockerfiles/onbuild/usr/local/spacemacs" /usr/local/spacemacs            && \
    emacs -Q -batch -l /usr/local/spacemacs/lib/init.el > /dev/null 2>&1

SHELL ["/usr/bin/emacs", "-l", "/usr/local/spacemacs/lib/dockerfile-dsl.el", \
       "-Q", "-batch", "--eval"]

# Build and test spacemacs (with all packages downloaded)
RUN (progn                                                                 \
      (cp "~/.emacs.d/core/templates/.spacemacs.template" "~/")            \
      (mv "~/.spacemacs.template" "~/.spacemacs")                          \
      (mkdirp (dir (getenv "WORKSPACE")))                                  \
      (let ((dotfile-fp (f-expand "~/.spacemacs")))                        \
        (with-temp-file dotfile-fp                                         \
          (insert-file-contents dotfile-fp)                                \
          (goto-char (point-min))                                          \
          (re-search-forward (regexp-quote "distribution 'spacemacs"))     \
          (replace-match "distribution 'spacemacs-docker")                 \
          (goto-char (point-min))                                          \
          (re-search-forward (regexp-quote "install-packages 'used-only")) \
          (replace-match "install-packages 'all"))                         \
          ($ "emacs -batch -u ${UNAME} -kill" "sdruntests")))

# Usefull for inspecting with:
# docker inspect --format='{{index .Config.Labels "<LABEL_NAME>"}}' <ID>
LABEL USER="${UNAME}"                      \
      HOME="${UHOME}"                      \
      GIT_REPO="${GIT_REPO}"               \
      GIT_BRANCH="${GIT_BRANCH}"           \
      GIT_REV="${GIT_REV}"                 \
      WORKSPACE="${WORKSPACE}"             \
      SSHD_PORT="${SSHD_PORT}"             \
      XPRA_DISPLAY="${XPRA_DISPLAY}"       \
      XPRA_HTML_PORT="${XPRA_HTML_PORT}"   \
      MOSH_PORT_RANGE="${MOSH_PORT_RANGE}"

# There is a good(?) reason to use so many ONBUILD instructions...

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
               "chown ${UID}:${GID} -R \"${UHOME}\" \"${WORKSPACE}\"")

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
