## -*- mode: emacs-lisp -*-
### Dockerfile --- Spacemacs-docker dockerfile file :)
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
dockerfiles/spacemacs-docker/usr/local/spacemacs" /usr/local/spacemacs   && \
    emacs -Q -batch -l /usr/local/spacemacs/lib/init.el > /dev/null 2>&1

SHELL ["/usr/bin/emacs", "-l", "/usr/local/spacemacs/lib/build-helpers.el", \
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

RUN ($ `("echo SPACEMACS REVISION: \"%s\"" , (getenv "GIT_REV")))
