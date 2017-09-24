### Dockerfile --- Dockerfile for CircleCI with emacs-snapshot
##
## Copyright (c) 2012-2017 Sylvain Benner & Contributors
##
## Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
##
##
## This file is not part of GNU Emacs.
##
### License: GPLv3

FROM ubuntu:zesty

MAINTAINER JAremko <w3techplaygound@gmail.com>

COPY cleanup /usr/local/sbin/

# basic stuff
RUN echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf \
    && apt-get update && apt-get install \
    bash \
    ca-certificates \
    curl \
    git \
    git \
    gzip \
    jq \
    make \
    openssl \
    tar \
    && cleanup

# Emacs
RUN apt-get update && apt-get install software-properties-common \
    && apt-add-repository ppa:ubuntu-elisp/ppa \
    && apt-get update && apt-get install emacs-snapshot-nox \
# Cleanup
    && apt-get purge software-properties-common \
    && cleanup
