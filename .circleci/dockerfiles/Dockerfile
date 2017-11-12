FROM alpine

MAINTAINER JAremko <w3techplaygound@gmail.com>

RUN apk --no-cache add \
    bash \
    ca-certificates \
    curl \
    emacs-nox \
    git \
    gzip \
    jq \
    openssh \
    tar \
    && rm -rf /var/cache/* /tmp/* /var/log/* ~/.cache

ENV HOME=/root/

VOLUME /root/project

ENTRYPOINT ["bin/bash", "-c"]
