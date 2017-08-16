#!/usr/bin/env bash
## Documentation publishing script for Travis CI integration
##
## Copyright (c) 2012-2014 Sylvain Benner
## Copyright (c) 2014-2017 Sylvain Benner & Contributors
##
## Author: Eugene Yaremenko
## URL: https://github.com/syl20bnr/spacemacs
##
## This file is not part of GNU Emacs.
##
## License: GPLv3

if [ $USER != "travis" ]; then
    echo "This script is not designed to run locally."
    exit 2
fi

echo_headline () {
    printf '=%.0s' {1..70}
    printf "\n$1\n"
    printf '=%.0s' {1..70}
    echo
}

if  [ $TRAVIS_SECURE_ENV_VARS = true ]; then

    hub_version="2.2.9"
    hub_url="https://github.com/github/hub/releases/download/"
    hub_url+="v${hub_version}/hub-linux-amd64-${hub_version}.tgz"

    if  [ "$TRAVIS_BRANCH" = "master" ]; then
        PUBLISH="spacemacs.org"
    elif [ "$TRAVIS_BRANCH" = "develop" ]; then
        PUBLISH="develop.spacemacs.org"
    else
        echo "branch is \"${TRAVIS_BRANCH}\". Won't publish."
        exit 0
    fi

    echo "Publishing ${PUBLISH}"

    echo_headline "EXPORTING DOCUMENTATION:"
    emacs -batch -l init.el > /dev/null 2>&1
    emacs -batch -l init.el -l core/core-documentation.el \
          -f spacemacs/publish-doc
    if [ $? -ne 0 ]; then
        echo "spacemacs/publish-doc failed"
        exit 2
    fi

    echo_headline "CONFIGURING GIT USER:"
    git config --global user.name "${BOT_NAME}"
    git config --global user.email "${BOT_EMAIL}"
    git config --global push.default simple
    git config --global hub.protocol https
    export GITHUB_TOKEN=$BOT_TK

    echo_headline "CLONING TARGET REPOSITORY:"
    git clone "https://github.com/syl20bnr/${PUBLISH}.git" \
        -b gh-pages "/tmp/${PUBLISH}"
    rsync -avh ~/.emacs.d/export/ "/tmp/${PUBLISH}"
    git add -N .

    cd "/tmp/${PUBLISH}"
    if ! git diff-files  --quiet --; then
        echo_headline "COMMITTING CHANGES TO ${BOT_NAME}/${PUBLISH}:"
        git diff --color HEAD
        curl -L $hub_url | tar \
                               --strip-components=2 \
                               -xz \
                               --wildcards \
                               -C /tmp/ \
                               "*hub"
        /tmp/hub add --all
        /tmp/hub commit -m "doc update:$(date -u)"
        /tmp/hub fork
        mkdir -p ~/.ssh
        printf  "Host  github.com\n" > ~/.ssh/config
        printf  "  StrictHostKeyChecking no\n" >> ~/.ssh/config
        printf  "  UserKnownHostsFile=/dev/null\n" >> ~/.ssh/config
        git remote set-url "${BOT_NAME}" \
            "https://${BOT_NAME}:${BOT_TK}@github.com/${BOT_NAME}/${PUBLISH}"
        /tmp/hub push -f "${BOT_NAME}" gh-pages

        echo_headline "OPENING PR TO syl20bnr/${PUBLISH}.git"
        echo "Documentation updates (autoexport)" > msg
        echo "beep beep boop... Beep?" >> msg
        /tmp/hub pull-request -F msg
        echo "DONE!"
        exit 0
    else
        echo "NOTHING TO COMMIT!"
        exit 0
    fi
else
    echo "TRAVIS_SECURE_ENV_VARS isn't true - aborting."
    exit 0
fi
