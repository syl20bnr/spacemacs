#!/usr/bin/env bash
## Documentation publishing script for Travis CI integration
##
## Copyright (c) 2012-2014 Sylvain Benner
## Copyright (c) 2014-2018 Sylvain Benner & Contributors
##
## Author: Eugene Yaremenko
## URL: https://github.com/syl20bnr/spacemacs
##
## This file is not part of GNU Emacs.
##
## License: GPLv3

echo_headline () {
    printf '=%.0s' {1..70}
    printf "\n$1\n"
    printf '=%.0s' {1..70}
    echo
}

cd  ~/.emacs.d

if  [ "$TRAVIS_BRANCH" = "master" ]; then
    PUBLISH="spacemacs.org"
elif [ "$TRAVIS_BRANCH" = "develop" ]; then
    PUBLISH="develop.spacemacs.org"
else
    echo "branch is \"${TRAVIS_BRANCH}\". Won't publish."
    exit 0
fi

echo_headline "CONFIGURING GIT USER:"
git config --global user.name "${BOT_NAME}"
git config --global user.email "${BOT_EMAIL}"
git config --global push.default simple
git config --global hub.protocol https
export GITHUB_TOKEN=$BOT_TK

echo_headline "CLONING TARGET REPOSITORY:"
target_URL="https://github.com/syl20bnr/${PUBLISH}.git"
git clone "${target_URL}" -b gh-pages "/tmp/${PUBLISH}"
if [ $? -ne 0 ]; then
    echo "Failed to clone \"${target_URL}\""
    exit 2
fi
echo "Done."
rsync -avh ~/.emacs.d/export/ "/tmp/${PUBLISH}"
git add --all
git diff --cached --exit-code
if [ $? -eq 0 ]; then
    echo "Nothing to commit - exiting."
    exit 0
fi

hub_version="2.2.9"
hub_url="https://github.com/github/hub/releases/download/"
hub_url+="v${hub_version}/hub-linux-amd64-${hub_version}.tgz"

cd "/tmp/${PUBLISH}"
echo_headline "PUSHING CHANGES TO ${BOT_NAME}/${PUBLISH}:"
curl -L $hub_url | tar \
                       --strip-components=2 \
                       -xz \
                       --wildcards \
                       -C /tmp/ \
                       "*hub"
/tmp/hub add --all
/tmp/hub commit -m "doc update:$(date -u)"
if [ $? -ne 0 ]; then
    echo "hub commit failed"
    exit 2
fi
/tmp/hub fork
if [ $? -ne 0 ]; then
    echo "hub fork failed"
    exit 2
fi
mkdir -p ~/.ssh
printf  "Host  github.com\n" > ~/.ssh/config
printf  "  StrictHostKeyChecking no\n" >> ~/.ssh/config
printf  "  UserKnownHostsFile=/dev/null\n" >> ~/.ssh/config
fork_url="https://${BOT_NAME}:${BOT_TK}"
fork_url+="@github.com/${BOT_NAME}/${PUBLISH}.git"
git remote set-url "${BOT_NAME}" "${fork_url}"
/tmp/hub push -f "${BOT_NAME}" gh-pages > /dev/null 2>&1
#                      prevent token leak ^^^^^^^^^^^^^^
# But it's actually not necessary since TravisCI masks secrets in logs
if [ $? -ne 0 ]; then
    echo "hub push to \"${BOT_NAME}\" failed"
    exit 2
fi
echo_headline "OPENING PR TO syl20bnr/${PUBLISH}.git"
echo "Documentation updates (autoexport)" > msg
echo "beep beep boop... Beep?" >> msg
/tmp/hub pull-request -F msg
if [ $? -ne 0 ]; then
    echo "Seems like PR already exists (not a problem)"
fi
echo "DONE!"
exit 0
