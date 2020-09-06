#!/usr/bin/env bash
## HTML documentation publishing script for Travis CI integration
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

fold_start() {
    echo -e "travis_fold:start:$1\033[33;1m$2\033[0m"
}

fold_end() {
    echo -e "\ntravis_fold:end:$1\r"
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

export GITHUB_TOKEN=$BOT_TK

fold_start "CLONING_TARGET_REPOSITORY"
target_URL="https://github.com/syl20bnr/${PUBLISH}.git"
git clone "${target_URL}" -b gh-pages "/tmp/${PUBLISH}"
if [ $? -ne 0 ]; then
    echo "Failed to clone \"${target_URL}\""
    exit 2
fi
fold_end "CLONING_TARGET_REPOSITORY"

fold_start "SELECTING_CHANGED_FILES"
rsync -avh ~/.emacs.d/export/ "/tmp/${PUBLISH}"
cd "/tmp/${PUBLISH}"
/tmp/hub add --all
/tmp/hub commit -m "doc update:$(date -u)"
if [ $? -ne 0 ]; then
    echo "Nothing to commit - exiting."
    exit 0
fi
fold_end "SELECTING_CHANGED_FILES"

fold_start "CHECKING_IF_SPACEMACS_HEAD_IS_THE_SAME"
cd ~/.emacs.d
git remote update
base_revision=$(cat /tmp/base_revision)
rem_rev=$(git rev-parse '@{u}')
echo "Base revision: $base_revision"
echo "Remote revision: $rem_rev"
if [ "$base_revision" != "$rem_rev" ]; then
    echo "Looks like Spacemacs head has changed while we generated files."
    echo "Aborting."
    exit 0
fi
fold_end "CHECKING_IF_SPACEMACS_HEAD_IS_THE_SAME"

fold_start "PUSHING_CHANGES_TO_${BOT_NAME}/${PUBLISH}"
cd "/tmp/${PUBLISH}"
/tmp/hub fork
if [ $? -ne 0 ]; then
    echo "hub fork failed"
    exit 2
fi
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
fold_end "PUSHING_CHANGES_TO_${BOT_NAME}/${PUBLISH}"

fold_start "OPENING_PR_TO_syl20bnr/${PUBLISH}"
echo "Documentation updates (autoexport)" > msg
echo >> msg
echo "beep beep boop... Beep?" >> msg
/tmp/hub pull-request -F msg
if [ $? -ne 0 ]; then
    echo "Seems like PR already exists (not a problem)"
fi
fold_end "OPENING_PR_TO_syl20bnr/${PUBLISH}"
