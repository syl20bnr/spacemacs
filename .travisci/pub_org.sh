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

fold_start() {
    echo -e "travis_fold:start:$1\033[33;1m$2\033[0m"
}

fold_end() {
    echo -e "\ntravis_fold:end:$1\r"
}

cd  ~/.emacs.d

if [ "$TRAVIS_BRANCH" = "develop" ]; then
    PUBLISH="spacemacs"
else
    echo "branch is \"${TRAVIS_BRANCH}\". Won't publish."
    exit 0
fi

if [ `git rev-list HEAD...origin/$TRAVIS_BRANCH --count` != 0 ]; then
    echo "We are outdated. Won't publish."
    exit 0
fi

git config --global user.name "${BOT_NAME}"
git config --global user.email "${BOT_EMAIL}"
git config --global push.default simple
git config --global hub.protocol https
export GITHUB_TOKEN=$BOT_TK

fold_start "CLONING_TARGET_REPOSITORY"
target_URL="https://github.com/${SPACEMACS_REPO_SLUG}.git"
git clone "${target_URL}" -b "${TRAVIS_BRANCH}" "/tmp/${PUBLISH}"
if [ $? -ne 0 ]; then
    echo "Failed to clone \"${target_URL}\""
    exit 2
fi
fold_end "CLONING_TARGET_REPOSITORY"

fold_start "SELECTING_CHANGED_FILES"
rsync -rv \
      --include '*/' \
      --include='*.org' \
      --exclude='*' \
      --prune-empty-dirs \
      ~/.emacs.d/ \
      "/tmp/${PUBLISH}"
cd "/tmp/${PUBLISH}"
/tmp/hub add --all
/tmp/hub commit -m "documentation formatting: $(date -u)"
if [ $? -ne 0 ]; then
    echo "Nothing to commit - exiting."
    exit 0
fi
fold_end "SELECTING_CHANGED_FILES"

fold_start "PUSHING_CHANGES_TO_${BOT_NAME}/${PUBLISH}"
/tmp/hub fork
if [ $? -ne 0 ]; then
    echo "hub fork failed"
    exit 2
fi
fork_url="https://${BOT_NAME}:${BOT_TK}"
fork_url+="@github.com/${BOT_NAME}/${PUBLISH}.git"
git remote set-url "${BOT_NAME}" "${fork_url}"
/tmp/hub push -f "${BOT_NAME}" "${TRAVIS_BRANCH}" > /dev/null 2>&1
#                      prevent token leak ^^^^^^^^^^^^^^
# But it's actually not necessary since TravisCI masks secrets in logs
if [ $? -ne 0 ]; then
    echo "hub push to \"${BOT_NAME}\" failed"
    exit 2
fi
fold_end "PUSHING_CHANGES_TO_${BOT_NAME}/${PUBLISH}"

fold_start "OPENING_PR_TO_SPACEMACS_REPO"
echo "[bot] Documentation formatting" > msg
echo >> msg
echo "Merge with care - I'm just a stupid bot. Beep boop." >> msg
/tmp/hub pull-request -b "${TRAVIS_BRANCH}" -F msg
if [ $? -ne 0 ]; then
    echo "Seems like PR already exists (not a problem)"
fi
fold_end "OPENING_PR_TO_SPACEMACS_REPO"
