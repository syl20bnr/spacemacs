#!/usr/bin/env bash
## Documentation publishing preparation script for Travis CI integration
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

mkdir -p ~/.ssh
printf  "Host  github.com\n" > ~/.ssh/config
printf  "  StrictHostKeyChecking no\n" >> ~/.ssh/config
printf  "  UserKnownHostsFile=/dev/null\n" >> ~/.ssh/config

git config --global user.name "${BOT_NAME}"
git config --global user.email "${BOT_EMAIL}"
git config --global push.default simple
git config --global hub.protocol https
export GITHUB_TOKEN=$BOT_TK

git remote update
base_revision=$(git rev-parse '@')
echo $base_revision > /tmp/base_revision
echo "Base revision $base_revision"

fold_start "FORMATTING_DOCUMENTATION"
docker run --rm \
       -v "${TRAVIS_BUILD_DIR}/.ci/spacedoc-cfg.edn":/opt/spacetools/spacedoc-cfg.edn \
       -v "${TRAVIS_BUILD_DIR}":/tmp/docs/ \
       jare/spacetools docfmt /tmp/docs/
if [ $? -ne 0 ]; then
    echo "Formatting failed."
    exit 2
fi
fold_end "FORMATTING_DOCUMENTATION"

fold_start "CREATING_DOCUMENTATION_PATCH_FILE"
git add --all
git commit -m "documentation formatting: $(date -u)"
if [ $? -ne 0 ]; then
    echo "Documentation doesn't need fixes."
else
    git format-patch -1 HEAD --stdout > /tmp/docfmt.patch
    if [ $? -ne 0 ]; then
        echo "Failed to create patch file."
    fi
    cat /tmp/docfmt.patch
fi
fold_end "CREATING_DOCUMENTATION_PATCH_FILE"

rm -rf ~/.emacs.d
mv "${TRAVIS_BUILD_DIR}" ~/.emacs.d
cd  ~/.emacs.d
cp ./.travisci/.spacemacs ~/
ln -sf ~/.emacs.d "${TRAVIS_BUILD_DIR}"

fold_start "INSTALLING_DEPENDENCIES"
docker run --rm \
	-v "${TRAVIS_BUILD_DIR}:/root/.emacs.d" \
	-v "${TRAVIS_BUILD_DIR}/.travisci/.spacemacs:/root/.spacemacs" \
	--entrypoint emacs \
	jare/spacetools -batch -l /root/.emacs.d/init.el
if [ $? -ne 0 ]; then
    echo "Dependencies installation failed."
    exit 2
fi
fold_end "INSTALLING_DEPENDENCIES"

fold_start "EXPORTING_DOCUMENTATION"
docker run --rm \
	-v "${TRAVIS_BUILD_DIR}:/root/.emacs.d" \
	-v "${TRAVIS_BUILD_DIR}/.travisci/.spacemacs:/root/.spacemacs" \
	--entrypoint emacs \
	jare/spacetools -batch \
		-l /root/.emacs.d/init.el \
		-l /root/.emacs.d/core/core-documentation.el \
		-f spacemacs/publish-doc
if [ $? -ne 0 ]; then
    echo "spacemacs/publish-doc failed"
    exit 2
fi
fold_end "EXPORTING_DOCUMENTATION"

fold_start "INSTALLING_HUB"
hub_version="2.5.1"
hub_url="https://github.com/github/hub/releases/download/"
hub_url+="v${hub_version}/hub-linux-amd64-${hub_version}.tgz"
curl -L $hub_url | tar \
                       --strip-components=2 \
                       -xz \
                       --wildcards \
                       -C /tmp/ \
                       "*hub"
if [ $? -ne 0 ]; then
    echo "Hub installation failed."
    exit 2
fi
fold_end "INSTALLING_HUB"
