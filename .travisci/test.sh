#!/usr/bin/env bash
## Run script for Travis CI integration
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
    echo "Instead, navigate to the appropriate test folder and run make there."
    exit 1
fi

# Make sure that PR doesn't target master branch
if  [ $TRAVIS_SECURE_ENV_VARS = false ] &&
        [ $TRAVIS_PULL_REQUEST != false ] &&
        [ "$TRAVIS_BRANCH" = "master" ]; then
    printf '=%.0s' {1..70}
    printf "\n       し(*･∀･)／   Thanks for the contribution!  ＼(･∀･*)ノ\n"
    printf '=%.0s' {1..70}
    printf "\n( ＾◡＾)っ Please submit your PR against the develop branch.\n"
    echo   "You can read the contribution guidelines at:"
    echo   "https://github.com/syl20bnr/spacemacs/blob/develop/CONTRIBUTING.org"
    exit 1
fi

rm -rf ~/.emacs.d
mv "${TRAVIS_BUILD_DIR}" ~/.emacs.d
ln -sf ~/.emacs.d "${TRAVIS_BUILD_DIR}"
cd  ~/.emacs.d
echo "Pwd $(pwd)"

echo_headline () {
    printf '=%.0s' {1..70}
    printf "\n$1\n"
    printf '=%.0s' {1..70}
    echo
}

if [ ! -z "$TESTS" ]; then
    for test in "${TESTS[@]}"; do
        rm -rf ~/.emacs.d/elpa
        rm -rf ~/.emacs.d/.cache
        rm -f ~/.spacemacs
        testdir=~/.emacs.d/tests/$test
        echo_headline "Running '$test' in '$testdir' folder"
        if [ -f $testdir/dotspacemacs.el ]; then
            cp $testdir/dotspacemacs.el ~/.spacemacs
        fi
        cd $testdir && echo "Now in $(pwd)"
        make test || exit 2
    done

elif  [ ! -z "$FORMAT" ]; then
    echo_headline "Formatting documentation with core/tools/docfmt"
    emacs --batch -l ./core/tools/docfmt/run.el
    if [ $? -ne 0 ]; then
        echo "core/tools/docfmt failed"
        exit 2
    fi
    git diff --color HEAD > /tmp/spacefmt_result
    if [[ -s /tmp/spacefmt_result ]]; then
        echo_headline "PLEASE APPLY CHANGES BELOW:"
        cat /tmp/spacefmt_result
        exit 2
    fi
    echo "Done."

    echo_headline "Formatting documentation with core/tools/export"
    emacs --batch -l ./core/tools/export/run.el export
    if [ $? -ne 0 ]; then
        echo "core/tools/export failed"
        exit 2
    fi
    echo "Done."

    echo_headline "Checking for misplaced spaces and tabs."
    git diff --check --color > /tmp/space_test_result
    if [[ -s /tmp/space_test_result ]]; then
        echo_headline "PLEASE FIX ISSUES BELOW:"
        cat /tmp/space_test_result
        exit 2
    fi
    echo "Done."
fi
