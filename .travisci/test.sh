#!/usr/bin/env bash
## Test script for Travis CI integration
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

echo_headline "CHECKING FOR MISPLACED SPACES AND TABS:"
git diff --check --color > /tmp/space_test_result
if [[ -s /tmp/space_test_result ]]; then
    echo_headline "PLEASE FIX ISSUES BELOW:"
    cat /tmp/space_test_result
    exit 2
fi
echo "Done."

echo_headline "FORMATTING DOCUMENTATION:"
docker run --rm -v "${TRAVIS_BUILD_DIR}":/tmp/docs/ \
       jare/spacedoc format /tmp/docs/
if [ $? -ne 0 ]; then
    echo "Formatting failed."
    exit 2
fi

git diff --color HEAD > /tmp/spacefmt_result
if [[ -s /tmp/spacefmt_result ]]; then
    echo_headline "PLEASE APPLY CHANGES BELOW:"
    cat /tmp/spacefmt_result
    exit 2
fi

echo_headline "TESTING DOCUMENTATION WITH SDN EXPORT:"
docker run --rm -v "${TRAVIS_BUILD_DIR}":/tmp/docs/ \
       -v /tmp/sdn-files/:/tmp/export/ \
       jare/spacedoc export /tmp/docs/
if [ $? -ne 0 ]; then
    echo "Exporting failed."
    exit 2
fi

echo_headline "VALIDATING DOCUMENTATION:"
docker run --rm -v /tmp/sdn-files/:/tmp/sdn-files/ \
       jare/spacedoc validate /tmp/sdn-files/
if [ $? -ne 0 ]; then
    echo "Validation failed."
    exit 2
fi
echo "Done."
