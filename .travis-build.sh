#!/usr/bin/env bash
## run_build.sh --- Travis CI File for Spacemacs
##
## Copyright (c) 2012-2014 Sylvain Benner
## Copyright (c) 2014-2015 Sylvain Benner & Contributors
##
## Author: Sylvain Benner <sylvain.benner@gmail.com>
## URL: https://github.com/syl20bnr/spacemacs
##
## This file is not part of GNU Emacs.
##
## License: GPLv3

if [ $USER != "travis" ]; then
    echo "This script is not designed to run locally."
    echo "Instead, navigate to the appropriate test folder and run make there instead."
    exit 1
fi

# Make sure that PR doesn't target master branch
if  [ $TRAVIS_SECURE_ENV_VARS = false ] &&
        [ $TRAVIS_PULL_REQUEST != false ] &&
        [ $TRAVIS_BRANCH = "master" ]; then
    printf '=%.0s' {1..70}
    printf "\n       し(*･∀･)／   Thanks for the contribution!  ＼(･∀･*)ノ\n"
    printf '=%.0s' {1..70}
    printf "\n( ＾◡＾)っ Please submit your pull request against the develop branch.\n"
    echo   "You can read the contribution guidelines at:"
    echo   "https://github.com/syl20bnr/spacemacs/blob/develop/CONTRIBUTING.org"
    exit 1
fi

# Make sure that a maintainer was summoned via commit message to review it
if [ "${CHECK_MENTIONS}" = "true" ]; then
	cd "${TRAVIS_BUILD_DIR}"
	last_commit_message=$(git log -2 --pretty=%B)
	if [[ ! "$last_commit_message" == *"@"* ]]; then
		echo "Please summon a maintainer"
		echo "See: https://github.com/syl20bnr/spacemacs/blob/develop/MAINTAINERS.org"
		exit 1
	fi
	grep -Po "@\S+" <<< "$last_commit_message" > /tmp/tcm_mentions
	maintainer_mentioned=false
	while read p; do
		if grep -q "$p" "MAINTAINERS.org"; then
			echo "$p was mentioned"
			maintainer_mentioned=true
		fi
	done </tmp/tcm_mentions
	if [ ! "$maintainer_mentioned" = true ] ; then
		echo 'None of:'
		cat /tmp/tcm_mentions
		echo "Seems to be a maintainer. Typo?"
		echo "See: https://github.com/syl20bnr/spacemacs/blob/develop/MAINTAINERS.org"
		exit 1
	fi
	exit 0
fi

# Formatting conventions tests
if [ ! -z "$FORMATTING" ]; then
	cd "${TRAVIS_BUILD_DIR}"
	echo "TRAVIS_COMMIT_RANGE: ${TRAVIS_COMMIT_RANGE}"
	first_commit=`echo ${TRAVIS_COMMIT_RANGE} | sed -r 's/\..*//'`
	git diff --name-only "${first_commit}" HEAD > /tmp/changed_files
	case "${FORMATTING}" in
		space-test)
			echo "Testing for trailing and all sorts of broken white spaces"
			git reset -q "${first_commit}"
			git diff --check --color > space_test_result
			if [[ -s space_test_result ]]; then
				cat space_test_result
				exit 1
			fi
			echo "No bad spaces detected"
			exit 0
		;;
		spacefmt)
			echo "Testing changed files with spacefmt"
			rm -rf ~/.emacs.d
			ln -sf `pwd` ~/.emacs.d
			cp ~/.emacs.d/core/templates/.spacemacs.template ~/
			mv ~/.spacemacs.template ~/.spacemacs
			while read p
			do
				echo "Checking $p file"
				./core/tools/spacefmt/spacefmt -f "$p"
				if [ $? -ne 0 ]; then
					echo "spacefmt exited with $?"
					exit 2
				fi
			done </tmp/changed_files
			git diff --color HEAD > spacefmt_result
			if [[ -s spacefmt_result ]]; then
				echo "Please apply these changes:"
				cat spacefmt_result
				exit 1
			fi
			echo "All changed files comply with spacefmt"
			exit 0
		;;
	esac
fi

# Emacs tests
echo "Pwd $(pwd)"
rm -rf ~/.emacs.d
ln -sf `pwd` ~/.emacs.d

for test in "${TESTS[@]}"; do
    rm -rf ~/.emacs.d/elpa
    rm -rf ~/.emacs.d/.cache
    rm -f ~/.spacemacs

    testdir=~/.emacs.d/tests/$test
    echo "Running '$test' in '$testdir' folder"
    if [ -f $testdir/dotspacemacs.el ]; then
        cp $testdir/dotspacemacs.el ~/.spacemacs
    fi
    cd $testdir && echo "Now in $(pwd)"
    make test || exit 2
done
