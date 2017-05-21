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
        [ "$TRAVIS_BRANCH" = "master" ]; then
    printf '=%.0s' {1..70}
    printf "\n       し(*･∀･)／   Thanks for the contribution!  ＼(･∀･*)ノ\n"
    printf '=%.0s' {1..70}
    printf "\n( ＾◡＾)っ Please submit your pull request against the develop branch.\n"
    echo   "You can read the contribution guidelines at:"
    echo   "https://github.com/syl20bnr/spacemacs/blob/develop/CONTRIBUTING.org"
    exit 1
fi

rm -rf ~/.emacs.d
mv "${TRAVIS_BUILD_DIR}" ~/.emacs.d
ln -sf ~/.emacs.d "${TRAVIS_BUILD_DIR}"
cd  ~/.emacs.d
echo "Pwd $(pwd)"

# Formatting conventions tests
if [ ! -z "$FORMATTING" ]; then
	echo "TRAVIS_COMMIT_RANGE: ${TRAVIS_COMMIT_RANGE}"
	first_commit=`echo ${TRAVIS_COMMIT_RANGE} | sed -r 's/\..*//'`
	git diff --name-only "${first_commit}" HEAD > /tmp/changed_files
	case "${FORMATTING}" in
		space-test)
			echo "Testing for trailing and all sorts of broken white spaces"
			git reset -q "${first_commit}"
			git add -N .
			git diff --check --color > space_test_result
			if [[ -s space_test_result ]]; then
				cat space_test_result
				exit 1
			fi
			echo "No bad spaces detected"
			exit 0
		;;
		spacefmt)
			echo "Testing changed ORG files with spacefmt"
			cp ~/.emacs.d/core/templates/.spacemacs.template ~/
			mv ~/.spacemacs.template ~/.spacemacs
			while read p
			do
				if [ -f "$p" ]; then
					if [ ${p: -4} == ".org" ]; then
						echo "Checking $p file"
						./core/tools/spacefmt/spacefmt -f "$p"
						if [ $? -ne 0 ]; then
							echo "spacefmt failed"
							exit 2
						fi
					fi
				fi
			done </tmp/changed_files
			git diff --color HEAD > spacefmt_result
			if [[ -s spacefmt_result ]]; then
				printf '=%.0s' {1..70}
				printf "\nPLEASE APPLY CHANGES BELOW:\n"
				printf '=%.0s' {1..70}
				echo
				cat spacefmt_result
				exit 1
			fi
			echo "All changed files comply with spacefmt"
			exit 0
		;;
	esac
fi

# If we are pushing changes to the master branch,
# open PR to syl20bnr/${PUBLISH} with Spacemacs
# documentation exported as HTML and formatted with spacefmt
if  [ $TRAVIS_SECURE_ENV_VARS = true ] && [ ! -z "$PUBLISH" ] && [ $TRAVIS_PULL_REQUEST = false ]; then
	if  [ "$TRAVIS_BRANCH" = "master" ] && [ "$PUBLISH" != "spacemacs.org" ] ||
	    [ "$TRAVIS_BRANCH" = "develop" ] && [ "$PUBLISH" != "develop.spacemacs.org" ]; then
		echo "branch is \"${TRAVIS_BRANCH}\", won't publish to \"${PUBLISH}\" repository!"
		exit 0
	fi
	printf '=%.0s' {1..70}
	printf "\n FORMATTING DOCUMENTATION:\n"
	printf '=%.0s' {1..70}
	echo
	cp ~/.emacs.d/tests/doc/dotspacemacs.el ~/dotspacemacs.el
	mv ~/dotspacemacs.el ~/.spacemacs
	./core/tools/spacefmt/spacefmt doc
	if [ $? -ne 0 ]; then
		echo "spacefmt exited with: $?"
		exit 2
	fi
	printf '=%.0s' {1..70}
	printf "\n EXPORTING DOCUMENTATION:\n"
	printf '=%.0s' {1..70}
	echo
	emacs -batch -l init.el > /dev/null 2>&1
	emacs -batch -l init.el -l core/core-documentation.el -f spacemacs/publish-doc
	if [ $? -ne 0 ]; then
		echo "spacemacs/publish-doc failed"
		exit 2
	fi
	git config --global user.name "${BOT_NAME}"
	git config --global user.email "${BOT_EMAIL}"
	git config --global push.default simple
	git config --global hub.protocol https
	export GITHUB_TOKEN=$BOT_TK
	git clone "https://github.com/syl20bnr/${PUBLISH}.git" -b gh-pages "/tmp/${PUBLISH}"
	rsync -avh ~/.emacs.d/export/ "/tmp/${PUBLISH}"
	git add -N .
	cd "/tmp/${PUBLISH}"
	if ! git diff-files  --quiet --; then
		printf '=%.0s' {1..70}
		printf "\n COMMITTING CHANGES TO ${BOT_NAME}/${PUBLISH}:\n"
		printf '=%.0s' {1..70}
		echo
		git diff --color HEAD
		curl -L https://github.com/github/hub/releases/download/v2.2.9/hub-linux-amd64-2.2.9.tgz | tar \
			--strip-components=2 -xz --wildcards -C /tmp/ "*hub"
		/tmp/hub add --all
		/tmp/hub commit -m "doc update:$(date -u)"
		/tmp/hub fork
		mkdir -p ~/.ssh
		printf "Host  github.com\n  StrictHostKeyChecking no\n  UserKnownHostsFile=/dev/null\n" \
			>  ~/.ssh/config
		git remote set-url "${BOT_NAME}" \
			"https://${BOT_NAME}:${BOT_TK}@github.com/${BOT_NAME}/${PUBLISH}.git"
		/tmp/hub push -f "${BOT_NAME}" gh-pages
		printf '=%.0s' {1..70}
		printf "\n OPENING PR TO syl20bnr/${PUBLISH}.git\n"
		printf '=%.0s' {1..70}
		echo
		echo "Documentation updates (autoexport)" > msg
		echo "beep beep boop... Beep?" >> msg
		/tmp/hub pull-request -F msg
		printf '=%.0s' {1..70}
		printf "\n DONE!\n"
		printf '=%.0s' {1..70}
		echo
		exit 0
	else
		printf '=%.0s' {1..70}
		printf "\n NOTHING TO COMMIT!\n"
		printf '=%.0s' {1..70}
		echo
		exit 0
	fi
fi

# Emacs tests
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
