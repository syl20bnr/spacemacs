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

echo_headline () {
    printf '=%.0s' {1..70}
    printf "\n$1\n"
    printf '=%.0s' {1..70}
    echo
}

rm -rf ~/.emacs.d
mv "${TRAVIS_BUILD_DIR}" ~/.emacs.d
ln -sf ~/.emacs.d "${TRAVIS_BUILD_DIR}"
cd  ~/.emacs.d

echo_headline "INSTALLING \"${EVM_EMACS}\":"
curl -fsSkL https://gist.github.com/rejeep/ebcd57c3af83b049833b/raw \
     > /tmp/x.sh && source /tmp/x.sh
evm install $EVM_EMACS --use --skip
if [ $? -ne 0 ]; then
    echo "Installation failed"
    exit 2
fi
echo "DONE!"

echo_headline "INSTALLING DEPENDENCIES:"
cp ~/.emacs.d/.travisci/.spacemacs ~/
cd  ~/.emacs.d
emacs -batch -l init.el
if [ $? -ne 0 ]; then
    echo "Dependencies installation failed."
    exit 2
fi
echo "DONE!"

echo_headline "EXPORTING DOCUMENTATION:"
emacs -batch -l init.el -l core/core-documentation.el \
      -f spacemacs/publish-doc
if [ $? -ne 0 ]; then
    echo "spacemacs/publish-doc failed"
    exit 2
fi
echo "DONE!"

exit 0
