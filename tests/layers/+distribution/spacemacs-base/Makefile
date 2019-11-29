## Makefile --- Spacemacs Core
##
## Copyright (c) 2012-2018 Sylvain Benner & Contributors
##
## Author: Sylvain Benner <sylvain.benner@gmail.com>
## URL: https://github.com/syl20bnr/spacemacs
##
## This file is not part of GNU Emacs.
##
## License: GPLv3

TEST_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

LOAD_FILES = init.el
UNIT_TEST_FILES = \
  evil-evilified-state-utest.el \
  line-numbers-utest.el \
  spacebind-utest.el
FUNC_TEST_FILES = \
  evil-evilified-state-ftest.el

include ../../../../spacemacs.mk
