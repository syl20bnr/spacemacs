## Makefile --- Spacemacs Core
##
## Copyright (c) 2012-2020 Sylvain Benner & Contributors
##
## Author: Sylvain Benner <sylvain.benner@gmail.com>
## URL: https://github.com/syl20bnr/spacemacs
##
## This file is not part of GNU Emacs.
##
## License: GPLv3

TEST_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

LOAD_FILES = core/core-versions.el core/core-load-paths.el
UNIT_TEST_FILES = \
  core-configuration-layer-utest.el \
  core-spacebind-utest.el \
  core-funcs-utest.el
FUNC_TEST_FILES = \
  core-spacemacs-ftest.el \
  core-spacemacs-buffer-ftest.el \
  core-configuration-layer-ftest.el \
  core-release-management-ftest.el

include ../../spacemacs.mk
