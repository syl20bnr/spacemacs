## Makefile --- Spacemacs Core
##
## Copyright (c) 2012-2023 Sylvain Benner & Contributors
##
## Author: Sylvain Benner <sylvain.benner@gmail.com>
## URL: https://github.com/syl20bnr/spacemacs
##
## This file is not part of GNU Emacs.
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
