## Makefile --- Spacemacs master makefile
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


EMACS_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
TEST_NAME = `basename $(TEST_DIR) | tr a-z A-Z`

all: test

test: installation unit_tests func_tests

installation:
	@echo "================================================================="
	@echo "INSTALLATION OF PACKAGES FOR $(TEST_NAME)"
	@echo "================================================================="
	@emacs -batch \
		$(addprefix -l $(EMACS_DIR)/, $(LOAD_FILES))

ifneq ($(strip $(UNIT_TEST_FILES)),)
unit_tests:
	@echo "================================================================="
	@echo "UNIT TESTS FOR $(TEST_NAME)"
	@echo "================================================================="
	emacs -batch -l ert \
		$(addprefix -l $(EMACS_DIR)/, $(LOAD_FILES)) \
		$(addprefix -l $(TEST_DIR)/, $(UNIT_TEST_FILES)) \
		-f ert-run-tests-batch-and-exit
endif

ifneq ($(strip $(FUNC_TEST_FILES)),)
func_tests:
	@echo "================================================================="
	@echo "FUNCTIONAL TESTS FOR $(TEST_NAME)"
	@echo "================================================================="
	@emacs -batch -l ert \
		$(addprefix -l $(EMACS_DIR)/, $(LOAD_FILES)) \
		$(addprefix -l $(TEST_DIR)/, $(FUNC_TEST_FILES)) \
		-f ert-run-tests-batch-and-exit
endif

.PHONY: test unit_tests func_tests
