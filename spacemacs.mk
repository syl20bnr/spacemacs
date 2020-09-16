## Makefile --- Spacemacs master makefile
##
## Copyright (c) 2012-2020 Sylvain Benner & Contributors
##
## Author: Sylvain Benner <sylvain.benner@gmail.com>
## URL: https://github.com/syl20bnr/spacemacs
##
## This file is not part of GNU Emacs.
##
## License: GPLv3

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
