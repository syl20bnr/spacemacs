# By default the `help` goal is executed.
.DEFAULT_GOAL := help

# Displays a help message containing usage instructions and a list of available
# goals.
#
# Usage:
#
# To display the help message run:
# ```sh
# make help
# ```
#
# This is also the default goal so you can run it as following:
# ```sh
# make
# ```
.PHONY: help
help:
	@echo 'Usage: make [<GOAL_1>, <GOAL_2>, ...]'
	@echo ''
	@echo 'Examples:'
	@echo '  make'
	@echo '  make ci'
	@echo '  make ci-eclint'
	@echo ''
	@echo 'Goals:'
	@echo '  - ci:        Runs checks on this repository.'
	@echo '  - ci-eclint: Runs `eclint`.'
	@echo ''
	@echo 'Default goal: help'

# Runs checks on this repository.
#
# Usage:
# ```sh
# make ci
# ```
.PHONY: ci
ci: ci-eclint

# Runs `eclint`.
#
# Usage:
# ```sh
# make ci:eclint
# ```
.PHONY: ci-eclint
ci-eclint:
	@eclint check *
