VER := $(shell python --version 2>&1 | grep -o "[0-9].[0-9].[0-9]*")
MAJOR_VERSION = $(shell python --version 2>&1 | grep -o "Python [0-9]" | grep -o "[0-9]")
ZIP := python-${VER}-docs-html.zip
URL := https://docs.python.org/2/archives/${ZIP}
URL2:= https://docs.python.org/3/archives/${ZIP}

ifneq (2,${MAJOR_VERSION})
	URL := ${URL2}
endif

download:
	@if [ ! -e ${ZIP} ] ; then     \
		echo "Downloading ${URL}"; \
		wget ${URL};               \
		unzip ${ZIP};              \
	fi
	./pylookup.py -u $(ZIP:.zip=)

.PHONY: download
