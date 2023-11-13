CASK = cask
EMACS ?= emacs
# Handle the mess when inside Emacs.
unexport INSIDE_EMACS		#cask not like this.
ifeq ($(EMACS), t)
EMACS = emacs
endif

emacs = $(EMACS)
emacs_version = $(shell $(emacs) --batch --eval \
		'(princ (format "%s.%s" emacs-major-version emacs-minor-version))')
$(info Using Emacs $(emacs_version))

version=$(shell sed -ne 's/^;\+ *Version: *\([0-9.]\)/\1/p' lisp/pdf-tools.el)
pkgname=pdf-tools-$(version)
pkgfile=$(pkgname).tar

.PHONY: all clean distclean bytecompile test check melpa

all: $(pkgfile)

# Create a elpa package including the server
$(pkgfile): .cask/$(emacs_version) server/epdfinfo lisp/*.el
	$(CASK) package .

# Compile the Lisp sources
bytecompile: .cask/$(emacs_version)
	$(CASK) exec $(emacs) --batch -L lisp -f batch-byte-compile lisp/*.el

# Clean bytecompiled sources
byteclean:
	rm -f -- lisp/*.elc
	rm -f -- lisp/*.eln

# Run ERT tests
test: all
	PACKAGE_TAR=$(pkgfile) $(CASK) exec ert-runner

check: test

# Run the autobuild script tests in docker
test-autobuild: server-test

# Run all tests
test-all: test test-autobuild

# Init cask
.cask/$(emacs_version):
	$(CASK) install

# Run the autobuild script (installing depends and compiling)
autobuild:
	cd server && ./autobuild

# Soon to be obsolete targets
melpa-build: autobuild
	cp build/epdfinfo .
install-server-deps: ;

# Create a package like melpa would.
melpa-package: $(pkgfile)
	cp $(pkgfile) $(pkgname)-melpa.tar
	tar -u --transform='s/server/$(pkgname)\/build\/server/' \
		-f $(pkgname)-melpa.tar \
		$$(git ls-files server)
	tar -u --transform='s/Makefile/$(pkgname)\/build\/Makefile/' \
		-f $(pkgname)-melpa.tar \
		Makefile
	tar -u --transform='s/README\.org/$(pkgname)\/README/' \
		-f $(pkgname)-melpa.tar \
		README.org
	-tar --delete $(pkgname)/epdfinfo \
		-f $(pkgname)-melpa.tar

# Various clean targets
clean: server-clean byteclean
	rm -f -- $(pkgfile)
	rm -f -- pdf-tools-readme.txt
	rm -f -- pdf-tools-$(version).entry

distclean: clean server-distclean
	rm -rf -- .cask

# Server targets
server/epdfinfo: server/Makefile server/*.[ch]
	$(MAKE) -C server

server/Makefile: server/configure
	cd server && ./configure -q

server/configure: server/configure.ac
	cd server && ./autogen.sh

server-test: server/Makefile
	$(MAKE) -C server check

server-clean:
	! [ -f server/Makefile ] || $(MAKE) -C server clean

server-distclean:
	! [ -f server/Makefile ] || $(MAKE) -C server distclean

server-test-supported: server/test/Makefile
	$(MAKE) -C server/test print

server-test-unsupported: server/test/Makefile
	$(MAKE) -C server/test print-failing
