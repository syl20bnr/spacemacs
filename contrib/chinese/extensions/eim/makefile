###################################
# user customized variables
EMACS = emacs
PREFIX = $$HOME/local
###################################

###################################
ELISPDIR = $(PREFIX)/share/emacs/site-lisp/eim
EL = eim.el eim-extra.el eim-py.el \
            eim-wb-gb2312.el eim-wb-gbk.el eim-wb.el eim-cj.el \
     eim-cj-chars.el eim-eb.el eim-eb-map.el eim-table.el
ELC = eim.elc eim-extra.elc eim-py.elc \
      eim-wb-gb2312.elc eim-wb-gbk.elc eim-wb.elc eim-cj.elc \
      eim-eb.elc eim-eb-map.elc eim-cj-chars.elc eim-table.elc
EXTRAFILES = wb.txt mywb.txt wbpy.txt otherpy.txt py.txt cj.txt \
          cjeb.txt ebpy.txt
DISTFILES = ChangeLog ChangeLog.1 Readme makefile \
            $(EL) $(EXTRAFILES) \
            pychr.txt sanguo.txt charpy.st pyword2tbl.pl mergepy.pl \
           table2chartbl.pl

all: $(ELC)

install: all
	install -d $(ELISPDIR)
	install -m 0644 $(EL) $(ELC) $(ELISPDIR)
	install -m 0666 $(EXTRAFILES) $(ELISPDIR)
	cd $(ELISPDIR) && gzip -qf *.el

uninstall:
	if [ -d "$(ELISPDIR)" ]; then rm -rf $(ELISPDIR); fi

dist: $(DISTFILES)
	version=`grep "eim-version" eim.el`; \
	version=`echo $$version | perl -ne '/[0-9.]+/; print $$&'`; \
	rm -rf "eim-$$version"; \
	mkdir "eim-$$version"; \
	cp $(DISTFILES) "eim-$$version"; \
	tar -zcvf "eim-$$version.tar.gz" "eim-$$version"; \
	rm -rf "eim-$$version"

%.elc: %.el
	$(EMACS) -q -batch --no-site-file --eval="(add-to-list 'load-path \".\")" \
	-f batch-byte-compile $<

clean:
	-rm -f *.elc *~
