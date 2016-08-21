# Copyright (C) 2013-2015 all contributors <meta@public-inbox.org>
# License: AGPLv3 or later (https://www.gnu.org/licenses/agpl-3.0.txt)
all::

RSYNC = rsync
RSYNC_DEST = public-inbox.org:/srv/public-inbox/
docs := README COPYING INSTALL TODO HACKING
docs += $(shell git ls-files 'Documentation/*.txt')
INSTALL = install
PODMAN = pod2man
PODMAN_OPTS = -v --stderr -d 1993-10-02 -c 'public-inbox user manual'
podman = $(PODMAN) $(PODMAN_OPTS)
PODTEXT = pod2text
PODTEXT_OPTS = --stderr
podtext = $(PODTEXT) $(PODTEXT_OPTS)

m1 =
m1 += public-inbox-mda
m5 =
m7 =

man1 := $(addsuffix .1, $(m1))
man5 := $(addsuffix .5, $(m5))
man7 := $(addsuffix .7, $(m7))

all:: man html

man: $(man1) $(man5) $(man7)

prefix ?= $(HOME)
mandir ?= $(prefix)/share/man
man1dir = $(mandir)/man1
man5dir = $(mandir)/man5
man7dir = $(mandir)/man7

install-man: man
	test -z "$(man1)" || $(INSTALL) -d -m 755 $(DESTDIR)$(man1dir)
	test -z "$(man5)" || $(INSTALL) -d -m 755 $(DESTDIR)$(man5dir)
	test -z "$(man7)" || $(INSTALL) -d -m 755 $(DESTDIR)$(man7dir)
	test -z "$(man1)" || $(INSTALL) -m 644 $(man1) $(DESTDIR)$(man1dir)
	test -z "$(man5)" || $(INSTALL) -m 644 $(man5) $(DESTDIR)$(man5dir)
	test -z "$(man7)" || $(INSTALL) -m 644 $(man7) $(DESTDIR)$(man7dir)

%.1 : Documentation/%.pod
	$(podman) -s 1 $< $@+ && mv $@+ $@

mantxt = $(addprefix Documentation/, $(addsuffix .txt, $(m1)))
docs += $(mantxt)

all :: $(mantxt)

Documentation/%.txt : Documentation/%.pod
	$(podtext) $< $@+ && mv $@+ $@

txt2pre = ./Documentation/txt2pre <$< >$@+ && touch -r $< $@+ && mv $@+ $@
txt := INSTALL README COPYING TODO
dtxt := design_notes.txt design_www.txt dc-dlvr-spam-flow.txt
dtxt := $(addprefix Documentation/, $(dtxt)) $(mantxt)

%.html: %.txt
	$(txt2pre)
%.html: %
	$(txt2pre)

docs_html := $(addsuffix .html, $(subst .txt,,$(dtxt)) $(txt))
html: $(docs_html)
gz_docs := $(addsuffix .gz, $(docs) $(docs_html))
rsync_docs := $(gz_docs) $(docs) $(txt) $(docs_html)
%.gz: %
	gzip -9 --rsyncable <$< >$@+
	touch -r $< $@+
	mv $@+ $@

gz-doc: $(gz_docs)
rsync-doc:
	git set-file-times $(docs) $(txt)
	$(MAKE) gz-doc
	$(RSYNC) --chmod=Fugo=r -av $(rsync_docs) $(RSYNC_DEST)
clean-doc:
	$(RM) $(man1) $(man5) $(man7) $(gz_docs) $(docs_html) $(mantxt)

clean :: clean-doc
