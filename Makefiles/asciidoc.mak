 # -*- mode: makefile-gmake -*-

ASCIIDOC := /usr/bin/asciidoc
XMLTO := /usr/bin/xmlto

# man1
%.1.xml: %.1.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.1: %.1.xml
	$(XMLTO) man $<

# man2
%.2.xml: %.2.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.2: %.2.xml
	$(XMLTO) man $<

# man3
%.3.xml: %.3.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.3: %.3.xml
	$(XMLTO) man $<

# man4
%.4.xml: %.4.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.4: %.4.xml
	$(XMLTO) man $<

# man5
%.5.xml: %.5.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.5: %.5.xml
	$(XMLTO) man $<

# man6
%.6.xml: %.6.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.6: %.6.xml
	$(XMLTO) man $<

# man7
%.7.xml: %.7.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.7: %.7.xml
	$(XMLTO) man $<

# man8
%.8.xml: %.8.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.8: %.8.xml
	$(XMLTO) man $<

# man9
%.9.xml: %.9.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.9: %.9.xml
	$(XMLTO) man $<

