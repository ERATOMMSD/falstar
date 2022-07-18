.PHONY: all scanner install uninstall compile doc test clean hscc2019

PREFIX=/usr/local

SCANNER=falstar/src/falstar/parser/Scanner.java

SRC=$(shell find falstar/src -iname "*.scala")
BIN=falstar.jar falstar.sh falstar-session.sh falstar-config.sh

compile: falstar.jar

falstar.jar: $(SRC) $(SCANNER)
	mill falstar.assembly
	cp out/falstar/assembly.dest/out.jar $@

doc: README.html Validation.html

scanner: $(SCANNER)

test: falstar.jar
	./falstar.sh falstar/resource/configuration/test.cfg

install: $(BIN)
	install -m 755 falstar.sh falstar-session.sh falstar-config.sh $(PREFIX)
	install -m 644 falstar.jar $(PREFIX)

uninstall:
	rm -f $(addprefix $(PREFIX)/,$(BIN))

%.html: %.md
	pandoc -s $^ -o $@

%.java: %.flex
	jflex -nobak $^