include ~/.falstar/matlab

.PHONY: all scanner install uninstall compile doc test clean hscc2019

PREFIX=/usr/local

SCANNER=src/main/scala/falstar/parser/Scanner.java
SRC=$(shell find src/main/scala -iname "*.scala") $(SCANNER)
BINDIR=bin
BIN=falstar.jar falstar falstar-session

SCALAC=./scala-2.12.8/bin/scalac
JAVAC=javac

ARCH2018=$(wildcard src/test/configuration/arch2018/*.cfg)
HSCC2019=$(wildcard src/test/configuration/hscc2019/*.cfg)

all: compile

compile: falstar.jar
doc: README.html
scanner: $(SCANNER)

test: falstar.jar
	./falstar src/test/configuration/test.cfg

install: $(BIN)
	install -m 755 falstar falstar-session $(PREFIX)
	install -m 644 falstar.jar $(PREFIX)

uninstall:
	rm -f $(addprefix $(PREFIX)/,$(BIN))

arch2018: $(ARCH2018:src/test/configuration/arch2018/%.cfg=results/arch2018/%.csv)
hscc2019: $(HSCC2019:src/test/configuration/hscc2019/%.cfg=results/hscc2019/%.csv)

falstar.jar: $(BINDIR) $(SRC)
	$(SCALAC) -d $(BINDIR) -cp $(MATLABROOT)/java/jar/engine.jar $(SRC)
	$(JAVAC)  -d $(BINDIR) -cp $(BINDIR) $(SCANNER)
	jar cf $@ -C $(BINDIR) .

%.html: %.md
	pandoc -s $^ -o $@

%.java: %.flex
	jflex -nobak $^

$(BINDIR):
	mkdir -p $(BINDIR)

results/arch2018/%.csv: src/test/configuration/arch2018/%.cfg falstar.jar
	./falstar $<

results/hscc2019/%.csv: src/test/configuration/hscc2019/%.cfg falstar.jar
	./falstar $<

clean:
	rm -fr $(BINDIR)
	rm -fr slprj
	rm -f outcmaes*.dat
	rm -f variablescmaes*.mat
	rm -f falstar.jar
	rm -f *.slxc *.mexa64
