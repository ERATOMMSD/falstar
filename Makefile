.PHONY: all scanner compile doc test clean hscc2019

SCANNER=src/main/scala/falstar/parser/Scanner.java
SRC=$(shell find src/main/scala -iname "*.scala") $(SCANNER)

CP=lib/mvm.jar:lib/engine.jar:lib/util.jar

HSCC2019=$(wildcard src/test/configuration/hscc2019/*.cfg)

all: compile doc

compile: falstar.jar
doc: README.html
scanner: $(SCANNER)

falstar.jar: bin $(SRC)
	scalac -d bin -cp lib/engine.jar $(SRC)
	javac  -d bin -cp bin $(SCANNER)
	jar cf $@ -C bin .

%.html: %.md
	pandoc -s $^ -o $@

%.java: %.flex
	./jflex -nobak $^

bin:
	mkdir -p bin

test: results/test.csv

results/%.csv: src/test/configuration/%.cfg falstar.jar
	./falstar $<

hscc2019: $(HSCC2019:src/test/configuration/hscc2019/%.cfg=results/hscc2019/%.csv)

results/hscc2019/%.csv: src/test/configuration/hscc2019/%.cfg falstar.jar
	./falstar $<

clean:
	rm -fr bin
	rm -fr slprj
	rm -f outcmaes*.dat
	rm -f variablescmaes*.mat
	rm -f falstar.jar
	rm -f *.slxc *.mexa64
