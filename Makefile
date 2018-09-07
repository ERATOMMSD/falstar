.PHONY: all scanner compile doc test clean

SCANNER=src/main/scala/falstar/parser/Scanner.java

CP=lib/mvm.jar:lib/engine.jar:lib/util.jar

SRC=$(shell find src/main/scala -iname "*.scala") $(SCANNER)

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

clean:
	rm -fr bin
	rm -fr slprj
	rm -f outcmaes*.dat
	rm -f variablescmaes*.mat
	rm -f falstar.jar
	rm -f *.slxc *.mexa64
