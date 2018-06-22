.PHONY: all scanner compile doc test clean

SCANNER=src/main/scala/parser/Scanner.java

CP=lib/mvm.jar:lib/engine.jar:lib/util.jar

SRC=$(shell find src/main/scala -iname "*.scala") $(SCANNER)

all: compile doc

compile: main.jar
doc: README.html
scanner: $(SCANNER)

main.jar: bin $(SRC)
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
	rm -f main.jar
	rm -f *.slxc *.mexa64

test: src/test/configuration/afc.cfg src/test/configuration/emb.cfg
	./run src/test/configuration/afc.cfg
	./run src/test/configuration/emb.cfg
	ls results/ARCH2018/
