.PHONY: clean

SCANNER=src/main/scala/parser/Scanner.java

CP=lib/mvm.jar:lib/engine.jar:lib/util.jar

SRC=$(shell find src/main/scala -iname "*.scala") $(SCANNER)

main.jar: bin $(SRC)
	scalac -d bin -cp lib/engine.jar $(SRC)
	jar cf $@ -C bin .

%.java: %.flex
	./jflex -nobak $^

bin:
	mkdir -p bin

clean:
	rm bin -r
	rm main.jar
