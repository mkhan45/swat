all: sax

default: all

sax: always
	install _build/default/bin/$@.exe $@
	strip $@

always:
	dune build --profile=release

clean:
	dune clean
	rm -f sax
	rm -f lab1.zip

SRC_DIRS := ./lib ./bin

FILES := \
	Makefile \
	dune-project \
	.gitignore \
	$(shell find $(SRC_DIRS) -name '*.ml' -or -name '*.mli' -or -name '*.mll' -or -name '*.mly' -or -name 'dune')

handin: lab1.zip

lab1.zip: $(FILES)
	zip $@ $^

.PHONY: sax clean native handin
