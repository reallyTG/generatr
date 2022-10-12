BEAR := $(shell command -v bear 2> /dev/null)
NAME := generatr
R    ?= R

ifdef BEAR
	BEAR := $(BEAR)
endif

.PHONY: all build check clean document test install

all: install

build: document
	$(R) CMD build .

check: build
	$(R) CMD check $(NAME)*tar.gz

clean:
	-rm -f $(NAME)*tar.gz
	-rm -fr $(NAME).Rcheck
	-rm -rf src/*.o src/*.so

document:
	$(R) -e 'devtools::document()'

test:
	$(R) -e 'devtools::test()'

install:
	$(R) CMD INSTALL .
