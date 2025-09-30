.PHONY: test

all: build install

build: VERBOSITY_FLAG =
build:
	stack $(VERBOSITY_FLAG) build $(STACK_OPTS)
build/verbose:
	$(MAKE) $(MAKE_FLAGS) build VERBOSITY_FLAG="-v"

install: VERBOSITY_FLAG =
install:
	$(MAKE) $(MAKE_FLAGS) build
	mkdir  -p ./../bin
	stack $(VERBOSITY_FLAG) install $(STACK_OPTS) --local-bin-path ./../bin/
install/verbose:
	$(MAKE) $(MAKE_FLAGS) install VERBOSITY_FLAG="-v"

clean:
	rm *.cabal
	stack clean --full
	rm -rf ../bin
# If problems still persist after this, remove all GHC compilers in ~/.stack/programs/**/

test:
	stack test $(STACK_OPTS)

parser-info:
	stack exec happy -- -i src/Parser.y

ghci/irtester:
	stack ghci --main-is Troupe-compiler:exe:irtester --no-load

ghci/troupec:
	stack ghci --main-is Troupe-compiler:exe:troupec --no-load
