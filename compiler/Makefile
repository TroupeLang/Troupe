.PHONY: test

build: VERBOSITY_FLAG =
build:
	stack $(VERBOSITY_FLAG) build $(STACK_OPTS)
	mkdir  -p ./../bin
	stack $(VERBOSITY_FLAG) install $(STACK_OPTS) --local-bin-path ./../bin/
build/verbose:
	$(MAKE) $(MAKE_FLAGS) build VERBOSITY_FLAG="-v"

clean:
	rm *.cabal
	stack clean --full
	rm -rf ../bin
# If problems still persist after this, remove all GHC compilers in ~/.stack/programs/**/

ghci-irtester:
	stack ghci --main-is Troupe-compiler:exe:irtester --no-load

ghci-troupec:
	stack ghci --main-is Troupe-compiler:exe:troupec --no-load

test:
	stack test $(STACK_OPTS)

parser-info:
	stack exec happy -- -i src/Parser.y
