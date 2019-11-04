.PHONY: test
all:
	stack build $(STACK_OPTS)
	mkdir  -p ./../bin
	stack install $(STACK_OPTS) --local-bin-path ./../bin/
clear:
	rm *.cabal 
	stack clean

ghci-irtester:
	stack ghci --main-is Troupe-compiler:exe:irtester --no-load

ghci-troupec:
	stack ghci --main-is Troupe-compiler:exe:troupec --no-load
test: 
	stack test $(STACK_OPTS)
