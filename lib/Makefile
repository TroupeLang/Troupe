COMPILER=../bin/troupec

build:
	mkdir -p out
  # Standard Library
	$(COMPILER) ./Number.trp -l
	$(COMPILER) ./List.trp -l
	$(COMPILER) ./ListPair.trp -l
	$(COMPILER) ./String.trp -l
	$(COMPILER) ./Hash.trp -l
	$(COMPILER) ./Unit.trp -l
	$(COMPILER) ./StencilVector.trp -l
	$(COMPILER) ./HashMap.trp -l
	$(COMPILER) ./HashSet.trp -l
  # Old stuff, here be dragons...
	$(COMPILER) ./nsuref.trp -l
	$(COMPILER) ./printService.trp -l
	$(COMPILER) ./timeout.trp -l
	$(COMPILER) ./NetHealth.trp -l
	$(COMPILER) ./declassifyutil.trp -l
	$(COMPILER) ./stdio.trp -l
	$(COMPILER) ./raft.trp -l
	$(COMPILER) ./raft_debug.trp -l
	$(COMPILER) ./bst.trp -l
	$(COMPILER) ./localregistry.trp -l

clean:
	rm -rf out
