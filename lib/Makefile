COMPILER=../bin/troupec

build:
	mkdir -p out

  # No dependencies
	$(COMPILER) ./Number.trp -l
	$(COMPILER) ./List.trp -l
	$(COMPILER) ./ThreadUtil.trp -l

  # Dependency on `List`
	$(COMPILER) ./ListPair.trp -l
	$(COMPILER) ./Unit.trp -l
	$(COMPILER) ./DeclassifyUtil.trp -l
	$(COMPILER) ./StencilVector.trp -l

  # Dependency on `List` and `Number`
	$(COMPILER) ./String.trp -l

  # Dependency on `Number`, `String`, and `List`
	$(COMPILER) ./Hash.trp -l

  # Dependency on `List`, `ListPair`, and `StencilVector`
	$(COMPILER) ./HashMap.trp -l
	$(COMPILER) ./HashSet.trp -l

clean:
	rm -rf out
