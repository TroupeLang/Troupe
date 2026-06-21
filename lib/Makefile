COMPILER=../bin/troupec

build:
	mkdir -p out

  # Transition hack; 2025-12-14; AA
	$(COMPILER) ./Missing.trp -l

  # No dependencies
	$(COMPILER) ./Number.trp -l
	$(COMPILER) ./List.trp -l
	$(COMPILER) ./ThreadUtil.trp -l
	$(COMPILER) ./timeout.trp -l
	$(COMPILER) ./Result.trp -l

  # Dependency on `Number`
	$(COMPILER) ./Duration.trp -l

  # Dependency on `List`
	$(COMPILER) ./ListPair.trp -l
	$(COMPILER) ./Unit.trp -l
	$(COMPILER) ./Map.trp -l
	$(COMPILER) ./Set.trp -l
	$(COMPILER) ./IfcUtil.trp -l
	$(COMPILER) ./StencilVector.trp -l
	$(COMPILER) ./FVal.trp -l

  # Dependency on `List` and `Number`
	$(COMPILER) ./String.trp -l

  # Dependency on `Number` and `String` (and transitively `List`)
	$(COMPILER) ./Hash.trp -l

  # Dependency on `List`, `String`, and `Result`
	$(COMPILER) ./Html.trp -l

  # Dependency on `Number`, `String`, `List`, `Result`, and `Duration`
	$(COMPILER) ./Time.trp -l

  # Dependency on `List`, `ListPair`, and `StencilVector`
	$(COMPILER) ./HashMap.trp -l
	$(COMPILER) ./HashSet.trp -l

clean:
	rm -rf out
