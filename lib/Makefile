COMPILER=../bin/troupec

build:
	mkdir -p out

  # No dependencies
	$(COMPILER) ./Number.trp -l
	$(COMPILER) ./Option.trp -l
	$(COMPILER) ./ThreadUtil.trp -l
	$(COMPILER) ./timeout.trp -l
	$(COMPILER) ./VariantsDemo.trp -l

  # Dependency on `Option`
	$(COMPILER) ./BigInt.trp -l
	$(COMPILER) ./List.trp -l
	$(COMPILER) ./Outcome.trp -l

  # Dependency on `Outcome`
	$(COMPILER) ./SimpleFileIO.trp -l

  # Dependency on `Number`
	$(COMPILER) ./Duration.trp -l

  # Dependency on `List` and `Number`
	$(COMPILER) ./Vector.trp -l

  # Dependency on `List`
	$(COMPILER) ./Bench.trp -l
	$(COMPILER) ./ListPair.trp -l
	$(COMPILER) ./Unit.trp -l
	$(COMPILER) ./Map.trp -l
	$(COMPILER) ./Set.trp -l
	$(COMPILER) ./IfcUtil.trp -l
	$(COMPILER) ./StencilVector.trp -l

  # Dependency on `List` and `Number`
	$(COMPILER) ./String.trp -l

  # Dependency on `List`, `Option`, and `String`
	$(COMPILER) ./Pretty.trp -l

  # Dependency on `List`, `String`, `Outcome`, and `Pretty`
	$(COMPILER) ./Sexp.trp -l

  # Dependency on `Number` and `String` (and transitively `List`)
	$(COMPILER) ./Hash.trp -l

  # Dependency on `List`, `String`, and `Option`
	$(COMPILER) ./Html.trp -l

  # Dependency on `List`, `ListPair`, `String`, `Option`, and `Outcome`
	$(COMPILER) ./Json.trp -l

  # Dependency on `Html`, `String`, and `List`
	$(COMPILER) ./Svg.trp -l
	$(COMPILER) ./Markdown.trp -l

  # Dependency on `Html`, `String`, `List`, and `Outcome`
	$(COMPILER) ./Template.trp -l

  # Dependency on `Number`, `String`, `List`, `Option`, `Outcome`, and `Duration`
	$(COMPILER) ./Time.trp -l

  # Dependency on `List`, `ListPair`, and `StencilVector`
	$(COMPILER) ./HashMap.trp -l
	$(COMPILER) ./HashSet.trp -l

clean:
	rm -rf out
