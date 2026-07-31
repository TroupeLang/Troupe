# battallion

A terminal text editor written in Troupe. Under construction; the stages and what each one
delivers are in `_dev_planning/text-editor/mvp-plan.md`.

| File           | What it is                                                            |
|----------------|-----------------------------------------------------------------------|
| `Key.trp`      | Decodes terminal input bytes into `key` values, with a carry across chunk boundaries |
| `keydemo.trp`  | Runs `Key.decode` over a recorded byte stream and prints the keys      |

`keydemo.deps.json` pins `Key` by content hash. Regenerate it after changing `Key.trp` or
rebuilding the compiler:

```
./bin/troupec --update-deps examples/battallion/keydemo.trp
```

`make benchmark-deps` does the same for every program under `examples/` that imports a
program-relative module.

## Running

```
./local.sh examples/battallion/keydemo.trp --localonly
```
