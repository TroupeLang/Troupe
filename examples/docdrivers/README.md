# Document-processing drivers

Thin command-line programs that wrap the pure `string -> string` libraries (`Json`, `Markdown`,
`Template`) with whole-file I/O (`SimpleFileIO`). Each reads its input files, transforms them through
a library, writes the result, and echoes it to stdout. All paths are resolved relative to
`--io-root`; the runtime rejects any path that escapes that subtree.

| Driver                | Reads                     | Writes            | Library      |
|-----------------------|---------------------------|-------------------|--------------|
| `md2html.trp`         | a Markdown file           | HTML              | `Markdown`   |
| `jsonfmt.trp`         | a JSON file               | indented JSON     | `Json`       |
| `render-template.trp` | a template + a JSON file  | rendered output   | `Template` + `Json` |

## Running

From the repository root, with `local.sh` (add `--localonly` to skip p2p):

```sh
IOROOT="$(pwd)/examples/docdrivers/sample"

./local.sh examples/docdrivers/md2html.trp --localonly --io-root "$IOROOT" -- doc.md doc.html
./local.sh examples/docdrivers/jsonfmt.trp --localonly --io-root "$IOROOT" -- data.json data.pretty.json
./local.sh examples/docdrivers/render-template.trp --localonly --io-root "$IOROOT" -- greeting.tmpl data.json out.html
```

The program arguments after `--` are the input/output paths (relative to `--io-root`); the runtime
delivers them through `getCliArgs`. Output files are written inside `sample/`.

`render-template` is the combined demo: `SimpleFileIO` reads both files, `Json` parses `data.json`
into a native record, and `Template` renders `greeting.tmpl` against that record — autoescaping every
interpolation through `Html`. `md2html` likewise shows the safety story: a `javascript:` link in the
source is rendered as an inert anchor with no `href`.

## Sample inputs (`sample/`)

`doc.md`, `data.json`, `greeting.tmpl`. Generated outputs are not checked in.
