# md-navigator

Compiles a directory of Markdown documents into a static HTML mirror tree that can be browsed from
the filesystem, with no server. One page per document, mirroring the source layout; every page
carries a sidebar with the whole tree and a table of contents built from its own headings. Links
between documents are rewritten from `.md` to `.html`.

## Configuration

```json
{
  "sourceDir": "_dev_planning",
  "outputDir": "out/md-navigator",
  "title": "Troupe development planning",
  "commentsFile": "comments.md",
  "archiveDir": "_archive"
}
```

| Key            | Meaning                                                  | Default            |
|----------------|----------------------------------------------------------|--------------------|
| `sourceDir`    | Directory of Markdown documents, searched recursively    | `docs`             |
| `outputDir`    | Where the generated pages are written                    | `out/md-navigator` |
| `title`        | Site title, shown in the sidebar and page titles         | `Documentation`    |
| `commentsFile` | Where review comments are meant to end up; read back for context if it exists | `comments.md` |
| `archiveDir`   | Directory of superseded documents, relative to `sourceDir`; hidden from the tree unless asked for. Empty string turns the notion off | `_archive` |

All paths are relative to `--io-root`, which must contain them. The configuration file is named as
the first program argument, defaulting to `examples/md-navigator/config.json`.

## Running

From the repository root:

```sh
./local.sh examples/md-navigator/md-navigator.trp --localonly --io-root "$(pwd)" \
    -- examples/md-navigator/config.json
```

Then open the file the run prints. Entries whose name begins with `.` are skipped, so a nested
`.git` directory is not walked.

## Modules

| File                | Role                                                                     |
|---------------------|--------------------------------------------------------------------------|
| `md-navigator.trp`  | Configuration, the tree walk, link resolution, page assembly, main       |
| `Theme.trp`         | The stylesheet and the themes it is parameterised by                     |
| `Client.trp`        | The scripts the generated pages carry                                    |
| `Path.trp`          | Pure path arithmetic                                                     |
| `md-navigator.deps.json` | Content-hash pins for the three modules                             |

The three are program-relative modules. Their pins are hashes over codegened IR, so after editing
any of them run `bin/troupec --update-deps examples/md-navigator/md-navigator.trp` before building;
a normal build enforces the pins.

`Theme.trp` and `Client.trp` exist because they are data rather than control flow — together they
are most of the program's line count and almost none of its logic.

## What is generated

| Page          | Contents                                                                    |
|---------------|-----------------------------------------------------------------------------|
| one per `.md` | The document rendered, its source, sidebar, table of contents, modification date |
| `index.html`  | Every document with title, path and modification date; sortable by any column |
| `links.html`  | The link report                                                             |
| `search.html` | Full-text search across all documents                                       |
| `comments.html` | Review comments collected while browsing, and their `comments.md` form     |

A source document may itself render to `index.html` — `_dev_planning/index.md` does. When that
happens the generated page steps aside to `_mdnav-index.html` rather than overwriting the document,
and the run prints the name it used. The same applies to the other generated pages.

## Navigating

| Feature                | Detail                                                                  |
|------------------------|-------------------------------------------------------------------------|
| Breadcrumbs            | Overview / directory / document; a directory links to its own index      |
| Previous and next      | In tree order, at the foot of each document; `[` and `]` on the keyboard |
| Directory tree         | Collapsible, remembered per directory; the current document's ancestors are always open |
| Expand / collapse all  | Buttons above the tree                                                  |
| Filter box             | Substring match on filenames; `/` focuses it, `Escape` clears it         |
| Time filters           | 1h / 24h / 7d / 30d, or a custom number of hours, days or weeks; "All" cancels |
| Recency and filters agree | The marker tiers are exactly the filter windows, so a document matching a filter always carries a mark |
| Archive                | Documents under `archiveDir` are out of the tree until the "Show archive" button asks for them; the choice is remembered |
| Recency markers        | A dot on a four-slot track flush with the right edge, running older to newer left to right: the last month, week, day, hour; no dot means older than a month |
| Document counts        | Beside each directory name                                              |
| Table of contents      | Follows the reading position; hidden below 1100px wide                   |
| Heading anchors        | A `#` appears on hover, for linking to a section                         |
| Scroll positions       | Both the tree's and each document's own are remembered                   |
| Widths                 | Drag the sidebar's and the main column's edges; double-click a grip to reset |

Recency is modification time only. Whether a document was *added* recently is not derivable here: a
checkout writes a file and so sets both its modification and creation time, which makes a restored
file indistinguishable from a new one. Git is the only source that knows the difference.

## Themes

`Theme.trp` defines the stylesheet so that **no rule names a colour** — every one reads a custom
property, and a theme is a set of values for those properties. Light, dark and sepia ship; the
picker sits in the sidebar and remembers the choice, and `Auto` follows the operating system.

Adding a theme means adding one `*Vars` binding and one `:root[data-theme=...]` line in `themeCss`.
Nothing else in the program changes.

The semantic colours sit on the blue-violet axis, which dichromatic vision preserves: `--recent`
holds its hue under both protanopia and deuteranopia, where a vermillion simulates to a murky
olive. Magnitude is carried as three strengths of that one hue rather than as three hues.

More importantly, no signal depends on colour at all. Recency is a dot's **position** on a
four-slot track, which no form of colour blindness can affect; the hue's strength repeats the
same information so that either cue alone is sufficient. The track sits on the right because the
left of a tree already carries nesting, and two horizontal-position signals on the same side
compete with one another. Broken links and the delete action are
named in words, since red reads as a neutral grey to a red-green dichromat.

## Link handling

Each link is resolved against the document that contains it:

| Category             | Treatment                                                            |
|----------------------|----------------------------------------------------------------------|
| Another document     | Rewritten to the generated page; a `#fragment` is preserved          |
| A directory          | Resolved to its `index.md` or `README.md` when one exists            |
| A non-document file  | Points at the source file, reached from the output tree              |
| Above the source dir | Points at the real file when one exists inside `--io-root`           |
| Nothing              | Left unchanged and listed as broken                                  |
| A URL or `#anchor`   | Left unchanged                                                       |

No file is copied into the output tree, so a text-mode copy cannot corrupt a binary asset; links to
non-documents point back at the source instead.

Archived documents are generated and linked exactly like the rest — only their place in the tree is
conditional. A document being read is always shown in the tree even when it is archived and the
archive is hidden, since otherwise opening one would hide your own location. Note that the report's
unreferenced list counts archived documents too, and being unreferenced is often the normal state
for an archive: 3 of the 7 currently listed are archived.

## Source view and review comments

Each document page holds both the rendered form and its Markdown source, switched by the toggle at
the top and remembered across pages. In the source view each line is numbered.

Selecting lines there opens a composer, and the comment is stored against the document, the line
range, and the text as it read at that moment — so an entry can still be located if the lines have
since moved. The selected lines stay highlighted while you type, and lines carrying a comment are
marked when you return.

Comments accumulate in browser storage as you browse. `comments.html` lists them and renders them as
markdown to download or copy:

```markdown
## _dev_planning/index.md:14-16

- created: 2026-07-26T09:12:33.123Z

```
| `LANDED`  | The described work is in the tree on `dev-v1_1-modules` |
```

This row is stale.
```

The quote is fenced with enough backticks to survive whatever the quoted text contains.

A page opened from the filesystem cannot append to a file on disk, so exporting is a download or a
copy rather than a write; move it to `commentsFile`, or paste it there. If the file exists when the
program runs, its contents are shown on the comments page for context.

Commenting works in the source view only. Anchoring a comment to a position in the *rendered* HTML
would need source positions carried through the Markdown AST, which the library does not record.
