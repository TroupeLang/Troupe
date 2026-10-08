# Trapdoor

Trapdoor is a dungeon crawler in the manner of Rogue and NetHack, written in Troupe. The player
descends five floors of rooms and corridors, fights monsters, collects potions, scrolls, weapons and
armour, and wins by picking up the Ghost Light on the last floor.

A crawler hides the part of the floor the player has not seen. In most implementations the whole
floor sits in the same memory as the renderer, and the renderer chooses not to draw it; a rendering
mistake shows it. In Trapdoor the floor carries a confidentiality label, the terminal's channel
level is public, and the runtime refuses any write of the floor to the terminal. The player sees a
cell only after one process, holding one authority, has declassified it.

## Running

```
./local.sh examples/trapdoor/trapdoor.trp                  # a new game
./local.sh examples/trapdoor/trapdoor.trp -- 42            # the game of seed 42
./local.sh examples/trapdoor/replay.trp -- 42 'jjjl|i|'    # scripted keys; `|` prints the screen
./local.sh examples/trapdoor/bot.trp -- 42                 # an automatic player
./local.sh examples/trapdoor/leak.trp -- write             # a refused write (also: forge, release)
```

The terminal session needs 80 columns and 24 rows; rows beyond 24 show older messages.

| Key                       | Action                                                 |
|---------------------------|--------------------------------------------------------|
| `h j k l y u b n`, arrows | move one cell, or attack the monster standing there    |
| `.` `s`                   | wait a turn                                            |
| `,` `g`                   | pick up                                                |
| `>`                       | go down a trapdoor                                     |
| `i`                       | show the pack                                          |
| `q` `r` `w` `W` `d`       | drink, read, wield, wear, drop; then the item's letter |
| `?`                       | help                                                   |
| `Q`, CTRL-c               | quit                                                   |

A potion or scroll goes by its look ("fizzy potion") until one of its kind has been used or a scroll
of lore has been read. Which look stands for which kind is drawn per game.

## Files

| File           | Content                                                                    |
|----------------|----------------------------------------------------------------------------|
| `trapdoor.trp` | the main program: authorities, the channel level, the terminal session     |
| `World.trp`    | the dungeon master process; every downgrade of the game                    |
| `Monster.trp`  | a monster slot as a process, and the monster minds                         |
| `Rules.trp`    | the game as pure functions over one state value                            |
| `Dungeon.trp`  | floor generation                                                           |
| `Lore.trp`     | the public tables: bestiary, item kinds, names                             |
| `Rng.trp`      | the pseudo-random generator                                                |
| `Ui.trp`       | the player's model, key handling and view, as pure functions               |
| `replay.trp`   | a scripted session that prints screens as text                             |
| `bot.trp`      | an automatic player that reads only the player's model                     |
| `leak.trp`     | three attempts to print a floor row, two of which the runtime refuses      |

The terminal layer is Proscenium (`../proscenium`): `Term`, `Image`, `Style` and `Key`.

## Levels and processes

The program uses two levels. The level bot, `` `{}` ``, is public. The *dungeon level*,
`` `<dungeon;#root-integrity>` ``, is confidential and fully trusted; it differs from bot in
confidentiality only, so that moving a value from one to the other is a declassification and
involves no endorsement.

| Process           | File                    | Level of its state | Authority held                    |
|-------------------|-------------------------|--------------------|-----------------------------------|
| main, supervisor  | `trapdoor.trp`, `Term`  | bot                | root                              |
| kernel, renderer  | `Term` running `Ui`     | bot                | none (null-attenuated)            |
| dungeon master    | `World.trp`             | dungeon level      | `dm`, for the dungeon level only  |
| ten monster slots | `Monster.trp`           | dungeon level      | none (null-attenuated)            |

The main program lowers the terminal's channel level to bot with `setStdioLevel` before the session
starts. From then on the runtime checks every write to the terminal against bot, in whichever
process the write occurs.

The seed is raised to the dungeon level in the expression that draws it. The game state grows from
the seed through pure functions (`Rules.trp`, `Dungeon.trp`), so the state and everything computed
from it carry the dungeon level without any further labelling.

A turn runs as follows.

1. The kernel sends the dungeon master a command, a public triple such as `("MOVE", 1, 0)`.
2. The dungeon master applies `Rules.playerAct` to the state.
3. The dungeon master sends each monster slot a percept and receives one intent from each. Both
   are dungeon-level values.
4. The dungeon master applies `Rules.monstersAct`, which resolves the intents in slot order.
5. The dungeon master computes the player's view with `Rules.perceive`, declassifies it, lowers
   its blocking label, and sends the view to the kernel.

The dungeon master's control flow does not depend on the state: it performs these five steps for
every command, including a command that takes no game turn. The number of monster slots is a public
constant for the same reason. A slot whose monster is dead, or that never had one on this floor,
still receives a percept and still answers.

A monster slot keeps the monster's mind: whether it has noticed the player, its own random numbers,
its rhythm. Position and health stay with the dungeon master. A slot branches on its percept only
inside the pure function `Monster.decide`, so its loop runs at a public pc while its blocking label
stays at the dungeon level. A message sent by a slot therefore has dungeon-level presence, and a
plain `receive` in a public process never matches it (`../proscenium/Term.trp`, "WHO CAN RECEIVE
FROM THE KERNEL", records the measurement of that rule).

## Content of a view

A view is the only game data that becomes public. It contains:

- the cells in sight: the room the player stands in, walls included, and the eight neighbouring
  cells, with the items and live monsters on them;
- the player's position, health, strength, armour class, gold, experience, floor and turn count;
- the pack, each item under the name the player knows it by;
- the messages of the turn;
- whether the game is in play, lost or won, and what killed the player;
- after a scroll of magic mapping, the terrain of the whole floor, without items or monsters.

The player's map of the floor is assembled by `Ui.trp` from successive views. The dungeon master
holds no record of what the player has seen.

## Downgrade inventory

Every site below was found by searching the sources for the downgrade primitives and the library
wrappers that contain them. `Rules.trp`, `Dungeon.trp`, `Lore.trp`, `Rng.trp`, `Monster.trp` and
`Ui.trp` contain no value downgrade and no blocking-label downgrade.

**`World.trp`, `release`: `IfcUtil.declassifyDeep (Rules.perceive s, dm, IfcUtil.bot)`**

- Category: value.
- What moves: the view, from the dungeon level to bot, member by member.
- Who: `dm`, an authority for the dungeon level alone, passed to `World.run` by the main program.
- Where: after the monsters have acted, as the last computation of the turn.
- When: once per command, and once at the start of a game.
- Needed because: the terminal's level is bot, and the write of an undeclassified view is refused
  (`leak.trp -- write` shows the refusal on one row).
- This much and no more because: `Rules.perceive` computes the view from the sight rule, and the
  view is a new value that shares no structure with the state.
- A view consists of tuples, lists, strings and integers. `IfcUtil.declassifyDeep` does not
  traverse records, and a record inside a view would keep its fields at the dungeon level.
  `replay.trp` and `bot.trp` print screens built from views under the bot channel level, and each
  such print succeeds only if every member it used is public.

**`World.trp`, `release`: `blockdeclto (dm, IfcUtil.bot)`**

- Category: blocking label.
- What moves: the dungeon master's blocking label, from the dungeon level to bot.
- Who, where, when: as above, directly after the view is declassified.
- Needed because: a message's presence carries the sender's blocking label, and the kernel's
  receive matches only bot-level presence. `IfcUtil.declassifyDeep` already lowers the blocking
  label, with `blockdownto`, before it visits each member, and raises it again whenever it walks a
  list. Where the label ends therefore depends on the shape of the value: it ended at bot for the
  view of probe `p10.trp` and at the root level for the argument list of probe `args.trp`. The
  explicit call fixes the end state independently of the view's shape.
- What it releases: that the turn's computation terminated, and when.

**`World.trp`, `round` and `run`: `enableRangedReceive (IfcUtil.bot, lev, dm)`**

- Category: mailbox.
- What moves: the receive ceiling rises to the dungeon level at the open and returns to bot at the
  close. The runtime decides at the open whether the return is permitted.
- Who: `dm`.
- Where and when: around the collection of intents, opened and closed once per turn in
  straight-line code. `run` performs one trial open and close before the first game.
- Needed because: intents arrive with dungeon-level presence. A region left open for the whole
  life of the process was tried first; the command receive then raised the blocking label to the
  region's ceiling, the command arrived at the dungeon level, and the loop continued at a
  dungeon-level pc from the second turn on.
- The certification bit is read. If the trial open is refused, the dungeon master reports
  `REFUSED` to the player's process and ends.

**`Monster.trp`, `run`: `enableRangedReceive (IfcUtil.bot, lev, weak)`**

- Category: mailbox.
- The authority `weak` is attenuated to null, so the runtime does not certify the region: the
  certification bit is false and the region can never be closed. A monster slot receives in the
  region for as long as the program runs and never calls the close. The bit is not read, because
  no later operation depends on it.

**`trapdoor.trp`, `replay.trp`, `bot.trp`, `leak.trp`: `IfcUtil.declassifyDeep (getCliArgs authority, authority, IfcUtil.bot)` and `blockdeclto (authority, IfcUtil.bot)`**

- Category: value, then blocking label. `declassifyDeep` also lowers the blocking label on its
  own, in both dimensions, before each member it visits.
- What moves: the command-line arguments, from the root level to bot, and the blocking label that
  rose to the root level while the argument list was traversed.
- Who: the root authority, in the main program.
- Needed because: the program branches on the arguments before it starts the session. Without the
  second call the main thread's messages have root-level presence and the dungeon master never
  receives the first one.
- Released: what the operator typed on the command line, to the operator's own terminal.

**Main programs: `attenuate (authority, lev)` and `attenuate (authority, IfcUtil.null)`**

- Category: authority.
- The first call mints `dm`; the second mints `weak`. The root authority itself goes to
  `Term.run`, `setStdioLevel`, `getCliArgs` and `exit`, and to no game module.

**`leak.trp`, `release`: `declassify` and `blockdeclto`**

- Category: value and blocking label, on one row of a floor.
- The program exists to show the two refusals and the one permitted release side by side.

`Term.run` performs its own terminal ceremony under the root authority (`../proscenium/Term.trp`,
"LABELS"). At the bot channel level the intervals it opens are `[bot, bot]`.

Three places hold no downgrade although a design with one exists.

- The monster slots release nothing. They consume percepts and produce intents inside the dungeon
  level, and the dungeon master, which uses the intents, is at the same level.
- The player's side holds no authority. `Ui.trp` computes on public views only.
- The dungeon master declassifies no part of the state itself: not a cell, not a flag saying
  whether a turn passed. Only views are released.

## Checks performed

- `bot.trp` played seeds 1 to 8 to an outcome, between 920 and 1266 turns each, with no runtime
  error. It won seeds 1 and 4 and died on floor 4 in the other six.
- `leak.trp -- write` ends in `write to stdout above the stdio channel level`, `leak.trp -- forge`
  in `Not enough authority for declassification`, and `leak.trp -- release` prints the row.
- The terminal session was driven in tmux at 80 by 24: movement, the pack panel, the help panel,
  quitting with exit code 0.
- Probes for the label behaviour the design depends on are in
  `tests/_unautomated/claude/crawler-probes/`.

## ⚠ Caveats

- A seed given on the command line is known to whoever gave it, and it determines every floor.
  The labels track flows inside the program; they do not make a floor secret from an operator who
  chose the seed. Without a seed argument the seed is drawn with `random ()` and raised before it
  is bound.
- The blocking-label release makes the duration of a turn observable, and the computation of a
  turn branches on the hidden state.
- A command that takes no turn is visible in the view: the turn count and the position do not
  change. Walking into an unseen wall therefore tells the player that the cell is not walkable.
- The dungeon master waits for an intent from every slot without a timeout. A slot that died of a
  runtime error would stop the game.
- No golden test covers the game. `make test/examples` compiles it; behaviour was checked with
  `bot.trp`, `replay.trp` and tmux as listed above.
- The detected-dark palette was not exercised: tmux does not answer the background query, so every
  terminal run used the light palette.
- Start-up takes about nine seconds, spent in the compiler on the module graph that includes
  Proscenium (`examples/proscenium/chat.trp` measured 7.7 s on the same machine).
- Monster strength was tuned only against `bot.trp`, which has no tactics.
