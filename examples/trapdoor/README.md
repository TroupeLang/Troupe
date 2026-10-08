# Trapdoor

Trapdoor is a dungeon crawler in the manner of Rogue and NetHack, written in Troupe. The player
descends five floors of rooms and corridors, fights monsters, collects potions, scrolls, weapons and
armour, and wins by picking up the Ghost Light on the last floor.

```
You hit the kobold.  The kobold hits you.

       ------------
       |..........+###                                    -----------------
       |..........|  #     ---------------------          |..>............|
       |..........|  #     |...................|          |...............|
       |..........|  ######+...................|          |...............|
       ---+--------        ----------------+----          --------------+--
                                    ########               ##############
                                 ---+---------             #
                                 |...........|         ----+-------------
                                 |...........|         |................|
                                 |...........|         |................|
                                 |...........|         |................|
                                 --------+----         -----+------------
                                ##########                  #
 ------+------------        ----+--                         #
 |?................|        |.....|                         ######
 |.........k@......+########+.....+############################--+---
 -------------------        |.....|                           #+....|
                            -------                            |....|
                                                               ------
 Floor 1   $ 0   HP 17(20)   Str 10   AC 4   Level 1 (3 xp)   Turn 263
 ? help   i pack   Q quit
```

The screen above is the game of seed 7 after 263 commands of the automatic player, as
`bot.trp -- 7 263` prints it. The player is `@`, the letter `k` is a kobold, `?` is a scroll, and
`>` is the trapdoor to the next floor. A blank cell is either rock or a cell that the player has
not seen.

A crawler hides the part of the floor the player has not seen. In most implementations the whole
floor sits in the same memory as the renderer, and the renderer chooses not to draw it; a rendering
mistake then shows it. In Trapdoor the floor carries a confidentiality label, the terminal's
channel level is public, and the runtime refuses any write of the floor to the terminal. The player
sees a cell only after one process, which holds one authority, has declassified it.

## Programs

All commands run from the repository root.

```
./local.sh examples/trapdoor/trapdoor.trp                  # a new game
./local.sh examples/trapdoor/trapdoor.trp -- 42            # the game of seed 42
./local.sh examples/trapdoor/replay.trp -- 42 'jjjl|i|'    # scripted keys; `|` prints the screen
./local.sh examples/trapdoor/bot.trp -- 42                 # an automatic player
./local.sh examples/trapdoor/bot.trp -- 42 300             # the same, stopped after 300 commands    
./local.sh examples/trapdoor/leak.trp -- write             # a refused write (also: forge, release)
```

The terminal session needs 80 columns and 24 rows, and it shows older messages in any rows beyond
24. The programs `replay.trp` and `bot.trp` run without a terminal session and print screens as
plain text, 24 rows of 80 columns. A seed and a key string determine the output of `replay.trp`, and
a seed and a limit determine the output of `bot.trp`.

## Keys

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

A potion or scroll goes by its look ("smoky potion") until one of its kind has been used or a scroll
of lore has been read. The game draws afresh, for each new game, which look stands for which kind.

## Screens

The screens in this document are output of `bot.trp` and `replay.trp`, copied without changes
except that trailing spaces are removed. The terminal session draws the same characters and adds
renditions: remembered cells are gray, monsters are bold in a violet accent, and items are amber.

The next two screens show a release that the player causes. The first screen is the game of seed 4
after 13 commands of the automatic player (`bot.trp -- 4 13`): the player has seen two rooms and a
bat, `b`, and has picked up a scroll.

```
You now have a scroll labelled SOTTO VOCE.








                                                      -----------------+---
                                                      |...................|
                                                      |....@..............|
                                                      +..............b....|
                                                      |...................|
                                                      -----+---------------
                                                           #
                                                           #####
                                                      ---------+-----------
                                                      |...................|
                                                      |...................|
                                                      |...................|
                                                      ---------------------
 Floor 1   $ 0   HP 20(20)   Str 10   AC 2   Level 1 (0 xp)   Turn 13
 ? help   i pack   Q quit
```

The fourteenth command reads the scroll, which is a scroll of magic mapping (`bot.trp -- 4 14`).
The view of that turn contains the terrain of the whole floor. The map shows no item and no monster
outside the player's room, because a magic-mapping view carries terrain only.

```
You read the scroll labelled SOTTO VOCE.
A plan of the floor forms in your mind.
                                                          ------------------
      ------------------    -------------                 |................|
      |................+####+...........+#                |................|
      |................|    |...........|#################+................|
      |................|    -------------                 |................|
      ------------------                                  -------------+----
                                                                       #
         --------             -------------------     -----------------+---
         |......|             |.................|     |...................|
         |......|             |.................+#### |....@........b.....|
         |......+##           |.................|   ##+...................|
         |...>..| ############+.................|     |...................|
         ---+----             -----------+-------     -----+---------------
      #######                #############                 #
  ----+-------------       --+----------                   #####
  |................|       |...........|              ---------+-----------
  |................|       |...........|              |...................|
  |................|       |...........|              |...................|
  |................+#######+...........|              |...................|
  ------------------       -------------              ---------------------
 Floor 1   $ 0   HP 20(20)   Str 10   AC 2   Level 1 (0 xp)   Turn 14
 ? help   i pack   Q quit
```

The last screen shows the pack panel, which the key `i` opens. The screen is the same game after
169 commands, and `replay.trp` produced it from the keys that `bot.trp` pressed for those commands,
followed by `i`. The scroll read on that turn was a scroll of lore; before that turn, item `d` went
by the name "smoky potion".

```
You read the scroll labelled DEUS EX MACHINA.
You recognise everything in your pack.
                                                          ------------------
     Your pack                 ----------                 |...@............|
                               .........+#                |................|
     a - ) +0 dagger (wielded) .........|#################+................|
     b - [ +0 leather armour   ----------                 |................|
     c - [ +1 ring mail (worn)                            -------------+----
     d - ! potion of might                                             #
                               ------------------     -----------------+---
         |......|             |.................|     |...................|
         |......|             |.................+#### |...................|
         |......+##           |.................|   ##+...................|
         |...>..| ############+.................|     |...................|
         ---+----             -----------+-------     -----+---------------
      #######                #############                 #
  ----+-------------       --+----------                   #####
  |................|       |...........|              ---------+-----------
  |................|       |...........|              |...................|
  |................|       |...........|              |...................|
  |................+#######+...........|              |...................|
  ------------------       -------------              ---------------------
 Floor 1   $ 0   HP 19(20)   Str 10   AC 4   Level 1 (2 xp)   Turn 169
 ? help   i pack   Q quit
```

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

The terminal layer is Proscenium ([`../proscenium`](../proscenium)): the modules `Term`, `Image`,
`Style` and `Key`.

## Levels and processes

The program uses two levels. The level *bot*, written `` `{}` ``, is public. The *dungeon level*,
written `` `<dungeon;#root-integrity>` ``, is confidential and fully trusted. The dungeon level
differs from bot in confidentiality only, so a move of a value from the dungeon level to bot is a
declassification and involves no endorsement.

The game runs as the following processes. The *kernel* and the *renderer* are the two Proscenium
processes that run the application's update function and write frames to the terminal. The *dungeon
master* is the process that holds the game state. A *monster slot* is a process that decides the
moves of one monster.

| Process           | File                    | Level of its state | Authority held                    |
|-------------------|-------------------------|--------------------|-----------------------------------|
| main, supervisor  | `trapdoor.trp`, `Term`  | bot                | root                              |
| kernel, renderer  | `Term` running `Ui`     | bot                | none (null-attenuated)            |
| dungeon master    | `World.trp`             | dungeon level      | `dm`, for the dungeon level only  |
| ten monster slots | `Monster.trp`           | dungeon level      | none (null-attenuated)            |

The main program lowers the terminal's channel level to bot with `setStdioLevel` before the session
starts. From then on the runtime checks every write to the terminal against bot, in whichever
process the write occurs.

The main program raises the seed to the dungeon level in the expression that draws it. The game
state grows from the seed through pure functions (`Rules.trp`, `Dungeon.trp`), so the state and
every value computed from it carry the dungeon level without any further labelling.

## Turn protocol

The dungeon master and the monster slots exchange two kinds of value. A *percept* is what one
monster senses: its own position, the player's position, whether it can sense the player, and which
neighbouring cells it could step into. An *intent* is the step a monster wants to take. A *view* is
what the player sees and knows after a turn; [Content of a view](#content-of-a-view) lists its
members.

A turn consists of five steps.

1. The kernel sends the dungeon master a command, which is a public triple such as
   `("MOVE", 1, 0)`.
2. The dungeon master applies `Rules.playerAct` to the state.
3. The dungeon master sends each monster slot a percept and receives one intent from each. Percepts
   and intents are dungeon-level values.
4. The dungeon master applies `Rules.monstersAct`, which resolves the intents in slot order.
5. The dungeon master computes the view with `Rules.perceive`, declassifies it, lowers its own
   blocking label, and sends the view to the kernel.

The control flow of the dungeon master does not depend on the state. The dungeon master performs
the five steps for every command, including a command that takes no game turn, and the state
records whether a turn passed. The number of monster slots is a public constant for the same
reason: a slot whose monster is dead, or that has no monster on the current floor, still receives a
percept and still answers.

A monster slot keeps the mind of its monster: whether the monster has noticed the player, its own
random numbers, and its rhythm. The position and the health of the monster stay with the dungeon
master. A slot branches on its percept only inside the pure function `Monster.decide`, so the loop
of the slot runs at a public pc while its blocking label stays at the dungeon level. A message that
a slot sends therefore has dungeon-level presence, and a plain `receive` in a public process does
not match such a message. The probe `p11.trp` shows this for one sender and one receiver, and the
header of [`../proscenium/Term.trp`](../proscenium/Term.trp), under "WHO CAN RECEIVE FROM THE
KERNEL", records the earlier measurement of the same rule.

## Content of a view

A view is the only game data that becomes public. A view contains

- the cells in sight, which are the room the player stands in, walls included, and the eight
  neighbouring cells, with the items and the live monsters on them;
- the player's position, health, strength, armour class, gold, experience, floor and turn count;
- the pack, with each item under the name the player knows it by;
- the messages of the turn;
- the status of the game (in play, lost or won) and the cause of the player's death;
- after a scroll of magic mapping, the terrain of the whole floor, without items or monsters.

The module `Ui.trp` assembles the player's map of the floor from successive views. The dungeon
master holds no record of what the player has seen.

The intended policy is that an observer of the terminal learns the sequence of views, one per
command, together with what the blocking-label release adds
([The blocking-label release](#the-blocking-label-release)). This statement is the design's intent
and is not proven; no part of it has been checked against the formal model of Troupe. The player
chooses the commands, so the player chooses which views the game computes.

## Downgrade inventory

The inventory lists every site at which the program downgrades, mints an authority, or opens a
receive region. The sites were found by a search of the sources for the downgrade primitives and
for the library wrappers that contain them. The modules `Rules.trp`, `Dungeon.trp`, `Lore.trp`,
`Rng.trp`, `Monster.trp` and `Ui.trp` contain no value downgrade and no blocking-label downgrade.

### The view release

The function `release` in `World.trp` calls `IfcUtil.declassifyDeep (Rules.perceive s, dm,
IfcUtil.bot)`. The call moves the view from the dungeon level to bot, member by member. The
authority is `dm`, which covers the dungeon level alone and which the main program passes to
`World.run`. The call happens once per command and once at the start of a game, as the last
computation of the turn.

The release is needed because the terminal's level is bot: the runtime refuses the write of an
undeclassified value, as `leak.trp -- write` shows for one row of a floor. The release covers the
view and no part of the state, because `Rules.perceive` builds the view as a new value that shares
no structure with the state.

A view consists of tuples, lists, strings and integers. The function `IfcUtil.declassifyDeep` does
not traverse records, so a record inside a view would keep its fields at the dungeon level. The
programs `replay.trp` and `bot.trp` print screens built from views while the channel level is bot,
and each such print succeeds only if every member it uses is public.

### The blocking-label release

The function `release` then calls `blockdeclto (dm, IfcUtil.bot)`. The call moves the blocking
label of the dungeon master from the dungeon level to bot, under the same authority and at the same
point of the turn. The call releases that the computation of the turn terminated, and when.

The release is needed because the presence of a message carries the blocking label of its sender,
and the receive of the kernel matches bot-level presence only. The function
`IfcUtil.declassifyDeep` already lowers the blocking label, with `blockdownto`, before it visits
each member, and the traversal of a list raises the label again. The level at which the label ends
therefore depends on the shape of the value: the label ended at bot for the view of probe `p10.trp`
and at the root level for the argument list of probe `args.trp`. The explicit call
makes the end state independent of the shape of the view.

### The receive region of the dungeon master

The functions `round` and `run` in `World.trp` call `enableRangedReceive (IfcUtil.bot, lev, dm)`.
The call raises the receive ceiling of the dungeon master to the dungeon level, and the matching
close returns the ceiling to bot. The runtime decides at the open whether the return is permitted,
under the authority `dm`. The function `round` opens and closes the region once per turn, in
straight-line code around the collection of intents. The function `run` performs one trial open and
close before the first game.

The region is needed because intents arrive with dungeon-level presence. The region must not stay
open across turns. In probe `p7.trp` the region stays open for the whole life of the process; the
command receive then raises the blocking label to the ceiling of the region, the command arrives at
the dungeon level, and the loop continues at a dungeon-level pc from the second turn on.

The program reads the certification bit that the open returns. When the runtime refuses the trial
open, the dungeon master reports `REFUSED` to the player's process and ends.

### The receive region of a monster slot

The function `run` in `Monster.trp` calls `enableRangedReceive (IfcUtil.bot, lev, weak)`. The
authority `weak` is attenuated to null, so the runtime does not certify the region: the
certification bit is false (probe `p9.trp`) and the region can never be closed. A monster slot
receives in the region for as long as the program runs and never calls the close. The slot does not
read the bit, because no later operation of the slot depends on it.

### The command-line arguments

The four main programs call `IfcUtil.declassifyDeep (getCliArgs authority, authority,
IfcUtil.bot)` and then `blockdeclto (authority, IfcUtil.bot)`. The first call moves the
command-line arguments from the root level to bot, under the root authority. The second call
lowers the blocking label, which the traversal of the argument list leaves at the root level
(probe `args.trp`).

Both calls are needed because each program branches on its arguments before it starts the session.
Without the second call the messages of the main thread have root-level presence, and the dungeon
master never receives the first one. The two calls release what the operator typed on the command
line, and the release goes to the operator's own terminal.

### The minted authorities

Each main program calls `attenuate (authority, lev)`, which mints `dm`, and `attenuate (authority,
IfcUtil.null)`, which mints `weak`. The root authority itself goes to `Term.run`, `setStdioLevel`,
`getCliArgs` and `exit`, and to no game module.

The function `Term.run` performs its own terminal ceremony under the root authority; the header of
[`../proscenium/Term.trp`](../proscenium/Term.trp) describes it under "LABELS". At the bot channel
level every interval that the ceremony opens is `[bot, bot]`.

### The release in the demonstration program

The function `release` in `leak.trp` calls `declassify` and `blockdeclto` on one row of a floor.
The program exists to show two refused attempts beside the one permitted release.

### Absent downgrades

Three places hold no downgrade although a design with one exists.

- The monster slots release nothing. A slot consumes percepts and produces intents inside the
  dungeon level, and the dungeon master, which uses the intents, is at the same level.
- The player's side holds no authority. The module `Ui.trp` computes on public views only.
- The dungeon master declassifies no part of the state itself, neither a cell nor the flag that
  records whether a turn passed. The dungeon master releases views only.

## Checks performed

- The suite `make ci` passes with the example in the tree; its `test/examples` step compiles every
  program of the example.
- The program `bot.trp` played seeds 1 to 8 to an outcome, between 920 and 1266 turns each, with no
  runtime error. It won seeds 1 and 4 and died on floor 4 in the other six.
- The command `leak.trp -- write` ends in the runtime error `write to stdout above the stdio
  channel level`, the command `leak.trp -- forge` ends in `Not enough authority for
  declassification`, and the command `leak.trp -- release` prints the row.
- The terminal session was driven in tmux at 80 by 24: movement, the pack panel, the help panel,
  and quitting with exit code 0.
- The probes for the label behaviour that the design depends on are in
  [`tests/_unautomated/claude/crawler-probes`](../../tests/_unautomated/claude/crawler-probes).
  Each probe file ends with the output observed.

## ⚠ Caveats

- A seed given on the command line is known to whoever gave it, and the seed determines every
  floor. The labels track flows inside the program; the labels do not make a floor secret from an
  operator who chose the seed. Without a seed argument the program draws the seed with `random ()`
  and raises it before binding it.
- The built-in `random ()` returns a public value, and the program raises that value. No public
  code of the game calls `random ()` afterwards; the labels do not enforce that.
- The blocking-label release makes the duration of a turn observable, and the computation of a
  turn branches on the hidden state.
- A command that takes no turn is visible in the view: the turn count and the position do not
  change. A step into an unseen wall therefore tells the player that the cell is not walkable.
- The game runs at full integrity throughout, so the labels constrain confidentiality only.
  The module `Rules.trp` validates commands by ordinary checks.
- The dungeon master waits for an intent from every slot without a timeout. A slot that died of a
  runtime error would stop the game.
- No golden test covers the game. The step `make test/examples` compiles it; its behaviour was
  checked with `bot.trp`, `replay.trp` and tmux, as [Checks performed](#checks-performed) lists.
- The palette for a dark background was not exercised. The multiplexer tmux does not answer the
  background query, so every terminal run used the palette for a light background.
- Start-up takes about nine seconds, which the compiler spends on the module graph that includes
  Proscenium; `examples/proscenium/chat.trp` compiled in 7.7 s on the same machine.
- The strength of the monsters was tuned only against `bot.trp`, which has no tactics.
