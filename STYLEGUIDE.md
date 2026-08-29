# pine style

In general we follow the Google Common Lisp style guide:
https://google.github.io/styleguide/lispguide.xml

## Alexandria and other utility libraries

pine depends on `alexandria`, so `if-let`, `when-let` and friends are
available. Use the `alexandria:` prefix, `:import-from`, or a package-local
nickname.

Do not reach for utility functions you do not already see in the codebase.
`alexandria-2:line-up-first` is not used here. Prefer higher-order functions
over `alexandria:curry`.

## Data

`pine/data` is the vocabulary for maps, seqs and sets: `lookup`, `with`,
`without`, `size`, `keys`, `vals`, `do-map`, `do-pairs`, `as`. `lookup` and not
`at`: a node is *at* a path and a value is *looked up* in a collection, and one
word for both meant knowing which you had before you could read either. It
answers two values, because a collection may hold NIL and holding it is not the
same as holding nothing.

It is written over fset, and `src/data.lisp` is the only file that names `fset:`
or `sento.atomic:`. It says what a value is and where one is kept: `swap` / `cas`
/ `emptied`, and `table` for a registry, which is a map in a box with `all` /
`keep!` / `drop!` / `claim`. A hash table is for an identity map keyed by
objects, or for scratch one call owns; anything two threads read is a table.
Tests enforce both.

## Globals

Every `defvar`, `defparameter` and `defconstant` in one block after the
`defpackage`, before any function. A test enforces it.

State that outlives a call belongs in a `pine/data:box` and is replaced by a
pure function of what it held: `(d:swap! box #'1+)`. No locks.

## Threads

Sento is pine's concurrency, not a library it wraps. The image has one actor
system, made whether or not remoting is on.

Something that repeats is a tick on that system's wheel timer, through
`pine/run/timer:every-seconds`. Nothing sleeps in a loop.

Something that has to take messages in order is an endpoint, through
`pine/run/agent:agent` over `ac:actor-of`; `:pinned` where it may stand in a
fault. A receive never waits for an answer: `ask` from inside one is an error
rather than a hang.

`pine/run/task:spawn` makes a thread, and only for something that blocks: a pty
read, a child's stdout, a frontend's own loop.

## Compiler warnings

`make check` is warning-free. Keep it that way: a style-warning about an
undefined function is usually a load-order fault.

## Packages

The path to a package is its name, spelled the way a path is: `pine/edit/buffer`
lives in `src/edit/buffer.lisp` and declares itself there, and the systems are
spelled that way already. There is no manifest and there are no `package.lisp`
files; the `.asd` is the module structure.

A file opens with its own `defpackage`, then `in-package` on the line under
it, then the code. Name
what it needs with `:local-nicknames` or `:use` when it builds on a vocabulary
(`pine/ui/node`); qualify otherwise. A file's dependencies belong at its top,
where you are already looking.

A nickname means one package everywhere: `node` is `pine/fs/node` in every file
that says it, `widget` is `pine/ui/node`, `agent` is `pine/net/agent` and
`endpoint` is `pine/run/agent`. A nickname you have to look up is worse than
the name it shortened.

A helper with one caller is a local function. `flet` for a plain one, `labels`
where it recurses, written in the caller. What a package makes visible should
be what somebody else calls.

A file may only name packages that load before it. When two files need each
other, the layering is wrong: the one below declares a generic with a default
that does nothing, and the one above hangs a method on it — `pine/ui:confirming`,
`pine/run/command:asking`, `pine:opening`, `pine/text:visiting`. A generic and
not a variable, because a method says which case it answers for and a hook that
is set twice keeps whoever set it last.

## Nodes

Everything addressable is a `pine/fs/node` subclass answering the same six
generics: `contents`, `(setf contents)`, `nodes`, `resolve`, `describe`,
`leafp`. A buffer, a surface, a window, a provider's reading and a command are
all nodes. There is no registry beside the tree.

A provider's children must be the same objects each time: build them through
`node:child`, which memoizes. A node built fresh per call cannot be depended
on, so nothing reading it can ever be recomputed.

A provider says when the world behind it moved: `announces` names the shell
lines whose output stirs it, `every-seconds` the interval for the ones with no
stream. Nothing polls a provider that has a stream.

A write says so too. A provider that turns `(setf contents)` into an action on
the world has still moved, so `(setf contents)` announces once for every node:
without it a slider goes on showing what the world held before it was dragged.

A reading stands for a moment, not for the instant it was asked in. Two things
reading `/sys/cpu` a moment apart are asking about the same moment, so the
answer is remembered for a window rather than recomputed per read. The same
goes for a shell-out: `pine/provider/out:sh` remembers what a line said for a
breath, so a panel reading three things out of one command runs it once.

## Processes

A process pine starts dies with pine. A stream holds a pipe the image owns, so
stopping, crashing or being killed outright closes it and the child goes.
Anything that outlives the image that asked for it is a leak with no upper
bound: three hundred of them took a session's whole D-Bus allowance.

`pine/run/task:spawn` is for something that blocks. Everything else is an
endpoint or a tick.

## Frontends

The daemon lays out and the frontend paints. Whatever the daemon lays out for
is what the frontend has to be drawing at, so a frontend says its size and its
font in one place, and the frame carries the font it was laid out for.

A frontend is pushed when what it shows changes, on a tick, not on every
invalidation. The world behind a bar moves dozens of times a second and reads
the same; pushing that is what makes a pointer flicker.

What crosses back names a place, not a moment: a widget's id is where it sits
in its surface, so a click that crossed during a repaint still means the
button it was on.

A key has one name here whatever it was called where it came from. `C-SPC` in
a keymap is the key xkb hands over as `space`.

## Defining is registering

`defcommand` registers. `mode` registers. `defsurface` registers. Bindings are
`bind` forms beside the commands they name.

Each subsystem has one `install` that its boot calls, because a module that
compiles and has no caller is dead code. Nothing walks the system afterwards
guessing what to install.

## CLOS

A keyword in a `kind` slot switched on in two places is a class that has not
been written yet.

An empty class used only as a dispatch tag is a declaration written the wrong
way round: `(ns:serve :clock {...})` rather than `(defclass clock-server ...)`.

## Errors

Never swallow. No `ignore-errors` or bare `handler-case` that turns a fault
into `nil`. Either let it propagate, or route it through `pine/run/fault:attempt`,
which records the failure and surfaces it through the debugger buffer.

## Comments

There are none. `;;` and `;;;;` do not appear under `src/`, and a test enforces
it. What a banner was standing in for is a class name or a file name.

A docstring says what the code does now, and only where the name cannot. Never
narrate the change that produced it: no "now", "previously", "the bug this
fixes". Reasoning about a change belongs in the commit, not the source.

## Loop

Loop keywords take the colon: `(loop :for k :in list :collect ...)`.

## Naming

`c` is a client. `cli` is the command line interface. Check whether an
abbreviation already means something else in this tree before adopting it.

Lose redundant nouns: `place!` not `paint-tile!`. A constructor and the class
it makes may share a name across two packages -- `pine/ui/build:ring` makes a
`pine/ui/node:ring` -- and the constructor package shadows it explicitly.

## The surface language

`/a/b` is the sugar, and the whole of it. A config, the prompt and `pine eval`
all read it, so a spelling a config teaches is one that works where somebody
types it. `{...}`, `[...]` and `#{...}` were three more with no use anywhere;
a map is `(map ...)`, which reads in any file.

The substrate does not read in its own sugar: `src/data.lisp`, `src/said.lisp`
and `src/fs/` may not declare the readtable, because a namespace that needs the
syntax to say what a node is has the two the wrong way round. Everything above
that may, and the language declarations under `src/text/ts/lang/` do. A test
enforces it.

## Size

Nothing over 400 lines. A file is one idea, and a file that has outgrown that
is two files.

## Text

ASCII only. No em-dashes; use `--`, a colon, or a semicolon.
