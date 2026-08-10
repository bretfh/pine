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

`pine.data` is the vocabulary for maps, seqs and sets: `at`, `with`, `without`,
`size`, `keys`, `vals`, `do-map`, `do-seq`, `as`. It is written over fset, and
`src/data.lisp` is the only file that names `fset:`. Reach for a CL list or
hash table where one is the right shape; reach for `pine.data` where the value
is shared, nested, or crosses a thread.

## Globals

Every `defvar`, `defparameter` and `defconstant` in one block after the
`defpackage`, before any function. A test enforces it.

State that outlives a call belongs in a `pine.run.cell` and is replaced by a
pure function of what it held: `(c:swap cell #'1+)`. No locks.

## Compiler warnings

`make check` is warning-free. Keep it that way: a style-warning about an
undefined function is usually a load-order fault.

## Packages

The path to a package is its name. `pine.edit.buffer` lives in
`src/edit/buffer.lisp` and declares itself there. There is no manifest and
there are no `package.lisp` files; the `.asd` is the module structure.

A file opens with its own `defpackage`, then `in-package`, then the code. Name
what it needs with `:local-nicknames` or `:use` when it builds on a vocabulary
(`pine.ui.node`); qualify otherwise. A file's dependencies belong at its top,
where you are already looking.

A file may only name packages that load before it. When two files need each
other, the layering is wrong: one of them takes a hook (`pine.ts.parser:*on-parse*`,
`pine.edit.buffer:*on-current*`, `pine.ui.build:*asking*`) and the other fills
it in.

## Nodes

Everything addressable is a `pine.fs.node` subclass answering the same six
generics: `contents`, `(setf contents)`, `nodes`, `resolve`, `describe`,
`leafp`. A buffer, a surface, a window, a provider's reading and a command are
all nodes. There is no registry beside the tree.

A provider's children must be the same objects each time: build them through
`node:child`, which memoizes. A node built fresh per call cannot be depended
on, so nothing reading it can ever be recomputed.

A provider says when the world behind it moved: `announces` names the shell
lines whose output stirs it, `every-seconds` the interval for the ones with no
stream. Nothing polls a provider that has a stream.

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
into `nil`. Either let it propagate, or route it through `pine.run.fault:attempt`,
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
it makes may share a name across two packages -- `pine.ui.build:ring` makes a
`pine.ui.node:ring` -- and the constructor package shadows it explicitly.

## The surface language

`/a/b`, `{...}`, `[...]` are sugar for a config and for a person at the REPL,
not for pine's own source. Only `src/path/`, the language declarations under
`src/ts/lang/` and a config may declare the readtable; a test enforces it.

## Size

Nothing over 400 lines. A file is one idea, and a file that has outgrown that
is two files.

## Text

ASCII only. No em-dashes; use `--`, a colon, or a semicolon.
