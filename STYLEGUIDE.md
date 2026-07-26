# pine style

## Packages

The path to a package is its name. `pine.editor.keymap` lives in
`src/editor/keymap.lisp` and declares itself there. There is no manifest.

A directory whose name is a package holds only that package's files, and none
of them repeats the directory name. A directory whose name is not a package is
a group, and every file directly in it is one package.

A file opens with its own `defpackage`, then `in-package`, then the code. Name
what it needs with `:local-nicknames` or `:use` when it builds on a vocabulary
(`pine.ui.node`, `pine.ts.runtime`); qualify otherwise. A file's dependencies
belong at its top, where you are already looking.

A file may only name packages that load before it. `tests/deps.lisp` reads the
source off disk and fails on a cycle. It sees `pkg:sym` references, so it
cannot see a call through `:use` -- it is a check on the part that is written
down, not a proof of layering.

## Defining is registering

Writing the form is the whole of it. `defmode` makes the class, the keymap and
the singleton. `define-command` registers. `defface` registers. Bindings are
top-level `define-key` / `define-keys` forms beside the commands they name.

Nothing walks the system afterwards installing anything, and no file lists
what to install in what order. There is no `install-*`.

## CLOS

A keyword in a `kind` slot switched on in two places is a class that has not
been written yet.

Modes are classes; their behaviour is `dispatch-message` and `execute`
methods, layered by method combination. A backend adds methods to another
package's generics; it does not reopen that package.

## Errors

Never swallow. No `ignore-errors` or bare `handler-case` that turns a fault
into `nil` -- that is how an undefined function reads as "no result" for
three tests. Either let it propagate, or route it through
`pine.core.eval:attempt`, which records the failure and surfaces it through
the debugger.

## Comments

Docstrings describe what the code does now. Block comments explain why the
shape is what it is, when that is not visible from the forms.

Do not narrate the change that produced the code. No "now", "previously",
"the bug this fixes". No restating what the form already says -- a `defclass`
does not need a comment saying it is a class. Reasoning about a change belongs
in the PR, not the source.

## Loop

Loop keywords take the colon: `(loop :for k :in list :collect ...)`.

## Naming

`c` is a client. `cli` is the command line interface. Check whether an
abbreviation already means something else in this tree before adopting it.

Lose redundant nouns: `place!` not `paint-tile!`. A constructor and the class
it makes may share a name across two packages -- `pine.ui.build:ring` makes a
`pine.ui.node:ring` -- and the constructor package shadows it explicitly.

## Text

ASCII only. No em-dashes; use `--`, a colon, or a semicolon.
