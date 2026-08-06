.PHONY: foreign foreign-deps foreign-libs
.PHONY: repl dev daemon editor desktop check docs shot cairo-shot split-shot wm-shot wm-nested bin bench test stress probe ts-probe hl vm threads

# Two ways to get what pine needs, and every target below works under either.
# Guix is what pine develops against and what plain `make' uses. FOREIGN=1 is
# for a mac or a linux without guix: ocicl supplies the lisp systems, the
# system's own package manager the C libraries, and `make foreign' says what is
# missing. Nothing else in this file knows the difference:
#
#   make test              guix
#   make FOREIGN=1 test    ocicl and system libraries
FOREIGN ?=

# --rebuild-cache: the manifest's local-file packages (tree-sitter grammars,
# pine-pty) change on disk without manifest.scm's mtime moving; the cached
# profile would silently serve stale builds.
GUIX := guix shell --rebuild-cache -m manifest.scm --

ifeq ($(FOREIGN),)
  IN   := $(GUIX) sh -c
  ENV  := LD_LIBRARY_PATH="$$GUIX_ENVIRONMENT/lib" CL_SOURCE_REGISTRY="$$PWD//:$$GUIX_ENVIRONMENT/share/common-lisp//" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/pine/"
  # --no-userinit: deps come from guix and this tree only; the user's sbclrc
  # (ocicl runtime, its own source registry) must not leak into pine builds.
  SBCL := sbcl --no-userinit --eval "(require :asdf)"
else
  IN   := sh -c
  # ocicl put itself in the user's sbclrc, so this is the one path that wants
  # it. DYLD_ is what a mac's loader reads; setting both costs nothing.
  ENV  := LD_LIBRARY_PATH="$$PWD/lib:$$LD_LIBRARY_PATH" DYLD_LIBRARY_PATH="$$PWD/lib:$$DYLD_LIBRARY_PATH" CL_SOURCE_REGISTRY="$$PWD//" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/pine/"
  SBCL := sbcl --eval "(require :asdf)"
endif

repl:
	$(IN) '$(ENV) $(SBCL) --eval "(asdf:load-system :pine)"'

# build the pine OS (river session, pine as wm/daemon/frontends) and run it
# as a qemu vm: a window opens, auto-logs in, and the session starts.
vm:
	script=$$(guix system vm -L guix guix/os.scm) && \
	  echo "vm script: $$script" && \
	  $$script -m 4096 -smp 4 -vga virtio

# build the standalone `pine` CLI: save-lisp-and-die the cli entry into .pine.bin
# and a wrapper that carries the guix env the binary needs when run outside a
# `guix shell' -- LD_LIBRARY_PATH for the shared libs, and GUIX_ENVIRONMENT so
# pine.ts finds the tree-sitter grammars under lib/tree-sitter/: make bin
bin:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(sb-ext:save-lisp-and-die \".pine.bin\" :toplevel (function pine.cli:main) :executable t)" && printf '\''#!/bin/sh\nexport GUIX_ENVIRONMENT="%s"\nexport LD_LIBRARY_PATH="%s/lib:$$LD_LIBRARY_PATH"\nexec "$$(dirname "$$0")/.pine.bin" "$$@"\n'\'' "$$GUIX_ENVIRONMENT" "$$GUIX_ENVIRONMENT" > pine && chmod +x pine'
	@echo "built ./pine"

# the headless substrate: editor and desktop apps attach to it over remoting.
# Explicit heap: the daemon is long-lived; give the GC room instead of growing
# into fragmentation.
daemon:
	$(IN) '$(ENV) sbcl --dynamic-space-size 4096 --no-userinit --eval "(require :asdf)" --eval "(asdf:load-system :pine)" --eval "(pine:run-daemon)"'

# the editor: an xdg-shell window painting the editor surface, xkb keyboard,
# its own process attached to the daemon (make daemon must be up).
editor:
	$(IN) '$(ENV) $(SBCL) --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland.app.editor:run-editor)"'

# the desktop: bar, echo, and panels as wayland layer surfaces, its own process
# attached to the daemon.
desktop:
	$(IN) '$(ENV) $(SBCL) --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland.app.desktop:run-desktop)"'

# one command: daemon in the background, then the desktop and the editor
# attached to it. Closing the editor tears the daemon and desktop down.
dev:
	$(IN) '$(ENV) \
	  rm -f /tmp/pine-daemon.log; \
	  sbcl --dynamic-space-size 4096 --no-userinit --non-interactive --eval "(require :asdf)" --eval "(asdf:load-system :pine)" --eval "(pine:run-daemon)" >/tmp/pine-daemon.log 2>&1 & \
	  DPID=$$!; \
	  for i in $$(seq 1 90); do grep -q "daemon up" /tmp/pine-daemon.log && break; sleep 1; done; \
	  $(SBCL) --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland.app.desktop:run-desktop)" >/tmp/pine-desktop.log 2>&1 & \
	  WPID=$$!; \
	  $(SBCL) --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland.app.editor:run-editor)"; \
	  kill $$DPID $$WPID 2>/dev/null'

check:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(princ :loaded)" --eval "(terpri)"'

# hot-path microbenchmarks of the substrate (ns/op, bytes/op, gc%): make bench
bench:
	$(IN) '$(ENV) $(SBCL) --non-interactive --load bench/run.lisp'

# the fiveam suite: model correctness, live editor integration, cross-image
# agents. Exits nonzero on failure.
test:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:test-system :pine)"'

# ask the tree-sitter binding what it answers, many times over and from several
# threads at once: make ts-probe
ts-probe:
	$(IN) '$(ENV) $(SBCL) --non-interactive --load bench/ts-probe.lisp'

# run one suite over and over in a single image, to read an intermittent
# failure: make probe SUITE=pine.async TIMES=10
probe:
	$(IN) '$(ENV) SUITE="$(SUITE)" TIMES="$(TIMES)" $(SBCL) --non-interactive --load bench/probe.lisp'

# load and faults under volume: hundreds of buffers, thousands of messages,
# concurrent writers, fault storms, edits off the end of a buffer: make stress
# :abort t, because the suite raises actor systems and pty readers: an orderly
# exit waits for those threads and the image hangs holding its remoting port.
stress:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/test)" --eval "(sb-ext:exit :code (if (fiveam:run! :pine.stress) 0 1) :abort t)"'

# the diagrams under doc/ are generated from the .dot beside them: make docs
docs:
	$(IN) 'for d in doc/*.dot; do \
	  dot -Tpng -o "$${d%.dot}.png" "$$d"; \
	  dot -Tsvg -o "$${d%.dot}.svg" "$$d"; \
	  echo "wrote $${d%.dot}.png $${d%.dot}.svg"; done'

# print each highlighted token of a source file and its face: make hl FILE=x.lisp
hl:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine)" --eval "(pine.ts.syntax:hl-dump-file \"$(FILE)\")"'

# render the editor frame for a sample buffer to a PNG, headless: make shot
shot:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/cairo)" --eval "(pine.cairo.shot:frame-shot)" --eval "(sb-ext:exit)"'

# drive a real session through C-x 2/3/o/1 and PNG every step via the wire +
# paint-arranged (the frontend's exact path), with tree dumps: make split-shot
split-shot:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/cairo)" --load bench/split-shot.lisp'

# the same eyes for the window manager: headless river, pine wm driving it,
# test clients spawned in sequence, a PNG of the compositor's own output per
# step. No window on any display: make wm-shot
wm-shot:
	$(IN) '$(ENV) bench/wm-shot.sh'

# river nested as a window in this session, pine managing it, driven by hand:
# make wm-nested
wm-nested:
	$(IN) '$(ENV) bench/wm-nested.sh'

# render every desktop panel through the cairo backend to PNGs in /tmp,
# headless (no window, no daemon): make cairo-shot
cairo-shot:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/cairo)" --eval "(princ (pine.cairo.shot:shot))" --eval "(terpri)" --eval "(sb-ext:exit)"'

threads:
	@./pine eval '(with-output-to-string (s) (dolist (th (sb-thread:list-all-threads)) (unless (eq th sb-thread:*current-thread*) (format s "~&=== ~a~%" (sb-thread:thread-name th)) (let ((done (sb-thread:make-semaphore))) (ignore-errors (sb-thread:interrupt-thread th (lambda () (ignore-errors (sb-debug:print-backtrace :stream s :count 20)) (sb-thread:signal-semaphore done)))) (sb-thread:wait-on-semaphore done :timeout 2)))))'

# evaluate one form in an image with the test system loaded, in PINE.TEST, with
# the debugger left on so a fault prints its backtrace: make eval FORM='(...)'
eval:
	FORM='$(FORM)' $(IN) '$(ENV) $(SBCL) --disable-debugger --eval "(asdf:load-system :pine/test)" --eval "(in-package :pine.test)" --eval "(eval (read-from-string (uiop:getenv \"FORM\")))" --quit'

# ---------------------------------------------------------------- foreign ---
#
# What pine needs without guix. `make foreign' checks and reports; it installs
# nothing behind your back, because what installs software on your machine is
# your business. Everything it names is a one-line command it prints.

OCICL_SYSTEMS := alexandria bordeaux-threads cffi cffi-libffi cl-cairo2 cl-xkb \
                 closer-mop com.inuoe.jzon fiveam fset named-readtables \
                 posix-shm sento sento-remoting sqlite usocket wayflan-client

# where a build outside guix puts the shared libraries pine loads. lib/tree-sitter
# is one of the places pine.ts.runtime already looks; the rest go beside them and
# the FOREIGN env puts lib/ on the loader's path.
LIBDIR := lib
TSDIR  := lib/tree-sitter
GRAMMARS := ../tree-sitter-commonlisp

# cc on a mac and most linuxes, gcc where only that is installed
CC := $(shell command -v cc 2>/dev/null || command -v gcc 2>/dev/null)

UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
  # forkpty is in libc on a mac; on linux it is in libutil
  PTY_LINK :=
  SOEXT := dylib
  PKGS  := "brew install sbcl tree-sitter cairo sqlite libffi pkg-config graphviz"
  WAYLAND := "Wawona (github.com/Wawona/Wawona), a wayland compositor for macOS"
else
  PTY_LINK := -lutil
  SOEXT := so
  PKGS  := "your package manager: sbcl libtree-sitter-dev libcairo2-dev libsqlite3-dev libffi-dev libxkbcommon-dev pkg-config graphviz"
  WAYLAND := "any wayland compositor"
endif

foreign:
	@echo "pine without guix, on $(UNAME):"
	@echo
	@command -v sbcl >/dev/null 2>&1 \
	  && echo "  sbcl      $$(sbcl --version)" \
	  || echo "  sbcl      MISSING     install with $(PKGS)"
	@command -v ocicl >/dev/null 2>&1 \
	  && echo "  ocicl     $$(ocicl version 2>&1 | head -1)" \
	  || echo "  ocicl     MISSING     git clone https://github.com/ocicl/ocicl && cd ocicl && make && make install && ocicl setup"
	@test -n "$(CC)" \
	  && echo "  cc        $$($(CC) --version 2>&1 | head -1)" \
	  || echo "  cc        MISSING     install with $(PKGS)"
	@pkg-config --exists cairo 2>/dev/null \
	  && echo "  cairo     $$(pkg-config --modversion cairo)" \
	  || echo "  cairo     MISSING     install with $(PKGS)"
	@test -f "$(TSDIR)/libtree-sitter-commonlisp.$(SOEXT)" \
	  && echo "  grammars  built in $(TSDIR)" \
	  || echo "  grammars  MISSING     make FOREIGN=1 foreign-libs"
	@test -f "$(LIBDIR)/libpine-pty.$(SOEXT)" \
	  && echo "  pty       built in $(LIBDIR)" \
	  || echo "  pty       MISSING     make FOREIGN=1 foreign-libs"
	@test -n "$$WAYLAND_DISPLAY" \
	  && echo "  wayland   WAYLAND_DISPLAY=$$WAYLAND_DISPLAY" \
	  || echo "  wayland   not set     the editor, desktop and wm want $(WAYLAND)"
	@echo
	@echo "then: make FOREIGN=1 foreign-deps && make FOREIGN=1 foreign-libs && make FOREIGN=1 test"

# the lisp systems, from ocicl. It resolves what these depend on in turn.
foreign-deps:
	@command -v ocicl >/dev/null 2>&1 || { echo "no ocicl: see make foreign"; exit 1; }
	ocicl install $(OCICL_SYSTEMS)

# the C pine owns: its pty helper, and the two tree-sitter grammars generated
# from the sibling checkout. Guix builds these as packages; without guix they
# are three compiler invocations and they land where pine already looks.
foreign-libs:
	@test -n "$(CC)" || { echo "no C compiler: see make foreign"; exit 1; }
	mkdir -p $(LIBDIR) $(TSDIR)
	$(CC) -O2 lib/pty-helper.c -o $(LIBDIR)/pine-pty-helper
	$(CC) -shared -fPIC -DPINE_PTY_HELPER='"$(CURDIR)/$(LIBDIR)/pine-pty-helper"' \
	   lib/pty.c -o $(LIBDIR)/libpine-pty.$(SOEXT) $(PTY_LINK)
	@test -f $(GRAMMARS)/src/parser.c \
	  || { echo "no grammar checkout at $(GRAMMARS)"; exit 1; }
	$(CC) -shared -fPIC -I $(GRAMMARS)/src $(GRAMMARS)/src/parser.c \
	   -o $(TSDIR)/libtree-sitter-commonlisp.$(SOEXT)
	$(CC) -shared -fPIC -I $(GRAMMARS)/pine/src $(GRAMMARS)/pine/src/parser.c \
	   -o $(TSDIR)/libtree-sitter-pine.$(SOEXT)
	@echo "built $(LIBDIR) and $(TSDIR)"
