.PHONY: foreign foreign-deps foreign-libs foreign-wayflan
.PHONY: repl check test probe eval docs daemon editor shot bin

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

# load everything and say so, without running anything
check:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(princ :wayland-loaded)" --eval "(terpri)"'
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine)" --eval "(princ :loaded)" --eval "(terpri)"'

# the daemon: a pine listening on a remoting port, for a frontend to attach to
daemon:
	$(IN) '$(ENV) sbcl --noinform --dynamic-space-size 4096 --no-userinit --eval "(require :asdf)" --eval "(handler-bind ((warning (function muffle-warning))) (asdf:load-system :pine))" --eval "(setf pine.run.log:*to* *standard-output*)" --eval "(pine:daemon)" --eval "(loop (sleep 60))"'

# the editor: an xdg-shell window attached to the daemon. make daemon first.
editor:
	$(IN) '$(ENV) $(SBCL) --eval "(asdf:load-system :pine/wayland)" --eval "(pine:run-app \"editor\")"'

# the frame as PNGs, with no display and no daemon: /tmp/pine-rows.png is the
# cell grid, /tmp/pine-window.png the pixel pass a wayland surface gets
shot:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/cairo)" --eval "(princ (pine.cairo.shot:shot))" --eval "(terpri)"'

# one executable: the daemon, the frontends and the CLI, which is how pine is
# meant to be run. ./pine with no verb says what it takes.
bin:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(sb-ext:save-lisp-and-die \"pine\" :executable t :save-runtime-options t :toplevel (function pine.cli:main))"'
	@echo "wrote ./pine"

# the fiveam suite. Exits nonzero on failure.
test:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:test-system :pine)"'

# run one suite over and over in a single image, to read an intermittent
# failure: make probe SUITE=pine.repl TIMES=10
probe:
	$(IN) '$(ENV) SUITE="$(SUITE)" TIMES="$(TIMES)" $(SBCL) --non-interactive --load bench/probe.lisp'

# evaluate one form in an image with the test system loaded, in PINE.TEST, with
# the debugger left on so a fault prints its backtrace: make eval FORM='(...)'
eval:
	FORM='$(FORM)' $(IN) '$(ENV) $(SBCL) --disable-debugger --eval "(asdf:load-system :pine/test)" --eval "(in-package :pine.test)" --eval "(eval (read-from-string (uiop:getenv \"FORM\")))" --quit'

# the diagrams under doc/ are generated from the .dot beside them: make docs
docs:
	$(IN) 'for d in doc/*.dot; do \
	  dot -Tpng -o "$${d%.dot}.png" "$$d"; \
	  dot -Tsvg -o "$${d%.dot}.svg" "$$d"; \
	  echo "wrote $${d%.dot}.png $${d%.dot}.svg"; done'

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
	@test -d systems/wayflan \
	  && { grep -q "cons value value" systems/wayflan/src/client/client.lisp 2>/dev/null \
	       && echo "  wayflan   patched" \
	       || echo "  wayflan   UNPATCHED   make FOREIGN=1 foreign-wayflan"; } \
	  || echo "  wayflan   not fetched yet"
	@echo
	@echo "then: make FOREIGN=1 foreign-deps && make FOREIGN=1 foreign-libs && make FOREIGN=1 test"
	@echo "nix:  nix develop, then the same three"

# pine runs against river, and upstream wayflan cannot. Two wire-level fixes,
# the same two the guix package carries: the enum decoder errors on values
# outside its protocol tables (river advertises wl_shm formats -- drm fourcc
# codes -- that the bundled tables predate), and the string reader dies on a
# null string, which is what an unset app_id is on the wire. Unknown enums
# decode to their raw integer and null strings to ""; encoding stays strict.
#
# Loading wayflan is enough for the test suite, so this is only needed to run
# the editor, the desktop or the wm.
foreign-wayflan:
	@test -d systems/wayflan || { echo "no systems/wayflan: make FOREIGN=1 foreign-deps first"; exit 1; }
	@grep -q "cons value value" systems/wayflan/src/client/client.lisp \
	  && { echo "already patched"; exit 0; } || true
	sed -i.bak "s|(car (or (find value table :key #'cdr :test #'=)|(car (or (find value table :key #'cdr :test #'=) (cons value value)|" \
	  systems/wayflan/src/client/client.lisp
	sed -i.bak "s|(cffi:foreign-string-to-lisp|(if (< nul-length 2) (and (plusp nul-length) \"\") (cffi:foreign-string-to-lisp|" \
	  systems/wayflan/src/wire.lisp
	sed -i.bak "s|^        :encoding :utf-8)$$|        :encoding :utf-8))|" \
	  systems/wayflan/src/wire.lisp
	@echo "patched systems/wayflan"

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
