.PHONY: repl dev daemon editor desktop check docs shot cairo-shot split-shot wm-shot wm-nested bin bench test stress probe ts-probe hl vm threads

# --rebuild-cache: the manifest's local-file packages (tree-sitter grammar,
# pine-pty) change on disk without manifest.scm's mtime moving; the cached
# profile would silently serve stale builds.
GUIX := guix shell --rebuild-cache -m manifest.scm --
ENV  := LD_LIBRARY_PATH="$$GUIX_ENVIRONMENT/lib" CL_SOURCE_REGISTRY="$$PWD//:$$GUIX_ENVIRONMENT/share/common-lisp//" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/pine/"
# --no-userinit: deps come from guix and this tree only; the user's sbclrc
# (ocicl runtime, its own source registry) must not leak into pine builds.
SBCL := sbcl --no-userinit --eval "(require :asdf)"

repl:
	$(GUIX) sh -c '$(ENV) $(SBCL) --eval "(asdf:load-system :pine)"'

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
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(sb-ext:save-lisp-and-die \".pine.bin\" :toplevel (function pine.cli:main) :executable t)" && printf '\''#!/bin/sh\nexport GUIX_ENVIRONMENT="%s"\nexport LD_LIBRARY_PATH="%s/lib:$$LD_LIBRARY_PATH"\nexec "$$(dirname "$$0")/.pine.bin" "$$@"\n'\'' "$$GUIX_ENVIRONMENT" "$$GUIX_ENVIRONMENT" > pine && chmod +x pine'
	@echo "built ./pine"

# the headless substrate: editor and desktop apps attach to it over remoting.
# Explicit heap: the daemon is long-lived; give the GC room instead of growing
# into fragmentation.
daemon:
	$(GUIX) sh -c '$(ENV) sbcl --dynamic-space-size 4096 --no-userinit --eval "(require :asdf)" --eval "(asdf:load-system :pine)" --eval "(pine:run-daemon)"'

# the editor: an xdg-shell window painting the editor surface, xkb keyboard,
# its own process attached to the daemon (make daemon must be up).
editor:
	$(GUIX) sh -c '$(ENV) $(SBCL) --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland.app.editor:run-editor)"'

# the desktop: bar, echo, and panels as wayland layer surfaces, its own process
# attached to the daemon.
desktop:
	$(GUIX) sh -c '$(ENV) $(SBCL) --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland.app.desktop:run-desktop)"'

# one command: daemon in the background, then the desktop and the editor
# attached to it. Closing the editor tears the daemon and desktop down.
dev:
	$(GUIX) sh -c '$(ENV) \
	  rm -f /tmp/pine-daemon.log; \
	  sbcl --dynamic-space-size 4096 --no-userinit --non-interactive --eval "(require :asdf)" --eval "(asdf:load-system :pine)" --eval "(pine:run-daemon)" >/tmp/pine-daemon.log 2>&1 & \
	  DPID=$$!; \
	  for i in $$(seq 1 90); do grep -q "daemon up" /tmp/pine-daemon.log && break; sleep 1; done; \
	  $(SBCL) --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland.app.desktop:run-desktop)" >/tmp/pine-desktop.log 2>&1 & \
	  WPID=$$!; \
	  $(SBCL) --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland.app.editor:run-editor)"; \
	  kill $$DPID $$WPID 2>/dev/null'

check:
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(princ :loaded)" --eval "(terpri)"'

# hot-path microbenchmarks of the substrate (ns/op, bytes/op, gc%): make bench
bench:
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --load bench/run.lisp'

# the fiveam suite: model correctness, live editor integration, cross-image
# agents. Exits nonzero on failure.
test:
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --eval "(asdf:test-system :pine)"'

# ask the tree-sitter binding what it answers, many times over and from several
# threads at once: make ts-probe
ts-probe:
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --load bench/ts-probe.lisp'

# run one suite over and over in a single image, to read an intermittent
# failure: make probe SUITE=pine.async TIMES=10
probe:
	$(GUIX) sh -c '$(ENV) SUITE="$(SUITE)" TIMES="$(TIMES)" $(SBCL) --non-interactive --load bench/probe.lisp'

# load and faults under volume: hundreds of buffers, thousands of messages,
# concurrent writers, fault storms, edits off the end of a buffer: make stress
# :abort t, because the suite raises actor systems and pty readers: an orderly
# exit waits for those threads and the image hangs holding its remoting port.
stress:
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/test)" --eval "(sb-ext:exit :code (if (fiveam:run! :pine.stress) 0 1) :abort t)"'

# the diagrams under doc/ are generated from the .dot beside them: make docs
docs:
	$(GUIX) sh -c 'for d in doc/*.dot; do \
	  dot -Tpng -o "$${d%.dot}.png" "$$d"; \
	  dot -Tsvg -o "$${d%.dot}.svg" "$$d"; \
	  echo "wrote $${d%.dot}.png $${d%.dot}.svg"; done'

# print each highlighted token of a source file and its face: make hl FILE=x.lisp
hl:
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine)" --eval "(pine.ts.syntax:hl-dump-file \"$(FILE)\")"'

# render the editor frame for a sample buffer to a PNG, headless: make shot
shot:
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/cairo)" --eval "(pine.cairo.shot:frame-shot)" --eval "(sb-ext:exit)"'

# drive a real session through C-x 2/3/o/1 and PNG every step via the wire +
# paint-arranged (the frontend's exact path), with tree dumps: make split-shot
split-shot:
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/cairo)" --load bench/split-shot.lisp'

# the same eyes for the window manager: headless river, pine wm driving it,
# test clients spawned in sequence, a PNG of the compositor's own output per
# step. No window on any display: make wm-shot
wm-shot:
	$(GUIX) sh -c '$(ENV) bench/wm-shot.sh'

# river nested as a window in this session, pine managing it, driven by hand:
# make wm-nested
wm-nested:
	$(GUIX) sh -c '$(ENV) bench/wm-nested.sh'

# render every desktop panel through the cairo backend to PNGs in /tmp,
# headless (no window, no daemon): make cairo-shot
cairo-shot:
	$(GUIX) sh -c '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :pine/cairo)" --eval "(princ (pine.cairo.shot:shot))" --eval "(terpri)" --eval "(sb-ext:exit)"'

threads:
	@./pine eval '(with-output-to-string (s) (dolist (th (sb-thread:list-all-threads)) (unless (eq th sb-thread:*current-thread*) (format s "~&=== ~a~%" (sb-thread:thread-name th)) (let ((done (sb-thread:make-semaphore))) (ignore-errors (sb-thread:interrupt-thread th (lambda () (ignore-errors (sb-debug:print-backtrace :stream s :count 20)) (sb-thread:signal-semaphore done)))) (sb-thread:wait-on-semaphore done :timeout 2)))))'

# evaluate one form in an image with the test system loaded, in PINE.TEST, with
# the debugger left on so a fault prints its backtrace: make eval FORM='(...)'
eval:
	FORM='$(FORM)' $(GUIX) sh -c '$(ENV) $(SBCL) --disable-debugger --eval "(asdf:load-system :pine/test)" --eval "(in-package :pine.test)" --eval "(eval (read-from-string (uiop:getenv \"FORM\")))" --quit'
