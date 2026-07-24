.PHONY: repl dev daemon editor desktop check shot cairo-shot split-shot bin bench check-bench check-editor check-agent hl

# --rebuild-cache: the manifest's local-file packages (tree-sitter grammar,
# pine-pty) change on disk without manifest.scm's mtime moving; the cached
# profile would silently serve stale builds.
GUIX := guix shell --rebuild-cache -m manifest.scm --
ENV  := LD_LIBRARY_PATH="$$GUIX_ENVIRONMENT/lib" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/pine/"

repl:
	$(GUIX) sh -c '$(ENV) sbcl --eval "(asdf:load-system :pine)"'

# build the standalone `pine` CLI: save-lisp-and-die the cli entry into .pine.bin
# and a wrapper that carries the guix env the binary needs when run outside a
# `guix shell' -- LD_LIBRARY_PATH for the shared libs, and GUIX_ENVIRONMENT so
# pine.ts finds the tree-sitter grammars under lib/tree-sitter/: make bin
bin:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(sb-ext:save-lisp-and-die \".pine.bin\" :toplevel (function pine::cli) :executable t)" && printf '\''#!/bin/sh\nexport GUIX_ENVIRONMENT="%s"\nexport LD_LIBRARY_PATH="%s/lib:$$LD_LIBRARY_PATH"\nexec "$$(dirname "$$0")/.pine.bin" "$$@"\n'\'' "$$GUIX_ENVIRONMENT" "$$GUIX_ENVIRONMENT" > pine && chmod +x pine'
	@echo "built ./pine"

# the headless substrate: editor and desktop apps attach to it over remoting.
# Explicit heap: the daemon is long-lived; give the GC room instead of growing
# into fragmentation.
daemon:
	$(GUIX) sh -c '$(ENV) sbcl --dynamic-space-size 4096 --eval "(asdf:load-system :pine)" --eval "(pine:run-daemon)"'

# the editor: an xdg-shell window painting the editor surface, xkb keyboard,
# its own process attached to the daemon (make daemon must be up).
editor:
	$(GUIX) sh -c '$(ENV) sbcl --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wl-editor:run-editor)"'

# the desktop: bar, echo, and panels as wayland layer surfaces, its own process
# attached to the daemon.
desktop:
	$(GUIX) sh -c '$(ENV) sbcl --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland:run-desktop)"'

# one command: daemon in the background, then the desktop and the editor
# attached to it. Closing the editor tears the daemon and desktop down.
dev:
	$(GUIX) sh -c '$(ENV) \
	  rm -f /tmp/pine-daemon.log; \
	  sbcl --dynamic-space-size 4096 --non-interactive --eval "(asdf:load-system :pine)" --eval "(pine:run-daemon)" >/tmp/pine-daemon.log 2>&1 & \
	  DPID=$$!; \
	  for i in $$(seq 1 90); do grep -q "daemon up" /tmp/pine-daemon.log && break; sleep 1; done; \
	  sbcl --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland:run-desktop)" >/tmp/pine-desktop.log 2>&1 & \
	  WPID=$$!; \
	  sbcl --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wl-editor:run-editor)"; \
	  kill $$DPID $$WPID 2>/dev/null'

check:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine/wayland)" --eval "(princ :loaded)" --eval "(terpri)"'

# hot-path microbenchmarks of the substrate (ns/op, bytes/op, gc%): make bench
bench:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --load bench/run.lisp'

# correctness probes only (buffer model, vt goldens, highlight equivalence)
check-bench:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine)" --load bench/check.lisp --eval "(sb-ext:exit :code (if (pine.check:run-checks) 0 1))"'

# live integration probes over a real server/buffer-actor: minibuffer editing +
# error trapping + layouts + the user language. Self-exits nonzero on failure.
check-editor:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine)" --load bench/editor-check.lisp'

# proves cross-image live redefinition: spawn a real :process agent (its own
# SBCL), redefine a function in it, confirm the change took effect in that image.
check-agent:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine)" --load bench/agent-check.lisp'

# print each highlighted token of a source file and its face: make hl FILE=x.lisp
hl:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine)" --eval "(pine.ts:hl-dump-file \"$(FILE)\")"'

# render the editor frame for a sample buffer to a PNG, headless: make shot
shot:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine/cairo)" --eval "(pine.cairo:frame-shot)" --eval "(sb-ext:exit)"'

# drive a real session through C-x 2/3/o/1 and PNG every step via the wire +
# paint-arranged (the frontend's exact path), with tree dumps: make split-shot
split-shot:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine/cairo)" --load bench/split-shot.lisp'

# render every desktop panel through the cairo backend to PNGs in /tmp,
# headless (no window, no daemon): make cairo-shot
cairo-shot:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine/cairo)" --eval "(princ (pine.cairo:shot))" --eval "(terpri)" --eval "(sb-ext:exit)"'
