.PHONY: repl dev daemon editor desktop check cairo-shot wl-desktop wl-editor bin

GUIX := guix shell -m manifest.scm --
ENV  := LD_LIBRARY_PATH="$$GUIX_ENVIRONMENT/lib" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/pine/"
PRELOAD := LD_PRELOAD="$$GUIX_ENVIRONMENT/lib/libgtk4-layer-shell.so"

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
daemon:
	$(GUIX) sh -c '$(ENV) sbcl --eval "(asdf:load-system :pine)" --eval "(pine:run-daemon)"'

# the editor app: a separate process, attaches to the daemon.
editor:
	$(GUIX) sh -c '$(ENV) $(PRELOAD) sbcl --eval "(asdf:load-system :pine/gtk)" --eval "(pine.gtk:run-editor)"'

# the desktop app: layer-shell bar/panels, a separate process, attaches to the daemon.
desktop:
	$(GUIX) sh -c '$(ENV) $(PRELOAD) sbcl --eval "(asdf:load-system :pine/gtk)" --eval "(pine.desktop-app:run-desktop-app)"'

# one command: start the daemon in the background, wait until it is actually up,
# then the desktop + editor apps attached to it. Closing the editor tears the
# daemon and desktop down.
dev:
	$(GUIX) sh -c '$(ENV) \
	  rm -f /tmp/pine-daemon.log; \
	  sbcl --non-interactive --eval "(asdf:load-system :pine)" --eval "(pine:run-daemon)" >/tmp/pine-daemon.log 2>&1 & \
	  DPID=$$!; \
	  for i in $$(seq 1 90); do grep -q "daemon up" /tmp/pine-daemon.log && break; sleep 1; done; \
	  $(PRELOAD) sbcl --non-interactive --eval "(asdf:load-system :pine/gtk)" --eval "(pine.desktop-app:run-desktop-app)" >/tmp/pine-desktop.log 2>&1 & \
	  WPID=$$!; \
	  $(PRELOAD) sbcl --eval "(asdf:load-system :pine/gtk)" --eval "(pine.gtk:run-editor)"; \
	  kill $$DPID $$WPID 2>/dev/null'

check:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine/gtk)" --eval "(princ :loaded)" --eval "(terpri)"'

# the GTK-free desktop: attach to the running daemon (make daemon) and run the
# bar, echo, and panels as wayland layer surfaces (opens surfaces): make wl-desktop
wl-desktop:
	$(GUIX) sh -c '$(ENV) sbcl --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wayland:run-desktop)"'

# the GTK-free editor: an xdg-shell window painting the editor surface, with
# xkb keyboard. Needs make daemon up. Opens a window: make wl-editor
wl-editor:
	$(GUIX) sh -c '$(ENV) sbcl --eval "(asdf:load-system :pine/wayland)" --eval "(pine.wl-editor:run-editor)"'

# print each highlighted token of a source file and its face: make hl FILE=x.lisp
hl:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine)" --eval "(pine.ts:hl-dump-file \"$(FILE)\")"'

# render the editor frame to a PNG, headless (no window): make shot
shot:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine/gtk)" --eval "(pine.gtk::shot)" --eval "(sb-ext:exit)"'

# render every desktop panel through the GTK-free cairo backend to PNGs in /tmp,
# headless (no window, no daemon): make cairo-shot
cairo-shot:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine/cairo)" --eval "(princ (pine.cairo:shot))" --eval "(terpri)" --eval "(sb-ext:exit)"'
