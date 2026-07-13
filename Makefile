.PHONY: repl dev daemon editor desktop check

GUIX := guix shell -m manifest.scm --
ENV  := LD_LIBRARY_PATH="$$GUIX_ENVIRONMENT/lib" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/pine/"
PRELOAD := LD_PRELOAD="$$GUIX_ENVIRONMENT/lib/libgtk4-layer-shell.so"

repl:
	$(GUIX) sh -c '$(ENV) sbcl --eval "(asdf:load-system :pine)"'

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

# print each highlighted token of a source file and its face: make hl FILE=x.lisp
hl:
	$(GUIX) sh -c '$(ENV) sbcl --non-interactive --eval "(asdf:load-system :pine)" --eval "(pine.ts:hl-dump-file \"$(FILE)\")"'
