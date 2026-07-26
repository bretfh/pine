#!/bin/sh
# river nested as a window in the running session, with pine as its window
# manager, for driving by hand. Unlike wm-shot.sh this starts nothing and
# presses nothing: it opens a window and leaves it to you.
#
# The chords are the defaults, on Super. Note that the outer compositor claims
# its own Super chords first, so any it binds never reach the nested session --
# those are the ones to check with make wm-shot or in the VM, where nothing is
# above pine.
set -eu

out="${WM_NESTED_DIR:-/tmp/pine-wm-nested}"
port="${WM_NESTED_PORT:-7411}"
rm -rf "$out"
mkdir -p "$out/config/pine"

# An empty config: the defaults are what we want to drive. Its only job is to
# keep the developer's own init.lisp out of a test session.
: >"$out/config/pine/init.lisp"

cat >"$out/init" <<EOF
#!/bin/sh
export XDG_CONFIG_HOME="$out/config"

sbcl --no-userinit --non-interactive \
     --eval '(require :asdf)' \
     --eval '(asdf:load-system :pine)' \
     --eval "(pine:run-daemon :port $port)" >"$out/daemon.log" 2>&1 &

sleep 12

exec sbcl --no-userinit --non-interactive \
     --eval '(require :asdf)' \
     --eval '(asdf:load-system :pine/wayland)' \
     --eval "(pine.wayland.app.wm:run-wm :port $port)" >"$out/wm.log" 2>&1
EOF
chmod +x "$out/init"

cat <<EOF
nested river is opening as a window. keys inside it:

  Super-Return   terminal
  Super-j / -k   focus next / previous
  Super-2 / -3   next window opens below / beside the focused one
  Super-q        close the focused window
  Super-Shift-e  end the nested session

chords the outer compositor binds never reach this window; check those with
make wm-shot or in the vm. the daemon takes about ten seconds, so the first
window will not tile until then. logs: $out/daemon.log, $out/wm.log
EOF

WLR_BACKENDS=wayland \
WLR_WL_OUTPUTS=1 \
  river -log-level "${WM_NESTED_LOG:-error}" -c "$out/init" 2>&1 | tee "$out/river.log"
