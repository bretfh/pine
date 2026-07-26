#!/bin/sh
# Runs as river's init, inside the headless session: bring up the daemon and
# the window manager, then walk a scenario of clients and real key presses,
# capturing the compositor's own output at each step. Every step is logged, so
# a missing PNG points at the step that failed rather than at silence.
set -u

out="$WM_SHOT_DIR"
log() { printf '%s %s\n' "$(date +%H:%M:%S)" "$*" >>"$out/scenario.log"; }

shot() {
  if grim "$out/$1.png" 2>>"$out/scenario.log"; then
    log "shot $1"
  else
    log "shot $1 FAILED"
  fi
}

settle() { sleep "${1:-1.5}"; }

# Super plus a key, through the virtual keyboard protocol: a real press the
# compositor routes exactly like hardware, so registered bindings match.
press_super() {
  log "press super+$1"
  wtype -M logo -k "$1" -m logo 2>>"$out/scenario.log" || log "wtype super+$1 FAILED"
}

# A hermetic config directory: the harness must not load the developer's own
# init.lisp, and must not write to it.
export XDG_CONFIG_HOME="$out/config"
mkdir -p "$XDG_CONFIG_HOME/pine"

log "scenario start, WAYLAND_DISPLAY=${WAYLAND_DISPLAY:-unset}"

# The harness runs its own daemon on its own port: the developer's session
# daemon keeps its port and is never touched.
port="${WM_SHOT_PORT:-7411}"
log "harness daemon port $port"

# The configuration asks for the window manager. Nothing here starts one:
# the daemon spawns and supervises its frontends, which is the path a real
# login takes, so this exercises it.
cat >"$XDG_CONFIG_HOME/pine/init.lisp" <<'LISP'
(in-package :pine.user)
(setf *frontends* (list "wm" "desktop"))

;; wm-terminal spawns through a login shell, which resets PATH to the profile:
;; the manifest's foot is not on it, so name a terminal that is.
(setf (var :wm-terminal) "alacritty")

(defsurface bar (:as :bar)
  (column :class "bar" :align :center :spacing 8
    (label "P") (label "I") (label "N") (label "E")))

(defsurface echo (:as :echo)
  (row :class "echo" :align :center (label "echo strip")))
LISP

env -u WAYLAND_DISPLAY \
sbcl --no-userinit --non-interactive \
     --eval '(require :asdf)' \
     --eval '(asdf:load-system :pine)' \
     --eval "(pine:run-daemon :port $port)" >"$out/daemon.log" 2>&1 &
daemon_pid=$!
log "daemon pid $daemon_pid"

settle 15
# what river's init does: tell the daemon which display this session is on
log "announcing display $WAYLAND_DISPLAY"
sbcl --no-userinit --non-interactive \
     --eval '(require :asdf)' \
     --eval '(asdf:load-system :pine)' \
     --eval "(setf pine.server:*port* $port)" \
     --eval "(pine::cli (list \"session\"))" \
     >>"$out/session.log" 2>&1

settle 20
if pgrep -f 'run-app "wm"' >/dev/null; then
  log "daemon started the window manager"
else
  log "NO WINDOW MANAGER: see daemon.log and /tmp/pine-wm.log"
fi
# The bar is a layer surface. river closes those outright unless the window
# manager binds river_layer_shell_v1, so a bar on screen is the proof it did.
if pgrep -f 'run-app "desktop"' >/dev/null; then
  log "daemon started the desktop"
else
  log "NO DESKTOP: see daemon.log and /tmp/pine-desktop.log"
fi
shot 00-empty

foot >"$out/client-1.log" 2>&1 &
settle 3
shot 01-one-window

foot >"$out/client-2.log" 2>&1 &
settle 3
shot 02-two-windows

# The binding round trip: the compositor matches the chord, hands it to the
# window manager, the daemon looks it up and runs wm-terminal, and the
# frontend launches it -- a third window appears without anyone spawning it
# from this script.
press_super Return
settle 4
shot 03-binding-spawned

press_super j
settle 2
shot 04-focus-next

# the arrangement is a live layout tree: a split states where the next window
# joins the focused one, and the engine's weights do the arithmetic
press_super 2
press_super Return
settle 4
shot 05-split-below

press_super 3
press_super Return
settle 4
shot 06-split-beside

press_super q
settle 3
shot 07-after-close

# The frontend logs are superseded per spawn, so a frontend that died and was
# respawned leaves only its successor's log behind. Keep what is there now.
for f in wm desktop editor; do
  cp "/tmp/pine-$f.log" "$out/$f.log" 2>/dev/null || true
done

log "scenario done"
# The daemon only tears its frontends down on a clean stop, so a bare kill
# leaves them orphaned and talking to a daemon that is gone. Take the children
# first, by parent, so nothing outlives the harness.
kill $(ps -o pid= --ppid "$daemon_pid" 2>/dev/null) 2>/dev/null || true
kill "$daemon_pid" 2>/dev/null || true

# river outlives its init; end the session so the harness returns promptly.
kill "$PPID" 2>/dev/null || true
