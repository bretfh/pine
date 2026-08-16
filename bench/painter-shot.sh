#!/bin/sh
# The painter, end to end, with no display of your own: a headless river, a pine
# daemon beside it, the painter attached to both, and grim's PNG of what landed
# on the screen.
#
#   guix shell -m manifest.scm -- sh bench/painter-shot.sh /tmp/pine-screen.png
set -eu

out=${1:-/tmp/pine-screen.png}
port=${PINE_PORT:-17931}
# sway manages its own windows; river asks for a manager, and pine is one.
compositor=${COMPOSITOR:-sway}
run=$(mktemp -d)
export XDG_RUNTIME_DIR="$run"
export WLR_BACKENDS=headless
export WLR_LIBINPUT_NO_DEVICES=1
export WLR_RENDERER=pixman

env="LD_LIBRARY_PATH=$GUIX_ENVIRONMENT/lib \
CL_SOURCE_REGISTRY=$PWD//:$GUIX_ENVIRONMENT/share/common-lisp// \
ASDF_OUTPUT_TRANSLATIONS=/:$HOME/.cache/common-lisp/pine/"

cleanup() {
  [ -n "${painter:-}" ] && kill "$painter" 2>/dev/null || true
  [ -n "${daemon:-}" ] && kill "$daemon" 2>/dev/null || true
  [ -n "${wm:-}" ] && kill "$wm" 2>/dev/null || true
  rm -rf "$run"
}
trap cleanup EXIT

case "$compositor" in
  sway)
    printf 'output HEADLESS-1 resolution 1280x720\n' > "$run/config"
    sway -c "$run/config" >"$run/wm.log" 2>&1 & ;;
  river)
    printf '#!/bin/sh\nsleep 3600\n' > "$run/config"
    chmod +x "$run/config"
    river -c "$run/config" >"$run/wm.log" 2>&1 & ;;
  *) echo "no compositor called $compositor"; exit 1 ;;
esac
wm=$!

n=0
while :; do
  found=$(ls "$run" 2>/dev/null | grep '^wayland-[0-9]*$' | head -1 || true)
  [ -n "$found" ] && break
  n=$((n + 1))
  [ "$n" -gt 100 ] && { echo "the compositor did not come up:"; cat "$run/wm.log"; exit 1; }
  sleep 0.1
done
export WAYLAND_DISPLAY="$found"
echo "the compositor is up on $WAYLAND_DISPLAY"

env $env sbcl --dynamic-space-size 2048 --noinform --no-userinit \
  --eval '(require :asdf)' --eval '(require :sb-introspect)' \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :pine/all))' \
  --eval "(setf pine/run/actors:*port* $port)" \
  --eval '(setf pine/run/log:*to* *standard-output*)' \
  --eval '(pine:daemon :store nil :config nil)' \
  --eval '(pine:use :text)' --eval '(pine:use :edit)' --eval '(pine:use :desk)' \
  --eval '(pine/edit:type-text "(defun hello (who) who)")' \
  --eval '(loop (sleep 60))' >"$run/daemon.log" 2>&1 &
daemon=$!

n=0
while ! grep -q "answering peers" "$run/daemon.log" 2>/dev/null; do
  n=$((n + 1))
  [ "$n" -gt 300 ] && { echo "the daemon did not come up:"; tail -20 "$run/daemon.log"; exit 1; }
  sleep 0.2
done
echo "the daemon is up on $port"

env $env PINE_FRAME_DUMP=/tmp/pine-frame sbcl --dynamic-space-size 2048 --noinform --no-userinit \
  --eval '(require :asdf)' --eval '(require :sb-introspect)' \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :pine/wayland))' \
  --eval '(setf pine/run/log:*to* *standard-output*)' \
  --eval "(pine/wayland/painter:run :port $port)" >"$run/painter.log" 2>&1 &
painter=$!

n=0
while ! grep -q "surface" "$run/painter.log" 2>/dev/null; do
  n=$((n + 1))
  [ "$n" -gt 300 ] && { echo "the painter did not come up:"; tail -30 "$run/painter.log"; exit 1; }
  sleep 0.2
done
sleep 4

grim "$out" || { echo "grim said no"; tail -20 "$run/painter.log"; exit 1; }
echo "wrote $out"

# and again, with something typed at it. The keys go in at /key, which is where
# a painter puts what a keyboard gave it, so this is the same path a keystroke
# takes: the daemon works the frame out again, says so, and the painter paints.
pine() {
  env $env sbcl --noinform --no-userinit --non-interactive \
    --eval '(require :asdf)' --eval '(require :sb-introspect)' \
    --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :pine))' \
    --eval "(setf pine/run/actors:*port* $port)" \
    --eval "(pine/cli:main (list $*))" 2>/dev/null | tail -1
}

for k in '"C-a"' '";"' '";"' '"SPC"'; do pine '"write"' '"/key"' "$k" >/dev/null; done
sleep 2
grim "${out%.png}-typed.png" && echo "wrote ${out%.png}-typed.png"

echo "--- what the document holds"
pine '"read"' '"/text/scratch"'

# and what a keyboard on the compositor gives it. A headless seat has no keyboard
# until a virtual one appears and the compositor says so, which is why this is
# typed twice: the first is what makes the painter bind one.
if command -v wtype >/dev/null 2>&1; then
  wtype -s 200 "x" >/dev/null 2>&1 || true
  sleep 2
  wtype -s 200 "y" >/dev/null 2>&1 || true
  sleep 2
  echo "--- and after a keystroke from the compositor"
  pine '"read"' '"/text/scratch"'
fi

echo "--- what pine is showing"
pine '"read"' '"/wm/layout"'
pine '"run"' '"wm-windows"'

echo "--- painter"
grep -a -v '^;' "$run/painter.log" | grep -a -vE '^( |$)' | tail -16
echo "--- daemon"
grep -a -v '^;' "$run/daemon.log" | grep -a -vE '^( |$)' | tail -12
