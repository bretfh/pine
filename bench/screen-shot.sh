#!/bin/sh
# Pine end to end, with no display of your own: a headless compositor, one pine on
# it, and grim's PNG of what landed on the screen.
#
#   guix shell -m manifest.scm -- sh bench/screen-shot.sh /tmp/pine-screen.png
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
  [ -n "${pine:-}" ] && kill "$pine" 2>/dev/null || true
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

env $env PINE_FRAME_DUMP=/tmp/pine-frame sbcl --dynamic-space-size 2048 --noinform --no-userinit \
  --eval '(require :asdf)' --eval '(require :sb-introspect)' \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :pine/all))' \
  --eval "(setf pine/run/actors:*port* $port)" \
  --eval '(setf pine/run/log:*to* *standard-output*)' \
  --eval '(pine:daemon :store nil :config nil)' \
  --eval '(pine:write-at "/wm-places" "tiles")' \
  --eval '(pine/run/command:defcommand "chord-ran" () (:describes "a mark a chord leaves") (pine:write-at "/chord-ran" t))' \
  --eval '(pine/mode:bind (quote pine/wm/keys:wm) "s-c" "chord-ran")' \
  --eval '(pine:use :wm)' \
  --eval '(pine:use :text)' --eval '(pine:use :edit)' --eval '(pine:use :desk)' \
  --eval '(pine/edit:type-text "(defun hello (who) who)")' \
  --eval '(loop (sleep 60))' >"$run/pine.log" 2>&1 &
pine=$!

n=0
while ! grep -q "surface" "$run/pine.log" 2>/dev/null; do
  n=$((n + 1))
  [ "$n" -gt 300 ] && { echo "pine did not come up:"; tail -30 "$run/pine.log"; exit 1; }
  sleep 0.2
done
sleep 4

grim "$out" || { echo "grim said no"; tail -20 "$run/pine.log"; exit 1; }
echo "wrote $out"

# and again, with something typed at it. The keys go in at /key, which is where
# a keyboard's own says land, so this is the same path a keystroke takes: the
# surface works itself out again, says so, and the screen paints it.
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

echo "--- what the screen laid out for"
pine '"read"' '"/surface/bar/size"'
pine '"read"' '"/surface/editor/size"'

# and what a keyboard on the compositor gives it. A headless seat has no keyboard
# until a virtual one appears and the compositor says so, which is why this is
# typed twice: the first is what makes pine bind one.
if command -v wtype >/dev/null 2>&1; then
  wtype -s 200 "x" >/dev/null 2>&1 || true
  sleep 2
  wtype -s 200 "y" >/dev/null 2>&1 || true
  sleep 2
  echo "--- and after a keystroke from the compositor"
  pine '"read"' '"/text/scratch"'
fi

echo "--- a chord the compositor took, with a window focused"
foot -e sh -c 'sleep 20' >/dev/null 2>&1 &
sleep 4
pine '"run"' '"wm-windows"'
echo "before: $(pine '"read"' '"/chord-ran"')"
wtype -M logo -k c -m logo >/dev/null 2>&1 || echo "wtype failed"
sleep 3
echo "after:  $(pine '"read"' '"/chord-ran"')"

echo "--- what pine is showing"
pine '"read"' '"/wm/placement"'
pine '"run"' '"wm-windows"'

echo "--- pine"
[ -n "$PINE_KEEP_LOG" ] && cp "$run/pine.log" "$PINE_KEEP_LOG"
grep -a -v '^;' "$run/pine.log" | grep -a -vE '^( |$)' | tail -20
