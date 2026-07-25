#!/bin/sh
# Headless eyes for the window manager, the wm's answer to split-shot: a real
# river on the wlroots headless backend, pine wm driving it over
# river-window-management-v1, test clients spawned in sequence, one PNG per
# step. Nothing appears on any display, so this runs in the dev loop instead
# of a VM. Run it through the Makefile (make wm-shot), which supplies the
# guix environment the scenario inherits through river.
set -eu

out="${WM_SHOT_DIR:-/tmp/pine-wm-shot}"
here=$(cd "$(dirname "$0")" && pwd)

rm -rf "$out"
mkdir -p "$out"

# river runs its init as an executable, with no arguments and its own
# environment, so the scenario reaches its output directory through the
# environment river passes down.
init="$out/init"
cat >"$init" <<EOF
#!/bin/sh
export WM_SHOT_DIR="$out"
exec "$here/wm-scenario.sh"
EOF
chmod +x "$init"

WLR_BACKENDS=headless \
WLR_HEADLESS_OUTPUTS=1 \
XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}" \
  timeout --signal=TERM "${WM_SHOT_TIMEOUT:-120}" \
  river -log-level error -c "$init" >"$out/river.log" 2>&1 || true

echo "shots + logs in $out"
ls "$out"/*.png 2>/dev/null || echo "no shots captured -- read $out/wm.log and $out/river.log"
