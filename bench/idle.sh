#!/bin/sh
# What pine costs while nothing is happening. Samples every thread of the
# daemon and its frontends over a window and reports wakeups per second, so a
# loop that spins to ask "has anything happened?" shows up as a number rather
# than as a feeling. Run it against a live session: bench/idle.sh [seconds]
set -u

window="${1:-10}"
threshold="${IDLE_THRESHOLD:-20}"

# The lisp image only: a daemon started through `guix shell' has a wrapper
# carrying the same command line, and this pipeline's own grep shows up in ps.
pids_for() {
  ps -eo pid,args | grep -F "$1" | grep sbcl | grep -v "guix shell" |
    awk '{print $1}'
}

wakeups() {                     # total context switches across a pid's threads
  total=0
  for status in /proc/"$1"/task/*/status; do
    n=$(grep -E '^(voluntary|nonvoluntary)_ctxt_switches' "$status" 2>/dev/null |
        awk '{s+=$2} END{print s+0}')
    total=$((total + n))
  done
  echo "$total"
}

# name:pattern. The daemon and each frontend is its own image, so each is
# measured separately -- an idle frontend and a busy daemon are different
# faults.
targets="daemon:(pine:run-daemon)
desktop:run-app \"desktop\"
editor:run-app \"editor\"
wm:run-app \"wm\""

# One target per line: the patterns contain spaces, so the loop must not split
# on them.
old_ifs=$IFS
IFS='
'

found=""
for target in $targets; do
  name=${target%%:*}
  pattern=${target#*:}
  for pid in $(pids_for "$pattern"); do
    found="$found $name:$pid:$(wakeups "$pid")"
  done
done
IFS=$old_ifs

if [ -z "$found" ]; then
  echo "no pine processes found -- start a session first"
  exit 1
fi

sleep "$window"

printf '%-10s %8s %9s %s\n' process threads wakeups/s ''
status=0
for entry in $found; do
  name=${entry%%:*}
  rest=${entry#*:}
  pid=${rest%%:*}
  before=${rest#*:}
  [ -d "/proc/$pid" ] || { printf '%-10s %8s %9s (exited)\n' "$name" - -; continue; }
  after=$(wakeups "$pid")
  threads=$(ls "/proc/$pid/task" | wc -l)
  rate=$(awk -v a="$before" -v b="$after" -v w="$window" 'BEGIN{printf "%.1f", (b-a)/w}')
  over=$(awk -v r="$rate" -v t="$threshold" 'BEGIN{print (r > t) ? "over" : ""}')
  [ -n "$over" ] && status=1
  printf '%-10s %8s %9s %s\n' "$name" "$threads" "$rate" "$over"
done

[ "$status" -eq 0 ] || echo "(threshold ${threshold}/s -- IDLE_THRESHOLD overrides)"
exit "$status"
