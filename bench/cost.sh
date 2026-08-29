#!/bin/sh
# What a run cost the machine: wall, cpu, cores, memory, threads.
#
# Wraps any command and samples /proc for the whole process tree it makes.
# Touches nothing in pine. The number that answers "is any of this parallel"
# is CORES: cpu-seconds divided by wall-seconds. One means one thread did all
# of it, however many threads were standing about.
#
#   sh bench/cost.sh make bench WORK=typing SIZE=20000
#
# EVERY sets the sampling interval in seconds; the default is a tenth.

set -u

# clock ticks per second. getconf is not always about; linux has been 100
# for a very long time and /proc is in those units either way.
TICKS=$(getconf CLK_TCK 2>/dev/null || echo 100)
[ -n "$TICKS" ] || TICKS=100
EVERY=${EVERY:-0.1}
SEEN=$(mktemp -d)
trap 'rm -rf "$SEEN"' EXIT

"$@" &
TOP=$!

kids_of() { cat /proc/"$1"/task/*/children 2>/dev/null; }

tree() {
  pids="$TOP"; found="$TOP"
  while [ -n "$found" ]; do
    next=""
    for p in $found; do
      for k in $(kids_of "$p"); do
        case " $pids " in
          *" $k "*) ;;
          *) pids="$pids $k"; next="$next $k" ;;
        esac
      done
    done
    found="$next"
  done
  echo "$pids"
}

peak_rss=0; peak_threads=0; samples=0; busiest=0
start=$(date +%s.%N)

while kill -0 "$TOP" 2>/dev/null; do
  rss=0; threads=0; running=0
  for p in $(tree); do
    [ -r /proc/"$p"/stat ] || continue
    # utime + stime, in clock ticks. kept per pid so a process that exits
    # before the end still counts what it used.
    awk '{ n=split($0, f, " "); print f[14] + f[15] }' /proc/"$p"/stat \
      > "$SEEN/$p" 2>/dev/null || true
    r=$(awk '/^VmRSS:/{print $2}' /proc/"$p"/status 2>/dev/null)
    t=$(awk '/^Threads:/{print $2}' /proc/"$p"/status 2>/dev/null)
    rss=$((rss + ${r:-0})); threads=$((threads + ${t:-0}))
    # threads of this process that are on a cpu right now
    for s in /proc/"$p"/task/*/stat; do
      [ -r "$s" ] || continue
      case "$(awk '{print $3}' "$s" 2>/dev/null)" in R) running=$((running+1));; esac
    done
  done
  [ "$rss" -gt "$peak_rss" ] && peak_rss=$rss
  [ "$threads" -gt "$peak_threads" ] && peak_threads=$threads
  [ "$running" -gt "$busiest" ] && busiest=$running
  samples=$((samples + 1))
  sleep "$EVERY"
done

wait "$TOP" 2>/dev/null; code=$?
end=$(date +%s.%N)

ticks=$(cat "$SEEN"/* 2>/dev/null | awk '{n+=$1} END{print n+0}')
wall=$(awk -v a="$start" -v b="$end" 'BEGIN{printf "%.2f", b-a}')
cpu=$(awk -v t="$ticks" -v k="$TICKS" 'BEGIN{printf "%.2f", t/k}')
cores=$(awk -v c="$cpu" -v w="$wall" 'BEGIN{ if (w>0) printf "%.2f", c/w; else print 0 }')

printf '\n'
printf '%24s %s\n' "wall"            "${wall}s"
printf '%24s %s\n' "cpu (user+sys)"  "${cpu}s"
printf '%24s %s\n' "cores used"      "$cores"
printf '%24s %s\n' "peak rss"        "$((peak_rss / 1024)) MB"
printf '%24s %s\n' "threads alive"   "$peak_threads"
printf '%24s %s\n' "most on a cpu"   "$busiest"
exit $code
