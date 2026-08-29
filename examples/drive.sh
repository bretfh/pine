#!/bin/sh
# Drive a running pine from a shell.
#
# No lisp here, no fset and no actor system: a socket and a line of json each
# way. Everything below is one of the questions the tree already answers.
#
#   pine start            # or a daemon already running
#   sh examples/drive.sh | pine serve
#
# PINE_SOCKET names a pine when there is more than one.

say() { printf '%s\n' "$1"; }

say '{"id":1,"do":"ping"}'

# a map goes down and the same map comes back
say '{"id":2,"do":"write","path":"/probe/thing","value":{"map":[[":a",1],[":b",{"seq":[1,2]}]]}}'
say '{"id":3,"do":"read","path":"/probe/thing"}'

# an array is a list, and stays one even when it looks like a collection
say '{"id":4,"do":"write","path":"/probe/listy","value":[":seq",1,2]}'
say '{"id":5,"do":"read","path":"/probe/listy"}'

# what is under a place
say '{"id":6,"do":"ls","path":"/probe"}'

# start something that runs, give it a message, read what it said
say '{"id":7,"do":"write","path":"/proc","value":[":kind","program",":name","from-the-shell",":argv",["sh","-c","while read l; do echo \"got $l\"; done"]]}'
say '{"id":8,"do":"write","path":"/proc/from-the-shell/tell","value":"hello from sh"}'
sleep 6
say '{"id":9,"do":"read","path":"/proc/from-the-shell/said"}'

# stop it, by telling it rather than by writing a value
say '{"id":10,"do":"verb","path":"/proc/from-the-shell","verb":"stop"}'
say '{"id":11,"do":"read","path":"/proc/from-the-shell/state"}'

# watch a place: the event arrives unasked, on this same connection, and goes
# when the connection does
say '{"id":12,"do":"write","path":"/probe/watched","value":"before"}'
say '{"id":13,"do":"watch","path":"/probe/watched"}'
sleep 1
say '{"id":14,"do":"write","path":"/probe/watched","value":"after"}'
sleep 2

# what breaks is answered, never dropped
say '{"id":15,"do":"read","path":"/nowhere/at/all"}'
say '{"id":16,"do":"sing","path":"/probe"}'
say '{"id":17,"do":"read"}'
say 'not json at all'
