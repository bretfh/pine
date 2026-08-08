(defpackage #:pine.repl
  (:use #:cl)
  (:shadow #:read #:print #:close #:describe)
  (:export #:command #:commandp #:defcommand #:command-named #:commands
           #:forget #:name #:action #:describes #:asks #:arguments #:run #:word
           #:mode #:minor #:minor-mode #:mode-named #:modes #:unmode #:mode-for
           #:parent #:indicator #:settings #:claims #:claimsp #:precedence
           #:chain #:setting #:handle #:handler #:claimants #:bind #:binding
           #:in-force #:globp
           #:session #:open-session #:sessions #:*session* #:*history-kept*
           #:owner #:package-of #:node-of #:mode-of #:minors
           #:history #:input #:output #:openp
           #:read #:evaluate #:print #:interact #:close
           #:evaluation #:form #:answered #:fault #:said #:at-time
           #:unknown-command #:name-of))
