;;;; The desktop is two layers. pine.de is the framework: layer-shell surfaces,
;;;; the cairo painter, the panel registry, and the widget vocabulary a user
;;;; composes with. pine.desktop is one such user's config -- the specific bar,
;;;; panels, and data wiring -- built entirely on pine.de's exports. A user
;;;; replaces pine.desktop without touching the framework; the editor can reload
;;;; it live, since it is just a module of defwidgets and registrations.

(defpackage #:pine.de
  (:use #:cl #:gtk4)
  (:shadowing-import-from #:pine.layout
                #:defwidget #:column #:row #:label #:icon #:button #:boxed
                #:centered #:viewport #:gap #:rule #:meter #:rows #:choice)
  (:export
   ;; lifecycle + registry
   #:*bar-enabled* #:start-desktop #:set-bar! #:defpanel #:toggle-panel
   ;; reactive + shell helpers a widget uses
   #:cell #:sh #:toggle #:launch
   ;; reusable widget vocabulary
   #:card #:header #:pill
   ;; the layout DSL, re-exported so a config can (:use :pine.de) alone
   #:defwidget #:column #:row #:label #:icon #:button #:boxed
   #:centered #:viewport #:gap #:rule #:meter #:rows #:choice))

(defpackage #:pine.desktop
  (:use #:cl #:pine.de))
