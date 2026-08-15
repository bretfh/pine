(defpackage #:pine/wayland/app/chrome
  (:use #:cl #:wayflan-client #:pine/wayland/protocol)
  (:export #:own-chrome-p #:take! #:border-rgba #:borders!))
(in-package #:pine/wayland/app/chrome)

(defun own-chrome-p (hint)
  "True when a client cannot be talked out of decorating itself.

The compositor only carries a decoration mode to a client that bound
xdg-decoration; one that never did is reported as `only_supports_csd', and
asking it to use server side decorations does nothing at all."
  (eq hint :only-supports-csd))

(defun take! (proxy hint)
  "Ask a window's client to leave its decorations to us. Window management
state, so this belongs to a manage sequence."
  (if (own-chrome-p hint)
      (river-window-v1.use-csd proxy)
      (river-window-v1.use-ssd proxy))
  (river-window-v1.set-tiled proxy '(:top :bottom :left :right))
  proxy)

(defun border-rgba (colour)
  "(values R G B A) for an (R G B) theme COLOUR, in the channels set_borders
takes.

The protocol's channels are 32 bit and premultiplied. A theme colour is eight
bit and the border is opaque, so premultiplying is the identity here."
  (destructuring-bind (r g b) colour
    (flet ((scale (c) (floor (* c #xFFFFFFFF) 255)))
      (values (scale r) (scale g) (scale b) #xFFFFFFFF))))

(defun borders! (proxy width colour)
  "Border a window in COLOUR, which is what says whether it holds focus.
Rendering state."
  (multiple-value-bind (r g b a) (border-rgba colour)
    (river-window-v1.set-borders proxy '(:top :bottom :left :right)
                                 width r g b a))
  proxy)
