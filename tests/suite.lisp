(defpackage :pine.test
  (:use :cl :fiveam))

(in-package :pine.test)

(def-suite :pine
  :description "The pine test suite. Every subsystem registers a child suite,
so (fiveam:run! :pine.vt) runs one on its own.")
