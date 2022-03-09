(in-package :asdf)

(defsystem :fcg-server
  :author "Remi van Trijp"
  :maintainer "Sony Computer Science Laboratories Paris"
  :license "Apache 2.0"
  :homepage "http://www.fcg-net.org/"
  :serial t
  :depends-on (:snooze :utils :fcg :cl-json :trivial-timeout)
  :components ((:file "package")
               (:file "fcg-server")
               (:file "routes"))
  :description "A server system for FCG grammars.")
