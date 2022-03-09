(in-package :asdf)

(defsystem :fcg-interactive
  :author "Remi van Trijp"
  :maintainer "Sony CSL Paris"
  :license "Apache 2.0"
  :homepage "http://www.fcg-net.org/"
  :serial t
  :depends-on (:fcg-server)
  :components ((:file "grammars")
               (:file "fcg-interactive")
               (:file "examples"))
  :description "A server system for supporting FCG Interactive.")
