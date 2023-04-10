(defsystem "leibniz-internals"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "trivia")

  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "operator")
   (:file "expression")
   (:file "operators")))
