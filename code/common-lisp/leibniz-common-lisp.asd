(defsystem "leibniz-common-lisp"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "closer-mop")

  :serial t
  :components
  ((:file "packages")))
