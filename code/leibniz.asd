(defsystem "leibniz"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("leibniz-internals"
   "leibniz-common-lisp"
   "leibniz-maxima")

  :components
  ((:file "leibniz")))
