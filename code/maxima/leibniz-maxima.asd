(defsystem "leibniz-maxima"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "closer-mop"
   "maxima"
   "trivia")

  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "methods")
   (:file "operators")))
