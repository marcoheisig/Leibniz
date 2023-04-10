(in-package #:common-lisp-user)

(progn
  (defpackage #:leibniz-internals
    (:use #:common-lisp)
    (:export
     #:operator
     #:operatorp
     #:operator-min-arguments
     #:operator-max-arguments
     #:expressionp
     #:atomic-expression-p
     #:compound-expression-p
     #:compound-expression-operator
     #:compound-expression-argument
     #:compound-expression-number-of-arguments
     #:compound-expression-arguments
     #:internal-representation
     #:external-representation
     #:apply-noun
     #:apply-verb
     #:client-internal-representation
     #:client-external-representation
     #:client-apply-noun
     #:client-apply-verb
     #:client-apply-noun-internal
     #:client-apply-verb-internal
     #:*client*
     #:*operators*
     #:ensure-operator
     #:find-operator
     #:define-operator
     #:define-noun
     #:define-verb))

  (defpackage #:leibniz
    ;; Operators that shadow symbols in the CL package.
    (:export
     . #1=
     (#:=
      #:/=
      #:<
      #:>
      #:<=
      #:>=
      #:max
      #:min
      #:floor
      #:ceiling
      #:truncate
      #:round
      #:sin
      #:cos
      #:tan
      #:asin
      #:acos
      #:atan
      #:sinh
      #:cosh
      #:tanh
      #:asinh
      #:acosh
      #:atanh
      #:*
      #:+
      #:-
      #:/
      #:1+
      #:1-
      #:abs
      #:evenp
      #:oddp
      #:exp
      #:expt
      #:gcd
      #:lcm
      #:log
      #:mod
      #:rem
      #:signum
      #:sqrt
      #:isqrt
      #:cis
      #:conjugate
      #:phase
      #:realpart
      #:imagpart
      #:ash
      #:integer-length))
    ;; Other Operators
    (:export
     #:+e+
     #:+i+
     #:+gamma+
     #:+phi+
     #:+pi+
     #:free-variables
     #:integrate
     #:diff
     #:solve
     #:factor
     #:expand)
    (:export
     ;; Metadata
     #:*cl-operators*)
    (:import-from
     #:common-lisp
     .
     #.(loop for sym being the external-symbols of "CL"
             unless (member sym '#1# :test #'string=)
               collect sym))
    (:import-from #:leibniz-internals #:define-operator))

  (defpackage #:leibniz-user
    (:use #:leibniz #:leibniz-internals)
    (:import-from
     #:common-lisp
     .
     #.(loop for sym being the external-symbols of "CL"
             unless (member sym '#1# :test #'string=)
               collect sym))))
