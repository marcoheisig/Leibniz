(in-package #:leibniz-internals)

;;; Operators

(defgeneric operatorp (object))

(defgeneric operator-min-arguments (object))

(defgeneric operator-max-arguments (object))

;;; Expressions

(defgeneric expressionp (object))

(defgeneric atomic-expression-p (object))

(defgeneric compound-expression-p (object))

(defgeneric compound-expression-operator (expression))

(defgeneric compound-expression-argument (expression position))

(defgeneric compound-expression-number-of-arguments (expression))

(defgeneric compound-expression-arguments (expression))

;;; Client Functions

(defgeneric client-internal-representation (client object)
  (:documentation
   "Returns an object that is the internal representation of CLIENT for the
supplied external representation, which can be one of the following:

- A Common Lisp number, which denotes itself.

- A Common Lisp symbol, which denotes a variable of the same name.

- The external representation of any possible client."))

(defgeneric client-external-representation (client object)
  (:documentation
   "Returns an object that is the external representation of CLIENT for the
supplied internal representation."))

(defgeneric client-apply-noun (client operator arguments)
  (:documentation
   "Returns the external representation of CLIENT for a noun clause with the
supplied OPERATOR and the supplied list of external representations of any
client in ARGUMENTS."))

(defgeneric client-apply-verb (client operator arguments)
  (:documentation
   "Returns the external representation of CLIENT for a verb clause with the
supplied OPERATOR and the supplied list of external representations of any
client in ARGUMENTS."))

(defgeneric client-apply-noun-internal (client operator arguments)
  (:documentation
   "Returns the internal representation of CLIENT for a noun clause with the
supplied OPERATOR and the supplied list of internal representations of that
client in ARGUMENTS."))

(defgeneric client-apply-verb-internal (client operator arguments)
  (:documentation
   "Returns the internal representation of CLIENT for a verb clause with the
supplied OPERATOR and the supplied list of internal representations of that
client in ARGUMENTS."))
