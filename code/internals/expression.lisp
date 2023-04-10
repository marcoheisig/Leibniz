(in-package #:leibniz-internals)

;;; Common Lisp symbols and numbers are always a valid external representation
;;; of an expression, so that the results of many verbs can be passed directly
;;; to Common Lisp functions.

(defmethod expressionp
    ((number number))
  t)

(defmethod atomic-expression-p
    ((number number))
  t)
(defmethod compound-expression-p
    ((number number))
  nil)

(defmethod expressionp
    ((symbol symbol))
  t)

(defmethod atomic-expression-p
    ((symbol symbol))
  t)

(defmethod compound-expression-p
    ((symbol symbol))
  nil)

;;; The list of arguments of a compound expression can be derived from other,
;;; more specific methods.

(defmethod compound-expression-arguments
    ((object t))
  (loop for position below (compound-expression-number-of-arguments object)
        collect (compound-expression-argument object position)))

;;; Argument checking.

(defmethod client-apply-noun :before
    ((client t)
     (operator operator)
     (arguments list))
  (argcheck operator (length arguments)))

(defmethod client-apply-verb :before
    ((client t)
     (operator operator)
     (arguments list))
  (argcheck operator (length arguments)))

;;; Switch from external to internal representation.

(defmethod client-apply-noun
    ((client t)
     (operator operator)
     (arguments list))
  (client-external-representation
   client
   (client-apply-noun-internal
    client
    operator
    (mapcar
     (lambda (argument)
       (client-internal-representation client argument))
     arguments))))

(defmethod client-apply-verb
    ((client t)
     (operator operator)
     (arguments list))
  (client-external-representation
   client
   (client-apply-verb-internal
    client
    operator
    (mapcar
     (lambda (argument)
       (client-internal-representation client argument))
     arguments))))

;;; Verbs have the fallback solution of simply applying the noun.

(defmethod client-apply-verb-internal
    ((client t)
     (operator operator)
     (arguments list))
  (client-apply-noun-internal client operator arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions

(defvar *client*)

(defun internal-representation (object)
  (client-internal-representation *client* object))

(defun external-representation (object)
  (client-external-representation *client* object))

(defun apply-noun (operator arguments)
  (client-apply-noun *client* operator arguments))

(defun apply-verb (operator arguments)
  (client-apply-verb *client* operator arguments))
