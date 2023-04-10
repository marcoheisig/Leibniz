(in-package #:leibniz-maxima)

(defmethod print-object ((expression expression) stream)
  (print-unreadable-object (expression stream :type t)
    (maxima::mgrind (expression-form expression) stream)))

(defmethod expressionp
    ((expression expression))
  t)

(defmethod atomic-expression-p
    ((expression expression))
  nil)

(defmethod compound-expression-p
    ((expression expression))
  t)

(defmethod compound-expression-operator
    ((expression expression))
  (maxima::$part (expression-form expression) 0))

(defmethod compound-expression-argument
    ((expression expression)
     (position integer))
  (make-expression
   (maxima::$part (expression-form expression) (1+ position))))

(defmethod compound-expression-number-of-arguments
    ((expression expression))
  (maxima::$length (expression-form expression)))

;;; Internal Representation

(defmethod client-internal-representation
    ((maxima maxima)
     (expression expression))
  (expression-form expression))

(defmethod client-internal-representation
    ((maxima maxima)
     (integer integer))
  integer)

(defmethod client-internal-representation
    ((maxima maxima)
     (rational rational))
  `((maxima::mquotient)
    ,(client-internal-representation maxima (numerator rational))
    ,(client-internal-representation maxima (denominator rational))))

(defmethod client-internal-representation
    ((maxima maxima)
     (complex complex))
  `((maxima::mplus)
    ,(client-internal-representation maxima (realpart complex))
    ((maxima::mtimes)
     maxima::$%i
     ,(client-internal-representation maxima (imagpart complex)))))

(defmethod client-internal-representation
    ((maxima maxima)
     (symbol symbol))
  (let ((name (concatenate 'string "$" (symbol-name symbol)))
        (package (symbol-package symbol)))
    (assert package)
    (intern name package)))

;;; External Representation

(defmethod client-external-representation
    ((maxima maxima)
     (form t))
  (maxima-external-representation form))

(defun maxima-external-representation (form)
  (trivia:match form
    ((list* (list 'maxima::mlist) subforms)
     (mapcar #'maxima-external-representation subforms))
    ;; TODO complex numbers
    ((list (list 'maxima::mquotient)
           (and a (type integer))
           (and b (type integer)))
     (/ a b))
    ((type symbol)
     (intern (subseq (symbol-name form) 1)
             (symbol-package form)))
    ((type number)
     form)
    (_
     (make-expression form))))

;;; Verbs and Nouns.

;;; Maxima verbs are obtained by evaluating the corresponding nouns.
(defmethod client-apply-verb-internal
    ((maxima maxima)
     (operator operator)
     (arguments list))
  (maxima::meval
   (client-apply-noun-internal maxima operator arguments)))
