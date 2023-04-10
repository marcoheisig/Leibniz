(in-package #:leibniz-maxima)

(defclass maxima ()
  ())

(defclass expression ()
  ((%form
    :initform (alexandria:required-argument :form)
    :initarg :form
    :reader expression-form)))

(defun make-expression (form)
  (make-instance 'expression
    :form form))
