(in-package #:leibniz-internals)

(defclass operator ()
  ((%name
    :initform (alexandria:required-argument :name)
    :initarg :name
    :reader operator-name
    :type (and symbol (not null)))
   (%min-arguments
    :initform (alexandria:required-argument :min-arguments)
    :initarg :min-arguments
    :reader operator-min-arguments
    :type (integer 0 (#.call-arguments-limit)))
   (%max-arguments
    :initform (alexandria:required-argument :max-arguments)
    :initarg :max-arguments
    :reader operator-max-arguments
    :type (integer 0 (#.call-arguments-limit)))))

(defmethod print-object ((operator operator) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of operator))
          :name (operator-name operator)
          :min-arguments (operator-min-arguments operator)
          :max-arguments (operator-max-arguments operator)))

(defmethod operatorp ((object t)) nil)

(defmethod operatorp ((operator operator)) t)

(defparameter *operators* (make-hash-table :test #'eq))

(defmethod reinitialize-instance :before
    ((operator operator) &key &allow-other-keys)
  (remhash (operator-name operator) *operators*))

(defmethod initialize-instance :after
    ((operator operator) &key &allow-other-keys)
  (setf (gethash (operator-name operator) *operators*)
        operator))

(defun ensure-operator (&rest arguments &key name &allow-other-keys)
  (let ((operator (gethash name *operators*)))
    (if (not operator)
        (apply #'make-instance 'operator arguments)
        (apply #'reinitialize-instance operator arguments))))

(defun find-operator (name &optional (errorp t))
  (or (gethash name *operators*)
      (and errorp
           (error "~@<There is no operator named ~S.~:@>"
                  name))))

(defun argcheck (operator n-arguments)
  "Check whether it is permissible to call the operator with the supplied
number of arguments."
  (with-accessors ((name operator-name)
                   (min-arguments operator-min-arguments)
                   (max-arguments operator-max-arguments))
      operator
    (unless (<= min-arguments n-arguments)
      (error "~@<Only ~R arguments supplied to the operator ~S~
                      that expects ~:[at least ~]~R arguments.~:@>"
             n-arguments name (= min-arguments max-arguments) min-arguments))
    (unless (<= n-arguments max-arguments)
      (error "~@<~:(~R~) arguments supplied to the operator ~S~
                      that expects ~:[at most ~]~R arguments.~:@>"
             n-arguments name (= min-arguments max-arguments) max-arguments))))

(defmacro define-operator (name &optional (lambda-list '() lambda-list-supplied-p))
  (multiple-value-bind (required optional rest keyword aak aux keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore keyword))
    (unless (typep name '(and symbol (not null)))
      (error "~@<Not a valid operator name: ~A.~:@>" name))
    (unless (not aux)
      (error "~@<Operator lambda lists mustn't have &aux parameters.~:@>"))
    (unless (not keyp)
      (error "~@<Operator lambda lists mustn't have &key parameters.~:@>"))
    (let* ((n-required (length required))
           (n-optional (length optional))
           (min-arguments n-required)
           (max-arguments
             (if (or rest aak)
                 (1- call-arguments-limit)
                 (+ n-required n-optional)))
           (optional
             (loop for (name init suppliedp) in optional
                   collect
                   (list
                    name
                    init
                    (or suppliedp
                        (gensym (concatenate 'string (symbol-name name) "-SUPPLIED-P"))))))
           (lambda-list
             `(,@required ,@`(&optional ,@optional) ,@(when rest `(&rest ,rest))))
           (arguments
             (alexandria:with-gensyms (head tail collect x)
               `(let* ((,head (cons nil nil))
                       (,tail ,head))
                  (flet ((,collect (,x)
                           (setf (cdr ,tail) (cons ,x nil))
                           (setf ,tail (cdr ,tail))))
                    ,@(loop for var in required collect `(,collect ,var))
                    ,@(loop for (var nil suppliedp) in optional
                            collect `(when ,suppliedp (,collect ,var)))
                    ,@(when rest
                        `((mapcar #',collect ,rest)))
                    (cdr ,head))))))
      (alexandria:once-only
          ((operator `(ensure-operator
                       :name ',name
                       :min-arguments ,min-arguments
                       :max-arguments ,max-arguments)))
        (if (not lambda-list-supplied-p)
            (alexandria:with-gensyms (fn-name)
              `(progn
                 (defun ,fn-name ()
                   (apply-noun ,operator '()))
                 (define-symbol-macro ,name (,fn-name))))
            `(progn
               (defun ,name ,lambda-list
                 (apply-verb ,operator ,arguments))
               (defvar ,name
                 (lambda ,lambda-list
                   (apply-noun ,operator ,arguments)))))))))

(defmacro define-noun (client-spec operator-spec &body clauses)
  (multiple-value-bind (client client-specializer)
      (canonicalize-client-spec client-spec)
    (multiple-value-bind (operator operator-specializer)
        (canonicalize-operator-spec operator-spec)
      (alexandria:with-gensyms (arguments)
        `(defmethod client-apply-noun-internal
             ((,client ,client-specializer)
              (,operator ,operator-specializer)
              (,arguments list))
           (declare (ignorable ,client ,operator))
           (trivia:match ,arguments
             ,@clauses
             (_ (call-next-method))))))))

(defmacro define-verb (client-spec operator-spec &body clauses)
  (multiple-value-bind (client client-specializer)
      (canonicalize-client-spec client-spec)
    (multiple-value-bind (operator operator-specializer)
        (canonicalize-operator-spec operator-spec)
      (alexandria:with-gensyms (arguments)
        `(defmethod client-apply-verb-internal
             ((,client ,client-specializer)
              (,operator ,operator-specializer)
              (,arguments list))
           (declare (ignorable ,client ,operator))
           (trivia:match ,arguments
             ,@clauses
             (_ (call-next-method))))))))

(defun canonicalize-client-spec (client-spec)
  (trivia:match client-spec
    ((type symbol)
     (values
      (gensym "CLIENT")
      client-spec))
    ((list (and variable (type symbol)) specializer)
     (values variable specializer))
    (_ (error "~@<Invalid client spec: ~S~:@>"
              client-spec))))

(defun canonicalize-operator-spec (operator-spec)
  (trivia:match operator-spec
    ((type symbol)
     (values
      (gensym "OPERATOR")
      `(eql (find-operator ',operator-spec))))
    ((list (and variable (type symbol)) specializer)
     (values variable specializer))
    (_ (error "~@<Invalid operator spec: ~S~:@>"
              operator-spec))))


