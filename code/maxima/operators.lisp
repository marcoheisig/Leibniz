(in-package #:leibniz-maxima)

;;; Constants

(define-noun maxima leibniz:+e+
  ((list) 'maxima::$%e))

(define-noun maxima leibniz:+i+
  ((list) 'maxima::$%i))

(define-noun maxima leibniz:+gamma+
  ((list) 'maxima::$%gamma))

(define-noun maxima leibniz:+phi+
  ((list) 'maxima::$%phi))

(define-noun maxima leibniz:+pi+
  ((list) 'maxima::$%pi))

;;; Common Lisp Operators

(macrolet ((def (leibniz-operator maxima-operator)
             `(define-noun maxima ,leibniz-operator
                ((list _) t)
                ((list number1 number2)
                 `((,',maxima-operator) ,number1 ,number2))
                ((list* number more-numbers)
                 `((maxima::$mand)
                   ,@(loop
                       for a = number then b
                       for b in more-numbers
                       collect `((,',maxima-operator) ,a ,b)))))))
  (def leibniz:= maxima::mequal)
  (def leibniz:< maxima::mlessp)
  (def leibniz:> maxima::mgreaterp)
  (def leibniz:<= maxima::mleqp)
  (def leibniz:>= maxima::mgeqp))

(defun map-pairs (fn list)
  (loop for sublist on list
        for a = (first sublist) do
          (loop for b in (rest sublist) do
            (funcall fn a b))))

(define-noun maxima leibniz:/=
  ((list _) t)
  ((list number1 number2)
   `((maxima::mnotequal) ,number1 ,number2))
  ((list* numbers)
   (let ((booleans '()))
     (map-pairs
      (lambda (a b)
        (push `((maxima::mnotequal) ,a ,b)
              booleans))
      numbers)
     `((maxima::mand) ,@booleans))))

;; TODO min, max

;; TODO floor, ceiling, truncate, round

(macrolet ((def (leibniz-operator maxima-operator)
             `(progn
                (define-noun maxima ,leibniz-operator
                  ((list number) `((,',maxima-operator) ,number)))
                (define-verb maxima ,leibniz-operator
                  ((list number) (,maxima-operator number))))))
  (def leibniz:sin maxima::$sin)
  (def leibniz:cos maxima::$cos)
  (def leibniz:tan maxima::$tan)
  (def leibniz:asin maxima::$asin)
  (def leibniz:acos maxima::$acos)
  (def leibniz:sinh maxima::$sinh)
  (def leibniz:cosh maxima::$cosh)
  (def leibniz:tanh maxima::$tanh)
  (def leibniz:asinh maxima::$asinh)
  (def leibniz:acosh maxima::$acosh)
  (def leibniz:atanh maxima::$atanh))

(define-noun maxima leibniz:atan
  ((list number)
   `((maxima::$atan) ,number))
  ((list number1 number2)
   `((maxima::$atan2) ,number1 ,number2)))

(define-verb maxima leibniz:atan
  ((list number)
   (maxima::$atan number)))

(define-noun maxima leibniz:*
  ((list) 1)
  ((list number) number)
  ((list* numbers) `((maxima::mtimes) ,@numbers)))

(define-noun maxima leibniz:+
  ((list) 0)
  ((list number) number)
  ((list* numbers) `((maxima::mplus) ,@numbers)))

(define-noun maxima leibniz:-
  ((list number)
   `((maxima::mminus) ,number))
  ((list* number more-numbers)
   `((maxima::mplus)
     ,number
     ,@(mapcar
        (lambda (x)
          `((maxima::mminus) ,x))
        more-numbers))))

(define-noun maxima leibniz:/
  ((list number)
   `((maxima::mquotient) 1 ,number))
  ((list* numbers)
   (reduce
    (lambda (a b)
      `((maxima::mquotient) ,a ,b))
    numbers)))

(define-noun maxima leibniz:1+
  ((list number)
   `((maxima::mplus) ,number 1)))

(define-noun maxima leibniz:1-
  ((list number)
   `((maxima::mplus) ,number -1)))

(define-noun maxima leibniz:abs
  ((list number)
   `((maxima::$abs) ,number)))

(define-verb maxima leibniz:abs
  ((list number)
   (maxima::$abs number)))

;; TODO evenp, oddp

(define-noun maxima leibniz:exp
  ((list number)
   `((maxima::$exp) ,number)))

(define-verb maxima leibniz:exp
  ((list number)
   (maxima::$exp number)))

(define-noun maxima leibniz:expt
  ((list base power)
   `((maxima::mexpt) ,base ,power)))

;;; Other Operators

(define-noun maxima leibniz:free-variables
  ((list expression)
   `((maxima::$listofvars) ,expression)))

(define-verb maxima leibniz:free-variables
  ((list expression)
   (maxima::$listofvars expression)))

(define-noun maxima leibniz:integrate
  ((list* arguments)
   `((maxima::$integrate) ,@arguments)))

(define-verb maxima leibniz:integrate
  ((list* arguments)
   (apply #'maxima::$integrate arguments)))

(define-noun maxima leibniz:diff
  ((list* arguments)
   `((maxima::$diff) ,@arguments)))

(define-verb maxima leibniz:diff
  ((list* arguments)
   (apply #'maxima::$diff arguments)))

(define-noun maxima leibniz:solve
  ((list* arguments)
   `((maxima::$solve) ,@arguments)))

(define-verb maxima leibniz:solve
  ((list* arguments)
   (apply #'maxima::$solve arguments)))

(define-noun maxima leibniz:factor
  ((list expression)
   `((maxima::$factor) ,expression)))

(define-verb maxima leibniz:factor
  ((list expression)
   (maxima::$factor expression)))

(define-noun maxima leibniz:expand
  ((list expression)
   `((maxima::$expand) ,expression)))

(define-verb maxima leibniz:expand
  ((list expression)
   (maxima::$expand expression)))
