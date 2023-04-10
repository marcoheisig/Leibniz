(in-package #:leibniz)

;;; Constants

(define-operator +e+)

(define-operator +i+)

(define-operator +gamma+)

(define-operator +phi+)

(define-operator +pi+)

;;; Common Lisp Operators

(define-operator = (number &rest more-numbers))

(define-operator /= (number &rest more-numbers))

(define-operator < (number &rest more-numbers))

(define-operator > (number &rest more-numbers))

(define-operator <= (number &rest more-numbers))

(define-operator >= (number &rest more-numbers))

(define-operator max (number &rest more-numbers))

(define-operator min (number &rest more-numbers))

(define-operator floor (number &optional divisor))

(define-operator ceiling (number &optional divisor))

(define-operator truncate (number &optional divisor))

(define-operator round (number &optional divisor))

(define-operator sin (number))

(define-operator cos (number))

(define-operator tan (number))

(define-operator asin (number))

(define-operator acos (number))

(define-operator atan (number1 &optional number2))

(define-operator sinh (number))

(define-operator cosh (number))

(define-operator tanh (number))

(define-operator asinh (number))

(define-operator acosh (number))

(define-operator atanh (number))

(define-operator * (&rest numbers))

(define-operator + (&rest numbers))

(define-operator - (number &rest more-numbers))

(define-operator / (number &rest more-numbers))

(define-operator 1+ (number))

(define-operator 1- (number))

(define-operator abs (number))

(define-operator evenp (number))

(define-operator oddp (number))

(define-operator exp (number))

(define-operator expt (base power))

(define-operator gcd (&rest integers))

(define-operator lcm (&rest integers))

(define-operator log (number &optional base))

(define-operator mod (number divisor))

(define-operator rem (number divisor))

(define-operator signum (number))

(define-operator sqrt (number))

(define-operator isqrt (natural-number))

(define-operator cis (radians))

(define-operator conjugate (number))

(define-operator phase (number))

(define-operator realpart (number))

(define-operator imagpart (number))

(define-operator ash (integer count))

(define-operator integer-length (integer))

;;; Other Operators

(define-operator free-variables (expression))

(define-operator integrate (expression variable &optional a b))

(define-operator diff (expression &optional variable))

(define-operator solve (expression variable))

(define-operator factor (expression))

(define-operator expand (expression))

;;; Metadata

(defparameter *cl-operators*
  (loop for operator being the hash-keys of leibniz-internals:*operators*
        when (find-symbol (symbol-name operator) "CL")
          collect operator)
  "A list with the names of all Leibniz operators that have the same name as a
  standard Common Lisp symbol.  Useful for shadowing those symbols within
  package definitions.")
