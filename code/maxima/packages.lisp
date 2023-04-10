(cl:in-package #:common-lisp-user)

(defpackage #:leibniz-maxima
  (:use #:closer-common-lisp #:leibniz-internals)
  (:export
   #:maxima
   #:expression))

;; I have no idea why the variable %e-val isn't bound right away, but only
;; during Maxima's (initialize-runtime-globals).  Many Maxima functions crash
;; without this definition.  We don't want to call that initialization function
;; ourselves, because it also clobbers many standard CL variables like
;; *print-length*, so the somewhat awkward solution is to simply define this
;; one variable ourselves.
(defvar maxima::%e-val (maxima::mget 'maxima::$%e 'maxima::$numer))
