(in-package :cl-autodiff-asd)

(defpackage :cl-autodiff
  (:nicknames :autodiff)
  (:documentation "Macros for creating functions with automatic differentiation")
  (:use :common-lisp :alexandria)
  (:export
   define-with-derivatives))

