(defpackage #:cl-autodiff-asd
  (:use :cl :asdf))

(in-package :cl-autodiff-asd)

(defsystem :cl-autodiff
  :description "Macros for creating automatically-differentiating functions"
  :version "0.1"
  :author "Mason Smith <masonium@gmail.com>"
  :license "LLGPL"
  :components ((:file "package") 
	       (:file "autodiff" :depends-on ("package")))
  :depends-on (:alexandria))
