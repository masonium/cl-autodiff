(defpackage #:cl-autodiff-system
  (:use #:cl #:asdf))

(in-package :cl-autodiff-system)

(when (find-system :asdf-system-connections)
  (asdf:oos 'asdf:load-op :asdf-system-connections))

(defsystem :cl-autodiff
  :description "Macros for creating automatically-differentiating functions"
  :version "0.2"
  :author "Mason Smith <masonium@gmail.com>"
  :maintainer "Mason Smith <masonium@gmail.com>"
  :license "LLGPL"
  :components ((:file "package") 
	       (:file "autodiff" :depends-on ("package")))
  :depends-on (:alexandria))

(when (find-system :fiveam)
  (asdf:oos 'asdf:load-op :fiveam))

#+asdf-system-connections
(defsystem-connection cl-autodiff-tests
    :requires (cl-autodiff fiveam)
    :components ((:file "tests")))
