(in-package :cl-autodiff-system)

(defpackage :cl-autodiff-tests
  (:nicknames :autodiff-tests)
  (:documentation "Unit tests for cl-autodiff")
  (:use :common-lisp :autodiff :fiveam)
  (:export run-tests))

(in-package :cl-autodiff-tests)

(def-suite defun-ad-tests)
(in-suite defun-ad-tests)

(defun approx-= (x y)
  (< (abs (- x y)) 1e-2))

(def-fixture dummy-methods ()
  (defun-ad f1 (x) x)
  (defun-ad f2 (x y) x y)
  (defun-ad f3 (x y z) x y z)
  (defun-ad f10 (a b c d e f g h i j) a b c d e f g h i j)
  (&body))

(test grad-size
  (with-fixture dummy-methods ()
    (for-all ((x (gen-float))
	      (y (gen-float))
	      (z (gen-float)))
      (multiple-value-bind (val deriv)
	  (f1 x)
	(is (= val x)))
      (multiple-value-bind (val deriv)
	  (f2 x y)
	(is (= val y)))
      (multiple-value-bind (val deriv)
	  (f3 x y z)
	(is (= val z)))
    (is (= (f10 1 2 3 4 5 6 7 8 9 10) 10)))))

(def-fixture trig-methods ()
  (defun-ad trigs (x y z) (* (sin x) (cos y) (tan z)))
  (defun trigs-val (x y z) (* (sin x) (cos y) (tan z)))
  (defun trigs-dx (x y z) (* (cos x) (cos y) (tan z)))
  (defun trigs-dy (x y z) (* (sin x) (- (sin y)) (tan z)))
  (defun trigs-dz (x y z) (* (sin x) (cos y) (/ (* (cos z) (cos z)))))
  (&body))

(test trig   
  (with-fixture trig-methods ()
    (for-all ((x (gen-float :bound (* 2 pi)))
	      (y (gen-float :bound (* 2 pi)))
	      (z (gen-float :bound (/ pi 0.52))))
      (multiple-value-bind (val deriv)
	  (trigs x y z)
	(is (approx-= val (trigs-val x y z)))
	(is (approx-= (trigs-no-ad x y z) (trigs-val x y z)))
	(is (approx-= (svref deriv 0) (trigs-dx x y z)))
	(is (approx-= (svref deriv 1) (trigs-dy x y z)))
	(is (approx-= (svref deriv 2) (trigs-dz x y z)))))))

			  
(defun run-tests ()
  (run! 'defun-ad-tests))