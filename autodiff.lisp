(in-package :cl-autodiff)

(defparameter *ad-fns* (make-hash-table))
	   
(defmacro define-with-derivatives (name variable-list &body body)
  "Define a function that computes partial derivatives automatically"
  ;; Most of the work is done in MAKE-AUTODIFF. This functions just lifts the inputs 
  ;; to derivative n-tuples, which the autodiff expressions operate on.
  (let ((ad-name (intern (concatenate 'string "ADFN-" (string name)))))
    (setf (gethash name *ad-fns*) ad-name)
    (with-gensyms (result)
      `(progn 
	 (defun ,ad-name ,variable-list
	   ,@(mapcar (rcurry #'make-autodiff variable-list nil) body))
	 (defun ,name ,variable-list
	   (let* (,@(mapcar #'(lambda (x) 
			       (list x `(list ,x ,@(mapcar #'(lambda (y) 
							       (if (eq x y) 1 0)) 
							   variable-list))))
			   variable-list)
		  (,result (,ad-name ,@variable-list)))
	     (values (car ,result) (apply #'vector (cdr ,result)))))))))

(defun make-autodiff (expr var-list env)
  "Take a mathematical expression, and return a new expression 
that yields the original and each of the partial derivaties as a value"
  (cond
    ((null expr) nil)
    ((or (numberp expr) (equal expr 'pi))
     `(list ,expr ,@(make-list (length var-list) :initial-element 0)))
    ((symbolp expr) expr)
    ((listp expr)
     (let ((name (first expr)))
       (case name
	 ;; binarize each of +, -, *, and /
	 (+ (make-autodiff-binarize expr var-list env 0 'ad2-+ '+ '+))
	 (- (make-autodiff-binarize expr var-list env 0 'ad2-- '- '+))
	 (* (make-autodiff-binarize expr var-list env 1 'ad2-* '* '*))
	 (/ (make-autodiff-binarize expr var-list env 1 'ad2-/ '/ '*))
	 
	 ;; numerical predicates and numbers
	 ((< > <= >= = /= zerop minusp plusp evenp oddp 
	   rationalp integerp realp logbitp floatp complexp)
	  (make-autodiff-predicate expr var-list env))
	 
	 ;; special versions of certain arithmetic functions
	 (log 
	  (make-autodiff-log expr var-list env))
	 (atan
	  (make-autodiff-atan expr var-list env))
	 ;; passthrough forms
	 ('(declare go return-from)
	  expr)
	 ;; block-style forms
	 ((block progn progv tagbody return-from return
	    unwind-protect if setq setf funcall compose function-symbol)
	  (make-autodiff-block expr var-list env))
	 ((let let*)
	  (make-autodiff-let expr var-list env))
	 ((labels flet)
	  (make-autodiff-labels-flet expr var-list env))
	 (lambda
	     (make-autodiff-lambda expr var-list env))
	 (otherwise 
	  ;; try to expand as a macro
	  (let ((expanded (macroexpand-1 expr)))
	    (multiple-value-bind (fn hash-success) (gethash name *ad-fns*)
	      ;; if we can, auto-diff the new expression
	      (cond 
		((not (equal expr expanded))
		 (make-autodiff expanded var-list env))
		;; see if we recognize the function as one that
		;; needs expansion
		(hash-success
		 ;; if so, replace the symbol with its correct version
		 (cons fn (mapcar (rcurry #'make-autodiff var-list env) (rest expr))))
		;; if the function is the environment, we don't need to replace it
		((member name env)
		 (cons name (mapcar (rcurry #'make-autodiff var-list env) (rest expr))))
		(t 
		 ;; worst case, we assume the function is some special form, so we don't
		 ;; worry about the insides
		 expr))))))))))

(defun make-autodiff-log (expr var-list env)
  "Rewrite log by transforming the binary version with change-of-base, if necessary"
  (if (= (length expr) 3)
      (make-autodiff
       `(/ (log ,(second expr)) (log ,(third expr))) 
       var-list env)
      `(ad2-log ,(make-autodiff (second expr) var-list env)
		,(make-autodiff (third expr) var-list env))))

(defun make-autodiff-atan (expr var-list env)
  "Rewrite atan, expanded the unary form to a binary form if necessary"
  (let ((denom (if (third expr) (third expr) 1))) 
    `(ad2-atan ,(make-autodiff (second expr) var-list env)
	       ,(make-autodiff denom var-list env))))

(defun make-autodiff-predicate (expr var-list env)
  "Rewrite predicates to act only on the values, not the entire derivatives"
  (cons (first expr)
	(mapcar (compose (curry #'list 'car) (rcurry #'make-autodiff var-list env)) (rest expr))))

(defmacro define-binary-ad (sym args &body body)
  "Define the AD version of an existing binary function"
  (let ((fn-name (intern (concatenate 'string "AD2-" (string sym))))
	(deriv-body `(lambda ,args ,@body)))
    (with-gensyms (g1 g2)
      `(progn
	 (defun ,fn-name (expr1 expr2)
	   (let ((,g1 (car expr1))
		 (,g2 (car expr2)))
	     (cons
	      (,sym ,g1 ,g2)
	      (mapcar (curry #',deriv-body ,g1 ,g2) (cdr expr1) (cdr expr2)))))
	 (setf (gethash ',sym *ad-fns*) ',fn-name)
	 (setf (gethash #',sym *ad-fns*) #',fn-name)))))

(defmacro define-unary-ad (sym args &body body)
  "Define the AD version of an existing unary function"
  (let ((fn-name (intern (concatenate 'string "AD1-" (string sym))))
	(deriv-body `(lambda ,args ,@body)))
    (with-gensyms (g)
      `(progn
	 (defun ,fn-name (expr)
	   (let ((,g (car expr)))
	     (cons
	      (,sym ,g)
	      (mapcar (curry #',deriv-body ,g) (cdr expr)))))
	 (setf (gethash ',sym *ad-fns*) ',fn-name)
	 (setf (gethash #',sym *ad-fns*) #',fn-name)))))

;;; basic arithmetic operators
(define-binary-ad + (x y x1 y1)
  (declare (ignore x y))
  (+ x1 y1))
(define-binary-ad - (x y x1 y1)
  (declare (ignore x y))
  (- x1 y1))
(define-binary-ad * (x y x1 y1)
  (+ (* x y1) (* y x1)))
(define-binary-ad / (x y dx dy)
  (/ (- (* y dx) (x dy))
     (* y y)))

(define-binary-ad ash (x y dx dy)
  (+ (* dx (expt 2 y))
     (* dx (expt 2 y) (log 2) dy)))

;; binary analytic functions
(define-binary-ad expt (x y dx dy)
  (+ (* y (expt x (1- y)) dx) 
     (* (expt x y) (log x) dy)))
(define-binary-ad atan (y x dy dx)
  (/ (- (* x dy) (* y dx))
     (+ (* y y) (* x x))))

;; unary analytic functions
(define-unary-ad exp (x dx)
  (* (exp x) dx))
(define-unary-ad log (x dx)
  (/ dx x))
(define-unary-ad sqrt (x dx)
  (/ dx (* 2 (sqrt x))))

;; trig functions
(define-unary-ad sin (x dx)
  (* (cos x) dx))
(define-unary-ad cos (x dx)
  (* (sin x) (- dx)))
(define-unary-ad tan (x dx)
  (let ((c (cos x)))
    (/ dx (* c c))))
(define-unary-ad asin (x dx)
  (/ dx (sqrt (- 1 (* x x)))))
(define-unary-ad acos (x dx)
  (/ (- dx) (sqrt (- 1 (* x x)))))

;; hyperbolic trig functions
(define-unary-ad sinh (x dx)
  (* dx (cosh x)))
(define-unary-ad cosh (x dx)
  (* dx (sinh x)))
(define-unary-ad tanh (x dx)
  (let ((r (cosh x)))
    (/ dx (* r r))))
(define-unary-ad asinh (x dx)
  (/ dx (sqrt (1+ (* x x)))))
(define-unary-ad acosh (x dx)
  (/ dx (- (sqrt (1- (* x x))))))
(define-unary-ad atanh (x dx)
  (/ dx (- 1 (* x x))))

(define-unary-ad abs (x dx)
  (* dx (signum x)))

(define-unary-ad 1+ (x dx)
  (declare (ignore x))
  dx)
(define-unary-ad 1- (x dx)
  (declare (ignore x))
  dx)

;; complex functions
(define-unary-ad cis (x dx)
  (* (cis x) #C(0.0 1.0) dx))

(defun make-autodiff-block (expr var-list env)
  "Rewrite a block-form expression by preserving the original symbol and
rewriting each remaining expression"
  `(,(first expr) ,@(mapcar (rcurry #'make-autodiff var-list env) (rest expr))))

(defun make-autodiff-let (expr var-list env)
  "Rewrite the value part of each binding, and each of the block expressions"
  (let ((bindings (second expr)))
    `(,(car expr) 
       ,(mapcar #'(lambda (bd) (list (car bd) (make-autodiff (second bd) var-list env))) bindings)
       ,@(mapcar (rcurry #'make-autodiff var-list env) (cddr expr)))))

(defun make-autodiff-labels-flet (expr var-list env)
  "Transforms a labels or flet special form"
  (let* ((name (car expr))
	 (bindings (second expr))
	 (body (cddr expr))
	 ;; the local functions must be added to the environment
	 ;; for labels 
	 (new-env (union env (mapcar #'car bindings))))
    `(,name
      ,(mapcar #'(lambda (bd)
		   `(,(first bd) ,(second bd)
		      ,@(mapcar (rcurry #'make-autodiff var-list 
					(if (eq name 'labels) new-env env))
				(cddr bd))))
	       bindings)
      ,@(mapcar (rcurry #'make-autodiff var-list new-env) body))))

(defun make-autodiff-lambda (expr var-list env)
  (destructuring-bind (l args &body body) expr
    `(,l ,args ,@(mapcar (rcurry #'make-autodiff var-list env) body))))
	 

(defun make-autodiff-binarize (expr var-list env default-value
			       sym-fn sym-main sym-aux)
  "Left-binarize an expression before auto-differentiating"
  (case (length expr)
    (2 (list sym-fn 
	     (make-autodiff default-value var-list env) 
	     (make-autodiff (second expr) var-list env)))
    (3 (list sym-fn 
	     (make-autodiff (second expr) var-list env) 
	     (make-autodiff (third expr) var-list env)))
    (otherwise
     (make-autodiff 
      (list sym-main (second expr) (cons sym-aux (cddr expr)))
      var-list env))))	     
