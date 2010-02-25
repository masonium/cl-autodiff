# cl-autodiff
Written by Mason Smith [masonium@gmail.com](mailto:masonium@gmail.com)

1. [What is cl-autodiff?](#whatis)
2. [Installation](#install)
3. [Usage](#usage)
4. [Limitations](#limit)
5. [TODO](#todo)

<h2 id="whatis">1. What is cl-autodiff?</h2>
cl-autodiff is a library for automatic differentiation of mathematical
functions. [Automatic Differentiation](http://www.autodiff.org) is a technique
whereby derivatives of function are computed to machine precision, without the
need for programming the derivative explicitly and without resorting to
numerical approximation. It is licensed using the LLGPL, which should be
included with a copy of this distribution. If you find a use for cl-autodiff, be
sure to let me know. 

<h2 id="install">2. Installation</h2>
cl-autodiff is installable via ASDF. Simply make a symbolic link to
cl-autodiff.asd in your ASDF directory, and use `(asdf:oos 'asdf:load-op
:cl-autodiff)` to compile and load the program. Alternatively, if you use
clbuild, you can add cl-autodiff to your wnpp-projects file. The repo is located
at: 

http://github.com/masonium/cl-autodiff.git

<h2 id="usage">3. Usage</h2>

Simply put, use `DEFINE-WITH-DERIVATIVES` as you would `DEFUN` to define mathematical functions.

For instance, `p(x) = 3*x^2 - 2*x + 1`.

    CL-USER> (define-with-derivatives p (x) (+ (* 3 (expt x 2)) (* -2 x) 1))
    >> P

When you evaluate P at point now, P returns both the value and the derivative at that point.

    CL-USER> (p 3)
    >> 22
    #(16)

Note that the second value is a vector. `DEFINE-WITH-DERIVATIVES` computes the gradient, so your function can have any number of inputs. For instance, take q(x, y, z) = cos(x*y) - y*sin(z)

    CL-USER> (define-with-derivatives q (x y z) (- (cos (* x y)) (* y (sin z))))
    >> Q

    CL-USER> (q pi 1 (/ pi 2))
    >> -2
     #(0 -1 0)

`DEFINE-WITH-DERIVATIVES` works with virtually all analytic* Common Lisp math functions. It also works seamlessly with complex numbers.

    CL-USER> (define-with-derivatives r (z) (* z (sqrt z)))
    >> R

    CL-USER> (r #C(0.0 1.0))
    >> #C(-0.70710677 0.70710677)
    #(#C(1.0606601 1.0606601))

To a reasonable extent, it understands let, labels, etc. as well. One can even define recursive functions, assuming the underlying function is differentiable.

    CL-USER> (define-with-derivatives recur-cube (x)
               (labels ((f (x n) 
  	           (if (> n 0) (* x (f x (1- n))) 1)))
                 (f x 3)))
    >> RECUR-CUBE

    CL-USER> (recur-cube 2)
    8
    #(12)

Note that you can also use your user-defined functions in other functions.

    CL-USER> (define-with-derivatives sixth-power (x) 
	       (let ((y (recur-cube x)))
	         (* y y)))
    >> SIXTH-POWER

    CL-USER> (SIXTH-POWER -1)
    1
    #(-6)

`DEFUN-AD` is an alias for `DEFINE-WITH-DERIVATIVES`. 

`LAMBDA-AD` also does what you would expect.

    CL-USER> (funcall (lambda-ad (x y) (/ (expt x 3) (expt y 3))) 2 2)
    >> 1
    #(1.5 -1.5)

<h2 id="limit">4. Limitations</h2>
cl-autodiff is still in, at best, a pre-alpha stage. As far as I have tested it
(which has been almost not at all), it seems to work. However, there are a
number of known limitations (and some unknown ones, I'm sure). 

* The only way of defining functions with automatic derivatives is with the
  `DEFINE-WITH-DERIVATIVES` macro. As of yet, there is no equivalent of `LABELS`
  <del>or `LAMBDA`</del> for it.

* `LAMBDA-AD` doesn't have the syntactic leeway that `LAMBDA` does. In
  particular, neither `((lambda-ad (x) x) 1)` nor `(funcall #'(lambda (x) x) 1)`
  will work.
  
* <del>`DEFINE-WITH-DERIVATIVES` does not handle some special forms very well. Theoretically it can handle do loops, for instance, but I haven't tested it. The problem is that `DEFINE-WITH-DERIVATIVES` does nothing to a form it doesn't recognize. An alternate (and probably more effective) approach would be to assume that, for any unrecognized form, the subforms should be parsed. I'll probably change this in a few versions.</del>

* `DEFINE-WITH-DERIVATIVES` should be able to handle all macros, special forms,
  etc., but the functionality has not been tested at all. In fact, testing
  overall is rather limited. 
  
* `DEFINE-WITH-DERIVATIVES` can handle lambdas, but it CANNOT handle #'. So, for instance, (funcall (lambda (x) (+ x 3)) y) should differentiate fine (w/rt y, of course), but (funcall #'+ y 3) won't work at all. 

In general, if you stick with the built in mathematical operators, labels,
flets, let*, lets, and lambdas, you should be fine. As far as I can tell, if you
can compile/evaluate a form and get an answer at all, it will probably be correct.



<h2 id="todo">5. TODO</h2>
* [labels/flet]-with-derivatives

* Thorough unit testing

* Experiment with an operator overloading version
  - would require non-standard functions (ad-+, ad-sin, etc.)
