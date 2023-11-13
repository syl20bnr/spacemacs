#+TITLE: spinner.el

Add spinners and progress-bars to the mode-line for ongoing operations.

[[file:some-spinners.gif]]

[[file:all-spinners.gif]]

* Usage

First of all, don’t forget to add ~(spinner "VERSION")~ to your package’s dependencies.

** Major-modes
1. Just call ~(spinner-start)~ and a spinner will be added to the mode-line.
2. Call ~(spinner-stop)~ on the same buffer when you want to remove it.

The default spinner is a line drawing that rotates. You can pass an
argument to ~spinner-start~ to specify which spinner you want. All
possibilities are listed in the ~spinner-types~ variable, but here are
a few examples for you to try:

- ~(spinner-start 'vertical-breathing 10)~
- ~(spinner-start 'minibox)~
- ~(spinner-start 'moon)~
- ~(spinner-start 'triangle)~

You can also define your own as a vector of strings (see the examples
in ~spinner-types~).

** Minor-modes
Minor-modes can create a spinner with ~spinner-create~ and then add it
to their mode-line lighter. They can then start the spinner by setting
a variable and calling ~spinner-start-timer~. Finally, they can stop
the spinner (and the timer) by just setting the same variable to nil.

Here’s an example for a minor-mode named ~foo~. Assuming that
~foo--lighter~ is used as the mode-line lighter, the following code
will add an *inactive* global spinner to the mode-line.
#+begin_src emacs-lisp
(defvar foo--spinner (spinner-create 'rotating-line))
(defconst foo--lighter
  '(" foo" (:eval (spinner-print foo--spinner))))
#+end_src

1. To activate the spinner, just call ~(spinner-start foo--spinner)~.
   It will show up on the mode-line and start animating.
2. To get rid of it, call ~(spinner-stop foo--spinner)~. It will then
   disappear again.

Some minor-modes will need spinners to be buffer-local. To achieve
that, just make the ~foo--spinner~ variable buffer-local and use the
third argument of the ~spinner-create~ function. The snippet below is an example.

#+begin_src emacs-lisp
(defvar-local foo--spinner nil)
(defconst foo--lighter
  '(" foo" (:eval (spinner-print foo--spinner))))
(defun foo--start-spinner ()
  "Create and start a spinner on this buffer."
  (unless foo--spinner
    (setq foo--spinner (spinner-create 'moon t)))
  (spinner-start foo--spinner))
#+end_src

1. To activate the spinner, just call ~(foo--start-spinner)~.
2. To get rid of it, call ~(spinner-stop foo--spinner)~.

This will use the ~moon~ spinner, but you can use any of the names
defined in the ~spinner-types~ variable or even define your own.

* Extra options

Both ~spinner-start~ and ~spinner-create~ take extra options to configure the spinner, these are:

- ~FPS~: The number of frames to display per second. Defaults to ~spinner-frames-per-second~.
- ~DELAY~: After starting a spinner, it still won’t be displayed for this many seconds.
