nose.el
=======

This gives a bunch of functions that handle running nosetests on a
particular buffer or part of a buffer.

This is a fork from the [bitbucket repository][fork].

What's different ?
------------------

This fork:
- brings Windows compatibility.
- calls python with an inline script to launch nose.
- can launch test suites (require to install the nose fixes via
`easy_install nose-fixes`)
- is compatible with virtualenv

Install
-------

You'll need to add the directory containing `nose.el` to your `load-path`,
and then

    (require 'nose)

Usage
-------

By default, the root of a project is found by looking for any of the files
`setup.cfg`, `.hg`, `.git` and `.projectile`. You can add files to check
for to the file list:

    (add-to-list 'nose-project-root-files "something")

or you can change the project root test to detect in some other way
whether a directory is the project root:

    (setq nose-project-root-test (lambda (dirname) (equal dirname "foo")))

If you want dots as output, rather than the verbose output:

    (defvar nose-use-verbose nil) ; default is t

Probably also want some key bindings:

    (add-hook 'python-mode-hook
              (lambda ()
                (local-set-key "\C-ca" 'nosetests-all)
                (local-set-key "\C-cm" 'nosetests-module)
                (local-set-key "\C-cs" 'nosetests-suite)
                (local-set-key "\C-c." 'nosetests-one)
                (local-set-key "\C-cpa" 'nosetests-pdb-all)
                (local-set-key "\C-cpm" 'nosetests-pdb-module)
                (local-set-key "\C-cps" 'nosetests-pdb-suite)
                (local-set-key "\C-cp." 'nosetests-pdb-one)))

Notes
------

To be able to launch a test suite, your suite must define a function with
the name `load_tests`.

For instance (typical example to make `PyDev` *and* `nose.el` happy):

    import unittest

    ALL_TESTS = unittest.TestSuite([my_suites_go_here])

    def load_tests(loader=None, tests=None, pattern=None):
        return ALL_TESTS

    if __name__ == '__main__':
        unittest.TextTestRunner(verbosity=2).run(ALL_TESTS)

Thanks
------

To the original authors of nose.el:  `Jason Pellerin` and `Augie Fackler`

[fork]: https://bitbucket.org/durin42/nosemacs/overview
