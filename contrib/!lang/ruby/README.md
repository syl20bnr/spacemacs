# Ruby contribution layer for Spacemacs

![logo](img/ruby.gif)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Ruby contribution layer for Spacemacs](#ruby-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Prerequisites](#prerequisites)
        - [Ruby version management](#ruby-version-management)
        - [Rails support](#rails-support)
    - [Key bindings](#key-bindings)
        - [Ruby (enh-ruby-mode, robe, inf-ruby)](#ruby-enh-ruby-mode-robe-inf-ruby)
        - [ruby-test-mode](#ruby-test-mode)
        - [Rails (projectile-rails)](#rails-projectile-rails)
            - [Code Navigation](#code-navigation)
            - [Refactoring](#refactoring)
            - [RUN commands](#run-commands)
            - [Ex-commands](#ex-commands)

<!-- markdown-toc end -->

## Description

This layer aims at providing support for the Ruby language using
[enh-ruby-mode][] and [robe-mode][].

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ruby))
```

### Prerequisites

Some of the advanced features supported by this layer depend on external gems
that need to be installed in the context of your project (see below for guidance
based on your version manager):

* `pry` and `pry-doc` are required for *jump to definition* and *code documentation* (`robe-mode`)
* `ruby_parser` is required for *goto-step_definition* in `feature-mode`

You can install the gems in the context of your current project by
adding them to the `Gemfile`, e.g.:

```ruby
 gem 'pry'
```

or on the command line (please refer to your ruby version manager
specific documentation for details and caveats):

```shell
$ gem install pry
```

### Ruby version management

This layer supports the use of [RVM][] and [Rbenv][].
To enable it, set the `ruby-version-manager` var in your `~/.spacemacs`:

```elisp
(defun dotspacemacs/init ()
  (setq-default ruby-version-manager 'rbenv)
)
```

Possible values are `rbenv` and `rvm`.

### Rails support

Rails support is available through [projectile-rails][].
To enable it, set the `ruby-enable-ruby-on-rails-support` var in your
`~/.spacemacs`:

```elisp
(defun dotspacemacs/init ()
  (setq-default ruby-enable-ruby-on-rails-support t)
)
```

This will also add [haml-mode][] (for templates written in [haml language][]
and [feature-mode][] for [Cucumber][] support.

## Key bindings

### Ruby (enh-ruby-mode, robe, inf-ruby)

Key binding          | Description
---------------------|------------
<kbd>SPC m g g</kbd> | go to definition (robe-jump)
<kbd>SPC m h d</kbd> | go to Documentation
<kbd>SPC m s f</kbd> | send function definition
<kbd>SPC m s F</kbd> | send function definition and switch to REPL
<kbd>SPC m s i</kbd> | start REPL
<kbd>SPC m s r</kbd> | send region
<kbd>SPC m s R</kbd> | send region and switch to REPL
<kbd>SPC m s s</kbd> | switch to REPL

### ruby-test-mode

ruby-test-mode comes bundled with spacemacs, but this contribution adds
a couple of useful keybindings:

Key binding          | Description
---------------------|------------
<kbd>SPC m t b</kbd> | run test file
<kbd>SPC m t t</kbd> | run test at pointer

### Rails (projectile-rails)

#### Code Navigation

Key binding            | Description
-----------------------|------------
<kbd>SPC m r f a</kbd> | find localization file
<kbd>SPC m r f c</kbd> | find controller
<kbd>SPC m r f e</kbd> | find environment file
<kbd>SPC m r f f</kbd> | find feature
<kbd>SPC m r f h</kbd> | find helper
<kbd>SPC m r f i</kbd> | find initializer
<kbd>SPC m r f j</kbd> | find javascript file
<kbd>SPC m r f l</kbd> | find library
<kbd>SPC m r f m</kbd> | find model
<kbd>SPC m r f n</kbd> | find migration
<kbd>SPC m r f o</kbd> | find log
<kbd>SPC m r f p</kbd> | find spec file
<kbd>SPC m r f r</kbd> | find rake task
<kbd>SPC m r f s</kbd> | find stylesheet file
<kbd>SPC m r f t</kbd> | find test
<kbd>SPC m r f u</kbd> | find fixture
<kbd>SPC m r f v</kbd> | find view
<kbd>SPC m r f y</kbd> | find layout
<kbd>SPC m r f @</kbd> | find mailer
<kbd>SPC m r g c</kbd> | go to current controller
<kbd>SPC m r g d</kbd> | go to DB schema
<kbd>SPC m r g e</kbd> | go to DB seeds
<kbd>SPC m r g h</kbd> | go to current helper
<kbd>SPC m r g j</kbd> | go to current javascript
<kbd>SPC m r g g</kbd> | go to Gemfile
<kbd>SPC m r g m</kbd> | go to current model
<kbd>SPC m r g n</kbd> | go to current migration
<kbd>SPC m r g p</kbd> | go to current spec
<kbd>SPC m r g r</kbd> | go to routes
<kbd>SPC m r g s</kbd> | go to current stylesheet
<kbd>SPC m r g t</kbd> | go to current test
<kbd>SPC m r g u</kbd> | go to current fixture
<kbd>SPC m r g v</kbd> | go to current view
<kbd>SPC m r g z</kbd> | go to spec helper
<kbd>SPC m r g .</kbd> | go to file at point (faster but less powerful than <kbd>SPC m g g</kbd>)

#### Refactoring

Key binding            | Description
-----------------------|------------
<kbd>SPC m r R x</kbd> | extract region into partial

#### RUN commands

Key binding            | Description
-----------------------|------------
<kbd>SPC m r c c</kbd> | run rails generator
<kbd>SPC m r i</kbd>   | start rails console
<kbd>SPC m r s r</kbd> | reload Rails project
<kbd>SPC m r r :</kbd> | run rake task
<kbd>SPC m r x s</kbd> | start rails server

#### Ex-commands

Key binding            | Description
-----------------------|------------
:A                     | Switch between implementation and tests

[enh-ruby-mode]: https://github.com/zenspider/enhanced-ruby-mode
[robe-mode]: https://github.com/dgutov/robe
[Rbenv]: https://github.com/sstephenson/rbenv
[RVM]: https://rvm.io/
[projectile-rails]: https://github.com/asok/projectile-rails
[haml-mode]: https://github.com/nex3/haml-mode
[feature-mode]: https://github.com/michaelklishin/cucumber.el
[rspec-mode]: https://github.com/pezra/rspec-mode
[Cucumber]: http://cukes.info
[haml language]: http://haml.info
