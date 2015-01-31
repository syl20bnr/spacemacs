# Ruby contribution layer for Spacemacs

![logo](img/ruby.gif)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Ruby contribution layer for Spacemacs](#ruby-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Ruby version manager](#ruby-version-manager)
    - [Prerequisites](#prerequisites)
    - [Ruby version management](#ruby-version-management)
    - [Rails support](#rails-support)
   - [Key bindings](#key-bindings)
        - [enh-ruby-mode](#enh-ruby-mode)
        - [ruby-test-mode](#ruby-test-mode)

<!-- markdown-toc end -->

## Description

This layer aims at providing support for the Ruby language using
[enh-ruby-mode][] and [robe-mode][].

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ruby)
  "List of contribution to load."
)
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
To enable it, set the `ruby-on-rails-support` var in your
`~/.spacemacs`:

```elisp
(defun dotspacemacs/init ()
  (setq-default ruby-on-rails-support t)
)
```

This will also add [haml-mode][] (for templates written in [haml
language](http://haml.info) and [feature-mode][] for
[Cucumber](http://cukes.info) support.

## Key bindings

### Ruby (enh-ruby-mode, robe, inf-ruby)

Key binding          | Description
---------------------|------------
<kbd>SPC m g g</kbd> | go to definition (robe-jump)
<kbd>SPC m h d</kbd> | go to Documentation
<kbd>SPC m s i</kbd> | start REPL
<kbd>SPC m s f</kbd> | send function definition
<kbd>SPC m s F</kbd> | send function definition and switch to REPL
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

Key binding          | Description
---------------------|------------
<kbd>SPC m g A</kbd> | go to localization
<kbd>SPC m g C</kbd> | go to controller
<kbd>SPC m g c</kbd> | go to current controller
<kbd>SPC m g d</kbd> | go to DB schema
<kbd>SPC m g e</kbd> | go to DB seeds
<kbd>SPC m g E</kbd> | go to environment
<kbd>SPC m g F</kbd> | go to feature
<kbd>SPC m g H</kbd> | go to helper
<kbd>SPC m g h</kbd> | go to current helper
<kbd>SPC m g I</kbd> | go to initializer
<kbd>SPC m g J</kbd> | go to javascript
<kbd>SPC m g j</kbd> | go to current javascript
<kbd>SPC m g K</kbd> | go to rake task
<kbd>SPC m g L</kbd> | go to lib
<kbd>SPC m g l</kbd> | go to Gemfile
<kbd>SPC m g M</kbd> | go to model
<kbd>SPC m g m</kbd> | go to current model
<kbd>SPC m g N</kbd> | go to migration
<kbd>SPC m g n</kbd> | go to current migration
<kbd>SPC m g O</kbd> | go to log
<kbd>SPC m g P</kbd> | go to spec
<kbd>SPC m g p</kbd> | go to current spec
<kbd>SPC m g r</kbd> | go to routes
<kbd>SPC m g S</kbd> | go to stylesheet
<kbd>SPC m g s</kbd> | go to current stylesheet
<kbd>SPC m g T</kbd> | go to test
<kbd>SPC m g t</kbd> | go to current test
<kbd>SPC m g U</kbd> | go to fixture
<kbd>SPC m g u</kbd> | go to current fixture
<kbd>SPC m g V</kbd> | go to view
<kbd>SPC m g v</kbd> | go to current view
<kbd>SPC m g x</kbd> | go to spec helper
<kbd>SPC m g Y</kbd> | go to layout
<kbd>SPC m g @</kbd> | go to mailer
<kbd>SPC m g .</kbd> | go to file at point (faster but less powerful than <kbd>SPC m g g</kbd>)

#### Refactoring

Key binding          | Description
---------------------|------------
<kbd>SPC m r x</kbd> | extract region into partial

#### RUN commands

Key binding          | Description
---------------------|------------
<kbd>SPC m R c</kbd> | start rails console
<kbd>SPC m R s</kbd> | start rails server
<kbd>SPC m R k</kbd> | run rake task
<kbd>SPC m R g</kbd> | run rails generator
<kbd>SPC m R R</kbd> | reload project (REPL)


[enh-ruby-mode]: https://github.com/zenspider/enhanced-ruby-mode
[robe-mode]: https://github.com/dgutov/robe
[Rbenv]: https://github.com/sstephenson/rbenv
[RVM]: https://rvm.io/
[projectile-rails]: https://github.com/asok/projectile-rails
[haml-mode]: https://github.com/nex3/haml-mode
[feature-mode]: https://github.com/michaelklishin/cucumber.el
[rspec-mode]: https://github.com/pezra/rspec-mode
