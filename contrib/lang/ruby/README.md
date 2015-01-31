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

### enh-ruby-mode

<kbd>SPC m i</kbd> start REPL
<kbd>SPC m g</kbd> go to definition (robe-jump)
<kbd>SPC m d</kbd> go to Documentation
<kbd>SPC m R</kbd> reload environment (Rails)

### ruby-test-mode

ruby-test-mode comes bundled with spacemacs, but this contribution adds
a couple of useful keybindings:

<kbd>SPC m t b</kbd> run test file
<kbd>SPC m t t</kbd> run test at pointer

### projectile-rails

#### GOTO commands

<kbd>SPC m r f</kbd> goto file at point
<kbd>SPC m r g</kbd> goto Gemfile
<kbd>SPC m r r</kbd> goto routes file
<kbd>SPC m r d</kbd> goto DB schema file
<kbd>SPC m r s</kbd> goto DB seeds file
<kbd>SPC m r h</kbd> goto spec helper

#### Find commands
<kbd>SPC m r m</kbd> find rails model
<kbd>SPC m r M</kbd> find current rails model

<kbd>SPC m r c</kbd> find rails controller
<kbd>SPC m r C</kbd> find current rails controller

<kbd>SPC m r v</kbd> find rails view
<kbd>SPC m r V</kbd> find current rails view

<kbd>SPC m r j</kbd> find javascript file
<kbd>SPC m r J</kbd> find current javascript

<kbd>SPC m r s</kbd> find stylesheet file
<kbd>SPC m r S</kbd> find current stylesheet

<kbd>SPC m r h</kbd> find rails helper
<kbd>SPC m r H</kbd> find current rails helper

<kbd>SPC m r p</kbd> find spec file
<kbd>SPC m r P</kbd> find current spec file

<kbd>SPC m r t</kbd> find test file
<kbd>SPC m r T</kbd> find current test file

<kbd>SPC m r n</kbd> find migration
<kbd>SPC m r N</kbd> find current migration

<kbd>SPC m r u</kbd> find fixture
<kbd>SPC m r U</kbd> find current fixture

<kbd>SPC m r l</kbd> find lib file
<kbd>SPC m r f</kbd> find feature file
<kbd>SPC m r i</kbd> find initializer
<kbd>SPC m r o</kbd> find log
<kbd>SPC m r e</kbd> find environment file
<kbd>SPC m r a</kbd> find localization file
<kbd>SPC m r @</kbd> find mailer
<kbd>SPC m r y</kbd> find layout
<kbd>SPC m r k</kbd> find rake task

<kbd>SPC m r x</kbd> extract region into partial

#### RUN commands
<kbd>SPC mrc</kbd> start rails console
<kbd>SPC mrs</kbd> start rails server
<kbd>SPC mrr</kbd> run rake task
<kbd>SPC mrg</kbd> run rails generator


[enh-ruby-mode]: https://github.com/zenspider/enhanced-ruby-mode
[robe-mode]: https://github.com/dgutov/robe
[Rbenv]: https://github.com/sstephenson/rbenv
[RVM]: https://rvm.io/
[projectile-rails]: https://github.com/asok/projectile-rails
[haml-mode]: https://github.com/nex3/haml-mode
[feature-mode]: https://github.com/michaelklishin/cucumber.el
[rspec-mode]: https://github.com/pezra/rspec-mode
