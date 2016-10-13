# Private directory

The content of this directory is ignored by Git. This is the default place
where to store your private configuration layers.

To create a new configuration layer:

    SPC SPC configuration-layer/create-layer RET

Then enter the name of your configuration in the prompt.

A directory named after the created configuration layer will be created here
along with template files within it (packages.el and extensions.el, more info
on the meaning of those files can be found in the [documentation][conf_layers]).

Each created file has further guidance written in them.

Once the configuration is done, restart Emacs to load, install and configure
your layer.

[conf_layers]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#extensions-and-packages
