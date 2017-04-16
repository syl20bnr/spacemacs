# YAML contribution layer for Spacemacs

Support for [YAML](http://yaml.org/) in Spacemacs.

## Configuration

If you want the `newline-and-indent` behavior on `RET`, add the following to your configuration:

```
(add-hook 'yaml-mode-hook
  '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
```

(from the [yaml-mode](https://github.com/yoshiki/yaml-mode) directions)

## Future

 - Ansible support (perhaps with [ansible-doc](https://github.com/lunaryorn/ansible-doc.el) and highlighting for `{{ }}` and `{% %}`)
