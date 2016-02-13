# emacs-fast-loader

A fast emacs init loader like init-loader.el.

## Requirements

`fast-loader` depends on the following shell commands.

* cd
* cat
* sed

## Install

Install `fast-loader.el` to load-path directory.

```
(require 'fast-loader)
```

## Usage

For example, the following represents init files tree.

```
~/.emacs.d
    |--- inits
    |      |---- 00-init.el
    |      |---- 01-packages.el
    |      |---- 02-theme.el
    |     ...
    |--- init.el
   ...
```

The following code enables `fast-loader` and loads `inits/*.el`.

```
(require 'fast-loader)
(fast-loader-load "~/.emacs.d/inits")
```

`fast-loader` concatenates `inits/*.el` and save the file in `~/.emacs.d/fast-loader-cache` directory.

### Auto byte compile

```
 (custom-set-variables '(fast-loader-byte-compile t))
```

## Performance

TODO:

## Contribution

1. Fork it ( http://github.com/nabeix/emacs-fast-loader )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create new Pull Request

## License

MIT
