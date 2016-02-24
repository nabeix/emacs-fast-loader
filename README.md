# emacs-fast-loader

A fast emacs init loader like init-loader.el.

## Requirements

`fast-loader` depends on the following shell commands.

* cd
* cat
* sed

## Installation

Install `fast-loader.el` to load-path directory.

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

`fast-loader` concatenates `inits/*.el` and save it as `~/.emacs.d/fast-loader-cache/inits.el`.

The concatenated file is updated automatically if you modify `inits/*.el`.

### Auto byte compile

```
 (custom-set-variables '(fast-loader-byte-compile t))
```

## Performance

Measuring condition:

* one init file consists of one function and 10 lines
* measure load time using `emacs-init-time`

### Disable byte compile

|   loader    | num of init files | time(sec) |
|:-----------:|:-----------------:|:---------:|
| fast-loader | 10                | 1.0       |
| fast-loader | 100               | 1.0       |
| fast-loader | 1000              | 1.1       |
| init-loader | 10                | 1.0       |
| init-loader | 100               | 1.1       |
| init-loader | 1000              | 2.7       |

### Enable byte compile

|   loader    | num of init files | time with compile(sec) | time(sec) |
|:-----------:|:-----------------:|:----------------------:|:---------:|
| fast-loader | 10                | 1.5                    | 1.0       |
| fast-loader | 100               | 3.0                    | 1.0       |
| fast-loader | 1000              | 28.4                   | 1.1       |
| init-loader | 10                | 2.0                    | 1.0       |
| init-loader | 100               | 5.5                    | 1.1       |
| init-loader | 1000              | 48.7                   | 2.7       |

## Contribution

1. Fork it ( http://github.com/nabeix/emacs-fast-loader )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create new Pull Request

## License

MIT
