# GKMNGRGN's Emacs Config

My accessibility-friendly Emacs configuration.

![](data/interface.png)

Please **do not open** a pull-request for this repository. You can configure
your editor from scratch (Don't be afraid of LISP; it's effortless.) or give a
try for [Spacemacs](https://www.spacemacs.org/) or [Doom
Emacs](https://github.com/hlissner/doom-emacs). But first, get a new mechanical
keyboard.

## Packages

I use **use-package** to install dependencies. If you want to see the list of
packages that I use, just open **init.el** file and look at all the lines
starting with `(use-package `.

Some features need to the external dependencies. My font is **IBM Plex**, please
get it from [this link](https://www.ibm.com/plex/) or choose another font.

The other external dependency is [silversearch-ag](https://geoff.greer.fm/ag/)
for searching and filtering. It supports Windows.

## Programming Languages

### Common Lisp

[SLIME](https://common-lisp.net/project/slime/) supports many CL implementations
but I prefer to use [SBCL](http://www.sbcl.org/). If your Emacs can't find your
SBCL path, specify it
[manually](http://ergoemacs.org/emacs/emacs_custom_system.html).

### Dart

Install [Dart SDK](https://dart.dev/) or [Flutter](https://flutter.dev/), it has
a builtin analysis tool. Then customize SDK path in your editor. If you don't
know how to customize, start with [this
tutorial](http://ergoemacs.org/emacs/emacs_custom_system.html).

### Go

Install [Go](https://go.dev/) first, then type this command for LSP support:

```
go get -u golang.org/x/tools/cmd/gopls
```

### Python

Install Python with [pyenv](https://github.com/pyenv/pyenv-installer)
([pyenv-win](https://github.com/pyenv-win/pyenv-win) for Windows):

```
pyenv install 3.8.2
pyenv global 3.8.2
pip install -U pip
pip install python-langauge-server[all]
```

### Rust

Download rust-analyzer
[here](https://github.com/rust-analyzer/rust-analyzer/releases).


### YAML

Install [yaml language
server](https://github.com/redhat-developer/yaml-language-server) with npm:

```
npm install -g yaml-language-server
```

## Installation

Clone the repository to your home folder:

```
cd ~
git clone git@github.com:gkmngrgn/emacs.d.git .emacs.d
```

If you are on Windows, don't forget to add a new environment variable named
"HOME":

```
HOME="%USERPROFILE%"
```

That's all.
