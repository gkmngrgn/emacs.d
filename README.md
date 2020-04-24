# GKMNGRGN's Emacs Config

My accessibility-friendly Emacs configuration.

![](data/interface.png)

Please **do not open** a pull-request for this repository. You can configure
your editor from scratch (Don't be afraid of LISP; it's effortless.) or give a
try for [Spacemacs][1] or [Doom Emacs][2]. But first, get a new mechanical
keyboard.

## Packages

I use **use-package** to install dependencies. If you want to see the list of
packages that I use, just open **init.el** file and look at all the lines
starting with `(use-package `.

Some features need to the external dependencies. My font is **IBM Plex**, please
get it from [this link][3] or choose another font.

The other external dependency is [silversearch-ag][4] for searching and
filtering. It supports Windows.

## Programming Languages

### Common Lisp

[SLIME][5] supports many CL implementations but I prefer to use [SBCL][6]. If
your Emacs can't find your SBCL path, specify it [manually][7].

### Dart

Install [Dart SDK][8] or [Flutter][9], it has a builtin analysis tool. Then
customize SDK path in your editor. If you don't know how to customize, start
with [this tutorial][10].

### Go

Install [Go][11] first, then type this command for LSP support:

```
go get -u golang.org/x/tools/gopls
```

### Python

Install Python with [pyenv][12] ([pyenv-win][13] for Windows):

```
pyenv install 3.8.2
pyenv global 3.8.2
pip install -U pip
pip install python-language-server pyls-mypy pyls-isort pyls-black pyflakes jedi
```

### Rust

Download rust-analyzer [here][14].


### YAML

Install [yaml language server][15] with npm:

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

[1]: https://www.spacemacs.org/
[2]: https://github.com/hlissner/doom-emacs
[3]: https://www.ibm.com/plex/
[4]: https://geoff.greer.fm/ag/
[5]: https://common-lisp.net/project/slime/
[6]: http://www.sbcl.org/
[7]: http://ergoemacs.org/emacs/emacs_custom_system.html
[8]: https://dart.dev/
[9]: https://flutter.dev/
[10]: http://ergoemacs.org/emacs/emacs_custom_system.html
[11]: https://go.dev/
[12]: https://github.com/pyenv/pyenv-installer
[13]: https://github.com/pyenv-win/pyenv-win
[14]: https://github.com/rust-analyzer/rust-analyzer/releases
[15]: https://github.com/redhat-developer/yaml-language-server
