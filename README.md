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

The other external dependency is [ripgrep][4] for searching and filtering. It
supports Windows.

## Programming Languages

### C++

Just install [LLVM][5]. It comes with Clangd, a language server for C++.

### Common Lisp

[SLIME][6] supports many CL implementations but I prefer to use [SBCL][7]. If
your Emacs can't find your SBCL path, specify it [manually][8].

### Dart

Install [Dart SDK][9] or [Flutter][10], it has a builtin analysis tool. Then
customize SDK path in your editor. If you don't know how to customize, start
with [this tutorial][11].

### Go

Install [Go][11] first, then type this command for LSP support:

```
go get -u golang.org/x/tools/gopls
```

### Python

Install Python with [pyenv][13] ([pyenv-win][14] for Windows):

```
pyenv install 3.8.2
pyenv global 3.8.2
pip install -U pip
pip install -U python-language-server pyls-mypy pyls-isort pyls-black pyflakes jedi
```

### Rust

Download rust-analyzer [here][15].


### CSS & HTML & JavaScript & JSON & TypeScript & YAML

Install all dependencies using npm:

```
npm install -g javascript-typescript-langserver \
               js-beautify \
               vscode-css-languageserver-bin \
               vscode-html-languageserver-bin \
               vscode-json-languageserver \
               yaml-language-server
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
[4]: https://github.com/BurntSushi/ripgrep/
[5]: https://clangd.llvm.org/
[6]: https://common-lisp.net/project/slime/
[7]: http://www.sbcl.org/
[8]: http://ergoemacs.org/emacs/emacs_custom_system.html
[9]: https://dart.dev/
[10]: https://flutter.dev/
[11]: http://ergoemacs.org/emacs/emacs_custom_system.html
[12]: https://go.dev/
[13]: https://github.com/pyenv/pyenv-installer
[14]: https://github.com/pyenv-win/pyenv-win
[15]: https://github.com/rust-analyzer/rust-analyzer/releases
