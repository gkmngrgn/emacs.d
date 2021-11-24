# GKMNGRGN's Emacs Config

My accessibility-friendly Emacs configuration.

![](data/interface.png)

Please **do not open** a pull-request for this repository. You can configure your editor from scratch (Don't be afraid of LISP; it's effortless.) or give a try for [Spacemacs][1] or [Doom Emacs][2]. But first, get a new mechanical keyboard.

## My Emacs Build Configuration

    sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa
    sudo apt update
    sudo apt install -y autoconf \
                        make \
                        checkinstall \
                        pkg-config \
                        texinfo \
                        libgnutls28-dev \
                        libncurses5-dev \
                        libjansson-dev \
                        libgccjit-9-dev \
                        gcc-10 \
                        g++-10 \
                        zlib1g-dev

    git clone -b emacs-28 --single-branch --depth=1 https://git.savannah.gnu.org/git/emacs.git
    cd emacs

    ./autogen.sh
    ./configure --prefix=$HOME/.local \
                --with-native-compilation \
                --with-x-toolkit=no \
                --with-xpm=ifavailable \
                --with-jpeg=ifavailable \
                --with-gif=ifavailable \
                --with-tiff=ifavailable \
                --without-xft \
                --without-x

    make -j$(nproc)
    make install


## Installation

Clone the repository to your home folder:

    cd ~
    git clone https://github.com/gkmngrgn/emacs.d.git .emacs.d

If you are on Windows, don't forget to add a new environment variable named "HOME":

    HOME="%USERPROFILE%"


## Copy & Paste Problem

For Linux, install `xsel` and after you select your text, type `M-|`,
then run the command `xsel -bi`.

For MacOS, you can use `pbcopy`.


## Emacs as a Daemon

If you want to run Emacs as a Daemon on system startup:

    mkdir -p ~/.config/systemd/user/
    cp ~/.emacs.d/emacs.service ~/.config/systemd/user/emacs.service
    systemctl --user enable --now emacs

To restart it:

    systemctl --user restart emacs


## Packages

I use **use-package** to install dependencies. If you want to see the list of packages that I use, just open **init.el** file and look at all the lines starting with `straight-use-package `.

The other external dependency is [ripgrep][4] for searching and filtering. It supports Windows.


## Programming Languages

### C & C++ & CMake

Install **LLVM**, it comes with a language server named [Clangd][5]. Install also [Cmake][6], you will need Python **PIP** to install the language server.


    python -m pip cmake-language-server


### Common Lisp

[SLIME][7] supports many CL implementations but I prefer to use [SBCL][8]. If your Emacs can't find your SBCL path, specify it [manually][9].

### Dart

Install [Dart SDK][10] or [Flutter][11], it has a builtin analysis tool. Then customize SDK path in your editor. If you don't know how to customize, start with [this tutorial][12].


### Go

Install [Go][13] first, then type this command for LSP support:

    go get -u golang.org/x/tools/gopls


### Python

    python -m pip install python-lsp-server


### Rust

Download rust-analyzer [here][14].

[1]: https://www.spacemacs.org/
[2]: https://github.com/hlissner/doom-emacs
[4]: https://github.com/BurntSushi/ripgrep/
[5]: https://clangd.llvm.org/
[6]: https://cmake.org/download/
[7]: https://common-lisp.net/project/slime/
[8]: http://www.sbcl.org/
[9]: http://ergoemacs.org/emacs/emacs_custom_system.html
[10]: https://dart.dev/
[11]: https://flutter.dev/
[12]: http://ergoemacs.org/emacs/emacs_custom_system.html
[13]: https://go.dev/
[14]: https://github.com/rust-analyzer/rust-analyzer/releases
