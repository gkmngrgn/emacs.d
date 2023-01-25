# GOEDEV's EMACS CONFIG

My accessibility-first, terminal-focused vanilla Emacs configuration.

Please **do not open** a pull-request for this repository. You can configure your editor
from scratch like me, or start with pre-configured Emacs if you're new:

- [Spacemacs](https://www.spacemacs.org/)
- [Doom Emacs](https://github.com/hlissner/doom-emacs)


## INSTALLATION

Consider installing the minimum version 28+ of Emacs. Native compilation is significant
for lsp performance. For MacOS:

    brew tap d12frosted/emacs-plus
    brew install emacs-plus --with-native-comp --without-cocoa

Use [dosh](https://github.com/gkmngrgn/dosh/) for the quick installation:

    git clone https://github.com/gkmngrgn/emacs.d.git
    cd emacs.d
    dosh install

If you are on Windows, check if the environment variable `HOME` is defined:

    HOME="%USERPROFILE%"


## COPY & PASTE PROBLEM

For Linux and WSL2, install `xsel` and after you select your text, type `M-|`, then run
the command `xsel -bi`.

For MacOS, you can use `pbcopy`.


## PACKAGES

I use built-in package manager, if you want to see the list of packages I installed,
search `goedev/install-packages` text in `init.el` file.


## SYSTEM DEPENDENCIES

I install all my CLI dependencies using my do.sh script
[here](https://git.goe.dev/goedev/config/src/branch/main/do.sh).

    brew install gopls               \
                 fd                  \
                 llvm                \
                 lua-language-server \
                 multimarkdown       \
                 ripgrep             \
                 rust-analyzer

    npm i -g typescript                   \
             typescript-language-server   \
             vscode-langservers-extracted \
             yaml-language-server
