# GOEDEV's EMACS CONFIG

My accessibility-first, terminal-focused vanilla Emacs configuration. The minimum supported Emacs version is **29**.

Please **do not open** a pull-request for this repository. You can configure your editor from scratch like me, or start with a pre-configured Emacs distro if you're new:

- [Spacemacs](https://www.spacemacs.org/)
- [Doom Emacs](https://github.com/hlissner/doom-emacs)
- [Centaur Emacs](https://github.com/seagle0128/.emacs.d)
- [JCS Emacs](https://github.com/jcs-emacs/jcs-emacs)

## INSTALLATION

You can choose any installation method you want for Emacs, but if you are on Windows, I suggest you to define `HOME` environment variable first.

```shell
HOME="%USERPROFILE%"
```

I use [dosh](https://github.com/gkmngrgn/dosh-cli) to configure my Emacs with a one-line command:

```shell
dosh setup   # replace config files.
dosh install # install packages.
```

## COPY & PASTE PROBLEM

With [wezterm](https://wezfurlong.org/wezterm/), I don't have any problem with copy & paste. But if you are using other terminal emulator, you may have some problem with copy & paste. For Linux and WSL2, install `xsel` and after you select your text, type `M-|`, then run the command `xsel -bi`.

For MacOS, you can use `pbcopy`.
