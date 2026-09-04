# .emacs.d

My personal Emacs configuration with vanilla keybindings and the minad stack.

## Prerequisites

- **Emacs:** Latest stable release. Older or newer releases are not guaranteed
to work.
- **Git:** Required for Elpaca and Magit.
- **LSP Servers:** Required for Eglot. See [init-dev.el](lisp/init-dev.el) for
a list of servers used.

## Install

> [!WARNING]
> Back up any existing configurations before proceeding.

Clone this repository to `~/.config/emacs` or `~/.emacs.d`. Packages will be
automatically downloaded and installed upon starting up Emacs for the first
time.

```shell
git clone https://github.com/simplyshiro/.emacs.d.git ~/.config/emacs
```

## References

- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d)
- [Emacs Bedrock](https://codeberg.org/ashton314/emacs-bedrock)
