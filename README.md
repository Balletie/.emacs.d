My Emacs configuration
======================

This config uses borg to manage packages.

About `borg.el`
---------------

*For more information see the [announcement][init] and the [manual].*

[Borg] is a bare-bones package manager for Emacs packages.  It
provides only a few essential features and should be combined with
other tools such as Magit, `epkg`, `use-package`, and `auto-compile`.

Borg assimilates packages into the `~/.emacs.d` repository as Git
submodules.  An assimilated package is called a drone and a borg-based
`~/.emacs.d` repository is called a collective.

[init]:    https://emacsair.me/2016/05/17/assimilate-emacs-packages-as-git-submodules
[Borg]:    https://gitlab.com/tarsius/borg
[manual]:  https://emacsmirror.net/manual/borg
