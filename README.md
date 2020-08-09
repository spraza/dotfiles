# dotfiles
A collection of my dot files

Just copy the files to your home directory to get started. See my [setup script](https://github.com/paymaan/setup-machine/blob/master/setup-machine-impl) to install anything specific that the dot files use.

I have also been playing around with [atom](https://atom.io) and [nuclide](https://nuclide.io) recently. I maintain that config using [sync-settings](https://atom.io/packages/sync-settings) package. The actual config is hosted as [my gist](https://gist.github.com/spraza/bea17df0c9661ab23e835fb3e3e0cf44).

## Chrome keybindings

I wanted my emacs key bindings in Chrome and on Linux. It wasn't as easy as I had hoped.

So I read: https://www.jefftk.com/p/emacs-keybindings-in-chrome-without-gnome. That helped.

However, I wanted to customize further. To do that, I edited `/usr/share/themes/Emacs/gtk-3.0/gtk-keys.css`. My `gtk-keys.css` file is in this git repo.
