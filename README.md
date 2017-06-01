# auto-minor-mode

This package lets you enable minor modes based on file name and
contents. To find the right modes, it checks filenames against
patterns in `auto-minor-mode-alist` and file contents against
`auto-minor-mode-magic-alist`. These work like the built-in Emacs
variables `auto-mode-alist` and `magic-mode-alist`.

Unlike major modes, all matching minor modes are enabled, not only
the first match.

A reason you might want to use it:

    (add-to-list 'auto-minor-mode-alist '("-theme\\.el\\'" . rainbow-mode))

There's intentionally no equivalent of `interpreter-mode-alist`.
Interpreters should determine the major mode. Relevant minor modes can
then be enabled by major mode hooks.

Minor modes are set whenever `set-auto-mode`, the built-in function
responsible for handling automatic major modes, is called.

## License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.
