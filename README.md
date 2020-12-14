# gcalcli-mode

Small and simple Emacs mode for viewing gcalcli output.

[gcalcli](https://github.com/insanum/gcalcli) is a python application
that allows you to access one or more Google Calendars from the
command line. This package provides a very simple mode for viewing the
output of `gcalcli agenda`, moving forward/backward in time, and
handling multliple google accounts.

This mode was inspired by the blog post
[How I view my google calendar agenda in Emacs](http://pragmaticemacs.com/emacs/how-i-view-my-google-calendar-agenda-in-emacs/).

## Installation

First [install gcalcli](https://github.com/insanum/gcalcli#installation).

It is recommended to get the agenda view configured how you like using
the standard option file setup.

Next, install this package. Clone the repo and load in emacs:

```elisp
(add-to-list 'load-path "~/path/to/gcalcli-mode")
(autoload 'gcalcli-agenda "gcalcli-agenda" nil t)
```

or with [straight.el](https://github.com/raxod502/straight.el):

```elisp
(use-package gcalcli-mode
  :straight (gcalcli-mode :type git :host github :repo "rlister/gcalcli-mode"))
```

If `gcalcli` is not in your `exec-path` you may override with:

```elisp
(setq gcalcli-bin "path/to/gcalcli")
```

Now run `M-x gcalcli-agenda` and you should see an agenda view.

## Keybindings

`gcalcli-mode` inherits from `special-mode` and uses the standard keys
from that mode, plus the following additions:

| Key   | Function                            |
|:------|:------------------------------------|
| `g`   | Call `gcalcli` again to update view |
| `q`   | Quit window and bury buffer         |
| `n`   | Next line                           |
| `p`   | Previous line                       |
| `l`   | Toggle event locations on/off       |
| `f`   | Move forward a week                 |
| `b`   | Move backward a week                |
| `t`   | Jump to week showing today          |
| `a`   | Add an event                        |
| `e`   | Edit events by search string        |
| `C-k` | Delete events by search string      |

## Multiple google accounts

If you have multiple google logins, create a [gcalcli config
folder](https://github.com/insanum/gcalcli#configuration-folders) for
each account. This will contain oauth and cache (and optionally
`gcalclirc`) for each account.

Then set the following alist to point to the folders. If you have more
than one entry, `gcalcli-agenda` will query for which to use. You can
create multiple agenda buffers for multiple folders.

```elisp
(setq gcalcli-config-alist
  '((home . "~/.config/gcalcli/home")
    (work . "~/.config/gcalcli/work")))
```

## Future plans

- delete and edit using current line text
- arbitrary agenda spans
