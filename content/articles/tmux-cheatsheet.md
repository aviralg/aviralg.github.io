+++
title = "tmux Cheatsheet"
author = ["aviral-goel"]
date = 2019-11-03T00:00:00-04:00
lastmod = 2019-11-03T17:18:03-05:00
tags = ["tmux"]
categories = ["Cheatsheet"]
draft = false
[menu.main]
  identifier = "tmux-cheatsheet"
+++

## Tmux {#tmux}

[tmux](http://tmux.github.io/ ) is a terminal multiplexer. It enables you to share the same screen among multiple terminal programs.
A single [tmux](http://tmux.github.io/ ) session lets you run multiple programs in separate windows (tabs). These windows can be split horizontally or vertically into panes.
[tmux](http://tmux.github.io/ ) is flexible. You can detach from a [tmux](http://tmux.github.io/ ) session to reconnect at a later point while it continues running the programs in the background.
You can create multiple [tmux](http://tmux.github.io/ ) sessions on the same machine and connect to them simultaneously from different terminal sessions. It is a _modern_ alternative to [GNU Screen](https://www.gnu.org/software/screen/).


## Commands {#commands}


### Command Line Interface {#command-line-interface}

| Description                                    | Command                                        | Shortcut                               |
|------------------------------------------------|------------------------------------------------|----------------------------------------|
| Create a named tmux session                    | new-session -s <session-name>                  | new -s <session-name>                  |
| Create a new named session with a named window | new-session -s <session-name> -n <window-name> | new -s <session-name> -n <window-name> |
| List tmux sessions                             | list-sessions​                                  | ls                                     |
| Attach to a session                            | attach -t <session-name>                       |                                        |
| Kill a session                                 | kil-session -t <session-name>                  |                                        |


### Session Management {#session-management}

| Description              | Command | Shortcut     |
|--------------------------|---------|--------------|
| Detach from tmux session |         | `Prefix` `d` |


### Window Management {#window-management}

| Description             | Command                                            | Shortcut       |
|-------------------------|----------------------------------------------------|----------------|
| Create window           | new-window -n <window-name> ["<application-name>"] | `Prefix` `c`   |
| Rename window           |                                                    | `Prefix` `,`   |
| Move to next window     |                                                    | `Prefix` `n`   |
| Move to previous window |                                                    | `Prefix` `p`   |
| Move to nth window      |                                                    | `Prefix` `<n>` |
| Show window menu        |                                                    | `Prefix` `w`   |
| Find window             |                                                    | `Prefix` `f`   |
| Close window            |                                                    | `Prefix` `&`   |
| Find window             |                                                    | `Prefix` `f`   |


### Pane Management {#pane-management}

| Description                | Command | Shortcut            |
|----------------------------|---------|---------------------|
| Divide Window Vertically   |         | `Prefix` `%`        |
| Divide Window Horizontally |         | `Prefix` `"`        |
| Cycle throw panes          |         | `Prefix` `o`        |
| Move to upper pane         |         | `Prefix` `Up`       |
| Move to lower pane         |         | `Prefix` `Down`     |
| Move to right pane         |         | `Prefix` `Right`    |
| Move to left pane          |         | `Prefix` `Left`     |
| Cycle through pane layouts |         | `Prefix` `Spacebar` |


### Miscellaneous {#miscellaneous}

| Description                      | Command | Shortcut     |
|----------------------------------|---------|--------------|
| Show clock                       |         | `Prefix` `b` |
| Enter command mode               |         | `Prefix` `:` |
| List predefined tmux keybindings |         | `Prefix` `?` |


## Pane Layouts {#pane-layouts}

`tmux` provides the following default pane layouts:

-   _even-horizontal_: stack panes horizontally
-   _even-vertical_: stack panes vertically
-   _main-horizontal_: single large pane on top and remaining panes arranged left-to-right below it
-   _main-vertical_: single large pane on left and remaining panes arranged top-to-bottom beside it
-   _tiled_: arrange panes evenly
