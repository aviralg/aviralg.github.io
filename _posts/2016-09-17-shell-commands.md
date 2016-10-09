---
layout: post
title: Shell Commands
categories:
- cheatsheet
tags:
- emacs
---

## Cheatsheet

| Shortcut |            Function              |        Description                                                   |
|:--------:|:--------------------------------:|:----------------------------------|
|**M - !** | `shell-command`                  | Run a shell command               |
|**M - &#124;** | `shell-command-on-region`        | Run a shell command using region as input       |
|**M - &** | `async-shell-command`            | Run a shell command asynchronously       |
|**C - u M - &#124;** | `shell-command-on-region`  | Run a shell command using region as input and replace it with output |
|**M - x shell** | `shell`                  | Start a separate shell in its own buffer           |
|**M - x term** | `term`         | Start a separate shell in its own buffer with full terminal emulation |
|---|---|---|


## References

- [GNU Emacs Manual:Shell](https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell.html)
- [Mastering Emacs:Executing Shell Commands in Emacs](https://www.masteringemacs.org/article/executing-shell-commands-emacs)
