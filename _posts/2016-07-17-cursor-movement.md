---
layout: post
title: Cursor Movement
categories:
- notes
tags:
- emacs
---

## Cheatsheet

| Shortcut |            Function              |        Description                |
|:--------:|:--------------------------------:|:----------------------------------|
|**C - f** | `forward-char`                   | Move forward one character        |
|**C - b** | `backward-char`                  | Move backward one character       |
|**C - n** | `next-line`                      | Move down one screen line         |
|**C - p** | `previous-line`                  | Move up one screen line           |
|**C - a** | `move-beginning-of-line`         | Move to the beginning of the line |
|**C - e** | `move-end-of-line`               | Move to the end of the line       |
|**M - f** | `forward-word`                   | Move forward one word             |
|**M - b** | `backward-word`                  | Move backward one word            |
|**M - r** | `move-to-window-line-top-bottom` | Without moving the text on the screen, reposition point on the left margin of the center-most text line of the window; on subsequent consecutive invocations, move point to the left margin of the top-most line, the bottom-most line, and so forth, in cyclic order |
|**M - <** | `beginning-of-buffer`            | Move to the top of the buffer     |
|**M - >** | `end-of-buffer`                  | Move to the end of the buffer     |
|**C - v** | `scroll-up-command` | Scroll one screen forward, and move point onscreen if necessary |
|**M - v** | `scroll-down-command` | Scroll one screen backward, and move point onscreen if necessary |
|**M - g c** | | Read a number `n` and move point to buffer position `n` |
|**M - g g**| `goto-line`| Read a number `n` and move point to the beginning of line number `n` |
|**M-g Tab**| | Read a number `n` and move to column `n` in the current line. |


## References

[GNU Emacs Manual:Moving Point](https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html#Moving-Point)
