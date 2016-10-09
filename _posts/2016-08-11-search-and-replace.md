---
layout: post
title: Search and Replace
categories:
- notes
- cheatsheet
tags:
- emacs
---

## Incremental/Non - Incremental Search

| Shortcut     |            Function              |        Description                |
|:------------:|:--------------------------------:|:----------------------------------|
|**C - s**     | `isearch-forward`                | Forward incremental search        |
|**C - s RET** | `search-forward`                 | Forward non incremental search    |
|**C - r**     | `isearch-backward`               | Forward incremental search        |
|**C - r RET** | `search-backward`                | Forward non incremental search    |

While in the middle of search, the following keys have special meaning -

| Key          |        Description                |
|:------------:|:----------------------------------|
|**BACKSPACE** | Erase last character typed        |
|**RETURN**    | Terminate the search and leave point at the current position in the buffer |
|**C - s**     | Search forward for same pattern   |
|**C - r**     | Search backward for same pattern  |
|**C - g**     | While search is in progress: Stop current search. This will leave you where you started as if nothing had happened |
|**C - g**     | While waiting for input: abort entire command |
|**C - w**     | Copy the word after point to search string |
|**C - y**     | Copy current kill ring entry to search string |
|**M - y**     | Copy previous kill ring entry to search string |

*TODO* - Page 160 onwards


## Word Search

Word search tells emacs to search for only complete words. Its a variation of incremental search.Word search ignores all punctuation, tabs, spaces and end of lines. Thus, one can look for a series of words that span more than one line.


| Shortcut |            Function              |        Description                |
|:----------------:|:--------------------------------:|:----------------------------------|
|**M - s w**       | `isearch-forward-word`           | Forward incremental word search   |
|**M - s w C - r** | `isearch-backward-word`          | Backward incremental word search  |


## Regular Expression Search




## References

[GNU Emacs Manual:Moving Point](https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html#Moving-Point)
