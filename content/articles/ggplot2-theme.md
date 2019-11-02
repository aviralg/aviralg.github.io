+++
title = "ggplot2 Theme"
author = ["aviral-goel"]
date = 2019-11-02T00:00:00-04:00
lastmod = 2019-11-02T11:43:08-04:00
tags = ["R", "graphs", "charts", "plots"]
categories = ["ggplot2"]
draft = false
[menu.main]
  identifier = "ggplot2-theme"
+++

I use [ggplot2](https://ggplot2.tidyverse.org) nowadays for all my graphing requirements because my research is on improving [R](https://www.r-project.org/) and alternatives such as [matplotlib](https://matplotlib.org/) are abysmal. This article is a collection of theme settings I have found useful, especially for generating figures for my [recent publication on R](http://aviral.io/publications/on-the-design-implementation-and-use-of-laziness-in-r.pdf).

For generating colorless background figures, I use the following theme setting.

```R
old_theme <- theme_set(theme_bw() +
                       theme(text = element_text(size = 15), panel.border = element_blank()))
```

This is how the figures look like before and after updating the theme.

<a id="orgd5774a6"></a>

{{< figure src="/images/ggplot2/without-theme.png" caption="Figure 1: Figure without updated theme" width="750px" >}}

<a id="org3ee6670"></a>

{{< figure src="/images/ggplot2/with-theme.png" caption="Figure 2: Figure with updated theme" width="750px" >}}

These figures were generated from the following code snippet.

```R
library(ggplot2)

ggplot(mpg, aes(class)) +
geom_bar()
```
