+++
title = "On the Design, Implementation, and Use of Laziness in R"
author = ["aviral-goel", "jan-vitek"]
date = 2019-10-06T00:00:00-04:00
lastmod = 2019-11-02T12:50:21-04:00
tags = ["R", "Laziness", "Meta-programming"]
categories = ["OOPSLA"]
draft = false
event = "oopsla"
doi = "10.1145/3360579"
arxiv = "1909.08958/"
artifact = "https://zenodo.org/record/3369573#.XaC2c-aYVhE"
talk = "https://youtu.be/qLxz9HPP6wI"
[menu.main]
  identifier = "on-the-design-implementation-and-use-of-laziness-in-r"
+++

The R programming language has been lazy for over twenty-five years. This paper presents a review of the design and implementation of call-by-need in R, and a data-driven study of how generations of programmers have put laziness to use in their code. We analyze 16,707 packages and observe the creation of 270.9 B promises. Our data suggests that there is little supporting evidence to assert that programmers use laziness to avoid unnecessary computation or to operate over infinite data structures. For the most part R code appears to have been written without reliance on, and in many cases even knowledge of, delayed argument evaluation. The only significant exception is a small number of packages which leverage call-by-need for meta-programming.
