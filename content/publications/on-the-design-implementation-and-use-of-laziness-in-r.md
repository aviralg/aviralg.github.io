+++
title = "On the Design, Implementation, and Use of Laziness in R"
author = ["Aviral Goel", "Jan Vitek"]
date = 2019-10-06T00:00:00-04:00
lastmod = 2019-10-06T23:12:58-04:00
tags = ["R", "laziness", "meta-programming"]
categories = ["oopsla"]
draft = false
conference_name = "OOPSLA, Oct 2019"
conference_url = "https://2019.splashcon.org/track/splash-2019-oopsla"
pdf = "on-the-design-implementation-and-use-of-laziness-in-r.pdf"
arxiv = "https://arxiv.org/abs/1909.08958"
[menu.main]
  identifier = "on-the-design-implementation-and-use-of-laziness-in-r"
+++

The R programming language has been lazy for over twenty-five years. This paper presents a review of the design and implementation of call-by-need in R, and a data-driven study of how generations of programmers have put laziness to use in their code. We analyze 16,707 packages and observe the creation of 270.9 B promises. Our data suggests that there is little supporting evidence to assert that programmers use laziness to avoid unnecessary computation or to operate over infinite data structures. For the most part R code appears to have been written without reliance on, and in many cases even knowledge of, delayed argument evaluation. The only significant exception is a small number of packages which leverage call-by-need for meta-programming.
