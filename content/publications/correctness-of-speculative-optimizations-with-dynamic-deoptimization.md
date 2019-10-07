+++
title = "Correctness of Speculative Optimizations with Dynamic Deoptimization"
author = ["Olivier Flückiger", "Gabriel Scherer", "Ming-Ho Yee", "Aviral Goel", "Amal Ahmed", "Jan Vitek"]
date = 2019-10-06T00:00:00-04:00
lastmod = 2019-10-06T23:00:48-04:00
tags = ["speculative-optimization", "dynamic-deoptimization", "on-stack-replacement"]
categories = ["popl"]
draft = false
conference_name = "POPL, Jan 2018"
conference_url = "https://popl18.sigplan.org/"
pdf = "correctness-of-speculative-optimizations-with-dynamic-deoptimization.pdf"
arxiv = "https://arxiv.org/abs/1711.03050"
[menu.main]
  identifier = "correctness-of-speculative-optimizations-with-dynamic-deoptimization"
+++

High-performance dynamic language implementations make heavy use of speculative optimizations to achieve speeds close to statically compiled languages. These optimizations are typically performed by a just-in-time compiler that generates code under a set of assumptions about the state of the program and its environment. In certain cases, a program may execute code compiled under assumptions that are no longer valid. The implementation must then deoptimize the program on-the-!y; this entails "nding semantically equivalent code that does not rely on invalid assumptions, translating program state to that expected by the target code, and transferring control. This paper looks at the interaction between optimization and deoptimization, and shows that reasoning about speculation is surprisingly easy when assumptions are made explicit in the program representation. This insight is demonstrated on a compiler intermediate representation, named `sourir`, modeled after the high-level representation for a dynamic language. Traditional compiler optimizations such as constant folding, unreachable code elimination, and function inlining are shown to be correct in the presence of assumptions. Furthermore, the paper establishes the correctness of compiler transformations specific to deoptimization: namely unrestricted deoptimization, predicate hoisting, and assume composition.
