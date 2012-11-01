An implementation of interactive realizers for classical arithmetic without nested quantifiers
==============================================================================================

Design of a ML-like language (with bounded recursion) and implementation of an interpreter for said language in Scheme. Core algorithm developed both in Scheme and in OCaml for comparison and future integration with the Isabelle proof assistant.

Documentation
-------------

For an in-depth explanation of what this does and why see the author's [Master's thesis](http://www.di.unito.it/~rispoli/theses/thesis2009.pdf).
Chapter 4, especially, describes how every part of the system works.

Usage
-----

To try one of the several examples included you should do something like:

    $ mzscheme
    Welcome to MzScheme v4.1.5 [3m], Copyright (c) 2004-2009 PLT Scheme Inc.
    > (include "toplevel.scm")
    > (find-solution "minimum.dsl" 'F)
    valid solution in x = 3
    > final-state
    ((#<procedure:P> 1 2) (#<procedure:P> 2 3))
    > (to-gnuplot '(f (compose f g) (compose f h)))
