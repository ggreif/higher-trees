# higher-trees
Haskell library for higher dimensional trees

[![Build Status CircleCI](https://circleci.com/gh/ggreif/higher-trees.svg?&style=shield)](https://circleci.com/gh/ggreif/higher-trees)

## Courtesy of [MuniHac2016](http://munihac.de/)

Image credit: http://opetopic.net
[![A 3-dimensional tree](http://opetopic.net/assets/svgs/3dboxtree.svg)](http://opetopic.net)

# Disclaimer

This is not *yet* a library. Hopefully it will become one someday.
Right now it is more of a playground for testing techniques how to deal
with such objects in GHC. See e.g. the `singleton` branch for interesting
type-level stuff (rather clumsy, I know). Also I have provoked GHC panics,
so if you want to examine them, check out the `panic-*` branches.

# Background

Higher-dimensional trees are interesting objects of study in
higher-categorical calculi. As far as I know @ericfinster came up with
this formulation, and he is developing a prover for diagrammatic
reasoning. He utilizes *opetopes* as the primitives (words) of his
calculus.

Higher dimensional trees are *almost* opetopes. They must have exactly
one cell in codimension 0 to qualify as such.

# Decorating

An n-dimensional tree can be *decorated* to obtain an
(n+1)-dimensional tree.

## Pointing

Point at an non-empty set {b0 .. bn} of boxes. These can be thought of
as a *monoid*: they determine the smallest subtree that encompasses
all those boxes.

The closure property is the following: if neither is
a predecessor of the other, then the monoid operation includes the
highest common predecessor (dominator) an all the boxes in-between.

What we get is a semi-lattice, a commutative idempotent monoid. Unit
is the empty pointed set.

## Sticking cards

When we have such a subtree, we can stick a card below it. For the
next round this becomes another box we can `mappend` to. All
encompassed boxes of the card are removed from the pointable set
(i.e. become `mempty`). They are still accessible for *sprouting* as
that process creates new boxes *outside of the card*.

The cards encompass an n-dimensional branching structure, thus giving
rise to an (n+1)-dimensional tree (a *branch* of it). In fact the card
*cuts out* a subtree of the n-dimensional original tree.

By collapsing the card to a box, the original tree gives rise to a new
n-dimensional tree that is (possibly) smaller. This process can be
considered an *inverse substitution*.

## Zippers

The term *pointing* is made precise by the data structure of the
*zipper*. In this case it is a path from the root leading to the box
we point at.
