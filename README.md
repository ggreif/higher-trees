# higher-trees
Haskell library for higher dimensional trees

[![Build Status CircleCI](https://circleci.com/gh/ggreif/higher-trees.svg?&style=shield)](https://circleci.com/gh/ggreif/higher-trees)

## Courtesy of [MuniHac2016](http://munihac.de/)

Image credit: http://opetopic.net
[![A 3-dimensional tree](http://opetopic.net/assets/svgs/3dboxtree.svg)](http://opetopic.net)

# Background

Higher-dimensional trees are interesting objects of study in
higher-categorical calculi. As far as I know @ericfinster came up with
this formulation, and he is developing a prover for diagrammatic
reasoning. He utilizes *opetopes* as the primitives (words) of his
calculus.

Higher dimensional trees are *almost* opetopes. They must have exactly
one cell in codimension 0 to qualify as such.
