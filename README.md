<!-- README.md is generated from README.Rmd. Please edit that file -->
rvtable <a hef="https://github.com/leonawicz/rvtable/blob/master/data-raw/rvtable.png?raw=true" _target="blank"><img src="https://github.com/leonawicz/rvtable/blob/master/inst/rvtable.png?raw=true" style="margin-bottom:5px;" width="120" align="right"></a>
===============================================================================================================================================================================================================================================================

[![Travis-CI Build
Status](https://travis-ci.org/leonawicz/rvtable.svg?branch=master)](https://travis-ci.org/leonawicz/rvtable)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/rvtable?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/rvtable)
[![Coverage
Status](https://img.shields.io/codecov/c/github/leonawicz/rvtable/master.svg)](https://codecov.io/github/leonawicz/rvtable?branch=master)

rvtable is an R package for storing distributions of random variables in
long format data frames with the `rvtable` class. The package provides a
simplified and consistent interface for managing and manipulating random
variables. The key purpose of rvtable is to carry distributions through
an analysis from beginning to end where the distributions are
empirically derived from a large data set that is impractical or
impossible to keep in original form.

The `rvtable` package provides a special type of data frame subclass
with associated functionality for relatively convenient storage and
manipulation of random variables. The emphasis is on distributions of
continuous random variables derived from relatively large samples,
though discrete random variables are also handled.

Motivation
==========

While this package can be used for organizing small samples, there is
not much point. The main motivation for `rvtable` is relatively seamless
storage and manipulation of empirically estimated continuous probability
distributions deriving from relatively large samples or data sets.

By relatively large, I mean cases where it is both substantially more
statistically and computationally efficient to store and subsequently
work with estimated probability distributions that are derived from and
sufficiently representative of the source data than to work directly on
the source data itself.

A large data set can be sampled to make it more manageable. This package
provides an alternative by estimating the empirical probability density
function of a continuous random variable so that the model of the
distribution can be carried through analyses pipelines in place of raw
observations. This allows sampling from a distribution of a random
variable when and however necessary. This is particularly helpful
well-estimated distributions remain much more compact and efficient than
holding onto much larger amounts of data throughout a processing chain
where it is ultimately not needed.

### Use case and limitations

For example, it is sometimes useful to calculate statistics as the final
step in a sequence of intensive data processing operations. It could be
that a product to come out of the analysis is a web application where
the analyst leaves it to the user to decide interactively which
variable(s) to select and what statistics to compute, what to graph and
how, whether to exclude or merge data, etc. A R Shiny app is the perfect
example of this.

This can lead to the problem of having far too much data to put in the
application yet not being able to summarize or aggregate the data in
advance sufficiently without taking too much control away from the user.
When there is a lot of data involved, using a relatively efficient
representation of a distribution of the data rather than raw
observations or a static, relatively small sample, can help transport
the relevant information from one stage of an analysis to another,
particularly in a context of limited computing resources or a simple
need for speed, such as in an interactive environment involving other
users. At the same time, using estimated probability distributions
leaves in place the ability to draw an arbitrarily large sample later
when the time is right and context demands it.

The main goals of `rvtable` are to provide functions for storage,
manipulation, and graphing of random variables in the preceding context.
This is achieved through:

-   Consistent use of data frame structures.
-   A number of helper functions for moving between joint, marginal, and
    conditional distributions.
-   Generic functions for providing various stock visual comparisons,
    such as between related marginal and conditional distributions, for
    various objects inheriting or deriving from rvtables.

`rvtable` is focused on handling distributions of continuous random
variables derived from large samples, not small data sets or discrete
random variables with few known or observed values, but it can still
provide a consistent data structure for the context in which it is used.

It is of no benefit to known distributions with closed mathematical form
expressions because there is never a need to lug a ton of such data
around in the first place. For example, a random normal distribution can
be sampled with `rnorm` at any time. `rvtable` is helpful for empirical
samples which are large and messy, of a complicated form or mixed
distributions, which cannot be reduced to a known or simple combination
of known distributions, where an efficient snapshot of the distribution
is helpful to avoid juggling excessive amounts of data from one analysis
stage to the next while retaining sufficient distributional information.

### Details

What does an rvtable hold?

-   An rvtable may contain one discrete or continuous random variable.
-   The distribution of the random variable is described in a table
    using a values column and a probabilities column.
-   Any other columns in an rvtable are ID columns and therefore should
    only store categorical variables.
-   An rvtable is essentially a way of storing the distribution of one
    random variable in a data frame or, if ID columns are present in
    addition to the the values and probabilities columns, sections of
    rows of the table can be seen as a sequence of conditional
    distributions based on combinations of the ID variables' levels.

Functions included in the package provide the following abilities:

-   Subset specific conditional distributions from a table or
    marginalize over/integrate out ID variables to obtain a specified
    marginal distribution of the random variable
-   Draw random samples from distributions.
-   Invert conditional distributions, i.e., obtain the pmf of a ID
    variable conditional on a values of the random variable the rvtable
    is based on.

Installation and bug reporting
------------------------------

Install the latest development version (0.6.1) from github:

    install.packages("devtools")
    devtools::install_github("leonawicz/rvtable")

Please file a minimal reproducible example of any clear bug at
[github](https://github.com/leonawicz/rvtable/issues).

Learning rvtable
----------------

Familiarity with dplyr is recommended. An introduction vignette for
rvtable is [available
online](https://leonawicz.github.io/rvtable/articles/rvtable.html) or:

    browseVignettes(package = "rvtable")

License
-------

#### The MIT License (MIT)

Copyright Â© 2016-2017 Scenarios Network for Alaska and Arctic Planning
and others.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
