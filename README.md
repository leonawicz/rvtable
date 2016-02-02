<!-- README.md is generated from README.Rmd. Please edit that file -->
rvtable
=======

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/rvtable.svg?branch=master)](https://travis-ci.org/leonawicz/rvtable)

rvtable is an R package for storing distributions of random variables in long format data tables with the 'rvtable' class. The package provides a simplified and consistent interface for managing and manipulating random variables.

The main goals of rvtable are to provide functions for storage, manipulation, and graphing of random variables. This is achieved through:

-   Consistent use of data table structures
-   A number of helper functions for moving between joint, marginal, and conditional distributions
-   Generic functions for providing various stock visual comparisons, such as between related marginal and conditional distributions, for data table-type objects inheriting or deriving from rvtable.

What does an RV table hold?

-   An RV table may contain one discrete or continuous random variable.
-   The distribution of the random variable is described in a table using a `Val` column for values and a `Prob` column for associated probabilities.
-   Any other columns in an RV table are ID columns and therefore should only store categorical variables.
-   An RV table is essentially a way of storing the distribution of one random variable in a table or, if ID columns are present in addition to the `Val` and `Prob` columns, sections of rows of the table can be seen as a sequence of conditional distributions based on combinations of the ID variables' levels.

Functions included in the package provide the following abiities:

-   Subset specific conditional distributions from a table
-   Marginalize over/integrate out ID variables to obtain a specified marginal distribution of the random variable
-   Draw random samples from distributions.
-   Invert conditional distributions, i.e., obtain the pmf of a ID variable conditional on a discrete value or range of continuous values of the random variable the table is based on.

Install the latest development version from github with

    ```R
    install.packages("devtools")
    devtools::install_github("leonawicz/rvtable")
    ```

Please file a minimal reproducible example of any clear bug at [github](https://github.com/leonawicz/rvtable/issues).

Learning rvtable
----------------

Vignettes for rvtable are not yet available. Familiarity with data.table and dplyr is recommended.
