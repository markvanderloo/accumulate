<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Introduction to accumulate}
-->

---
title: Introduction to `accumulate`
author: Mark P.J. van der Loo
css: "style.css"
---

Package version `packageVersion("accumulate")`{.R}. 

Use `citation('accumulate')` to cite the package.

## Introduction

`Accumulate` is a package for grouped aggregation, where the groups can be
dynamically collapsed into larger groups. When this collapsing takes place and
how collapsing takes place is user-defined.


## Installation

The latest CRAN release can be installed as follows.
```
install.packages("accumulate")
```
Next, the package can be loaded. You can use `packageVersion` (from base R) to
check which version you have installed.
```{#load_package .R}
library(accumulate)
# check the package version
packageVersion("accumulate")
```

## A first example

We will use a built-in dataset as example. 
```{#loading_data .R}
data(producers)
head(producers)
```
This synthetic dataset contains information on various sources of turnover from
producers, that are labeled with an economic activity classification (`sbi`)
and a `size` class (0-9). 

We wish to find a group mean by `sbi x size`. However, we demand that the group has
at least five records, otherwise we combine the size classes of a single `sbi` group.
This can be done as follows.
```{#first_example .R}
a <- accumulate(producers
              , collapse = sbi*size ~ sbi
              , test = min_records(5)
              , fun  = mean, na.rm=TRUE)
head(round(a))
```
The accumulate function does the following:

- For each combination of `sbi` and `size` occurring in the data, it checks whether
  `test` is satisfied. Here, it tests whether there are at least five records.
    - If the test is satisfied, the mean is computed for each non-grouping variable
      in the data. The output column `level` is set to 0 (no collapsing took place).
    - If the test is _not_ satisfied, it will only use `sbi` as grouping variable
      for the current combination of `sbi` and `size`. Then, if there are enough
      records, the mean is computed for each variable and the output variable `level`
      is set to 1 (first level of collapsing has been used). 
    - If the test is still not satisfied, no computation is possible
      and all outputs are `NA` for the current `sbi` and `size` combination.
 
Explicitly, for this example we see that for `(sbi,size)==(2752,5)` no
satisfactory group of records was found under the current collapsing scheme.
Therefore the `level` variable equals `NA` and all aggregated variables are
missing as well.  For `(sbi,size)==(2840,7)` there are sufficient records, and
since `level=0` no collapsing was necessary. For the group
`(sbi,size)=(3410,8)` there were not enough records to compute a mean, but
taking all records in `sbi==3410` gave enough records. This is signified by
`level=1`, meaning that one collapsing step has taken place (from `sbi x size`
to `sbi`).

 
Let us see how we specified this call to `accumulate`

- The first argument is the data to be aggregated.
- The second argument is a formula of the form `target groups ~ collapsing scheme`.
  The output is always at the level of the target groups. The collapsing scheme determines
  which records are used to compute a value for the target groups if the `test` is not
  satisfied.
- The third argument, called `test` is a function that should accept any subset of 
  records of `producers` and return `TRUE` or `FALSE`. In this case we used the convenience
  function `min_records(5)` provided by `accumulate`. The function `min_records()` creates
  a testing function for us that we can pass as testing function.
- Finally, the argument `fun` is the aggregation function that will be applied to each
  group. 

Observe that the accumulate function is similar to R's built-in `aggregate` function (this is
by design). There is a second function called `cumulate` that has an interface that
is similar to `dplyr::summarise`.

```{#cumulate_formula .R}
a <- cumulate(producers, collapse = sbi*size ~ sbi
      , test = function(d) nrow(d) >= 5
      , mu_industrial = mean(industrial, na.rm=TRUE)
      , sd_industrial = sd(industrial, na.rm=TRUE))

head(round(a))
```

Notice that here, we wrote our own test function.


### Exercises

1. How many combinations of `(sbi, size)` could not be computed, even when 
   collapsing to `sbi`? (You need to run the code and investigate the output).
2. Compute the trimmed mean of all numeric variables where you trim
   5% of each side the distribution. See `?mean` on how to compute trimmed
   means.

## The formula interface for specifying collapsing schemes

A collapsing scheme can be defined in a data frame or with a
formula of the form 
```
target grouping ~ collapse1 + collapse2 + ... + collapseN
```
Here, the `target grouping` is a variable or product of variables.  Each
`collapse` term is also a variable or product of variables. Each subsequent
term defines the next collapsing step. Let us show the idea with a 
more involved example.

The `sbi` variable in the `producers` dataset encodes a hierarchical classification
where longer digit sequences indicate higher level of detail. Hence we can collapse
to lower levels of detail by deleting digits at the end. Let us enrich the
`producers` dataset with extra grouping levels.

```{#derive_sbi_levels .R}
producers$sbi3 <- substr(producers$sbi,1,3)
producers$sbi2 <- substr(producers$sbi,1,2)
head(producers,3)
```

We can now use a more involved collapsing scheme as follows.
```{#accumulate_formula .R}
a <- accumulate(producers, collapse = sbi*size ~ sbi + sbi3 + sbi2
               , test = min_records(5), fun = mean, na.rm=TRUE)
head(round(a))
```
For `(sbi,size) == (2752,5)` we have 2 levels of collapsing. In other
words, for that aggregate, all records in `sbi3 == 275` were used.

### Exercises

1. Compute standard deviation for `trade` and `total` using the `cumulate` function
   under the same collapsing scheme as defined above.
2. What is the maximum collapsing level in the collapsing scheme above?
3. Find out how many combinations of `(sbi,size)` have been collapsed to 
   level 0, 1, 2, or 3. Tabulate them.
4. Define a collapsing scheme that ends with a single-digit `sbi` code and compute
   the means of all variables.


## The data frame interface for defining collapsing schemes

Collapsing schemes can be represented in data frames that have the
form

```
[target group, parent of target group, parent of parent of target group,...].
```
The package comes with a helper function that creates such a scheme
from hierarchical classifications that are encoded as digits.

For the `sbi` example we can do the following to derive a collapsing scheme.
```{#dataframe_construction .R}
sbi <- unique(producers$sbi)
csh <- csh_from_digits(sbi)
names(csh)[1] <- "sbi"
head(csh)
```
Here, the column `sbi` denotes the original (maximally) 5-digit codes,
`A1` the 4-digit codes, and so on. It is important that the name of
the first column matches a column in the data to be aggregated.
Both `cumlate` and `accumulate` accept such a data frame as an argument.
Here is an example with `cumulate`.

```{#dataframe_cumulate .R}
a <- cumulate(producers, collapse = csh, test = function(d) nrow(d) >= 5
       , mu_total = mean(total, na.rm=TRUE)
       , sd_total = sd(total, na.rm=TRUE))
head(a)
```

In this representation is is not possible to use multiple grouping
variables, unless you combine multiple grouping variables into a single
one, for example by pasting them together.

The advantage of this representation is that it allows users to externally
define a (manually edited) collapsing scheme.

### Exercises

1. Use `csh` to compute the median of all numerical variables of
   the `producers` dataset with `accumulate` (hint: you need to remove
   the `size` variable).


## Convenience functions to define tests

There are several options to define test on groups of records:

1. Use one of the built-in functions to specify common test conditions:
   `min_records()`, `min_complete()`, or `frac_complete()`.
2. Use a ruleset defined with the [validate](https://cran.r-project.org/package=validate)
   package, with the `from_validator()` function.
3. Write your own custom test function. 


Let us look at a small example for each case. For comparison we will
always test that there are a minimum of five records.


```{#helpers .R}
# load the data again to loose columns 'sbi2' and 'sbi3' and work
# with the original data.
data(producers)

# 1. using a helper function
a <- accumulate(producers, collapse = sbi*size ~ sbi
               , test = min_records(5)
               , fun  = mean, na.rm=TRUE)

# 2. using a 'validator' object
rules <- validate::validator(nrow(.) >= 5)
a <- accumulate(producers, collapse = sbi*size ~ sbi
               , test = from_validator(rules)
               , fun  = mean, na.rm=TRUE)

# 3. using a custom function
a <- accumulate(producers, collapse=sbi*size ~ sbi
               , test = function(d) nrow(d) >= 5
               , fun  = mean, na.rm=TRUE)
```

## Complex aggregates

An aggregate may be something more complex than a scalar. The `accumulate`
package also supports complex aggregates such as linear models.

```{#complex .R}
a <- cumulate(producers, collapse = sbi*size ~ sbi
                       , test = min_complete(5, c("other_income","trade"))
                       , model = lm(other_income ~ trade)
                       , mean_other = mean(other_income, na.rm=TRUE))

head(a)
```
Here, we demand that there are at least five records available for estimating 
the model.

The linear models are stored in a `list` of type `object_list`. Subsets or individual
elements can be accessed as usual with data frames.
```{#objlist .R}
a$model[[1]]
a$model[[2]]
```





### Smoke-testing your test function

If you write your own test function from scratch, it is easy to overlook some
edge cases like the occurrence of missing data, a column that is completely
`NA`, or receiving zero records. The function `smoke_test()` accepts a data set
and a test function and runs the test function on several common edge cases
based on the dataset. It does _not_ check whether the test function works as
expected, but it checks that the output is `TRUE` or `FALSE` in all cases and
reports errors, warnings and mesages if they occur.


As an example we construct a test function that checks whether one
of the variables has sufficient non-zero values.
```{#smoketest1 .R}
my_test <- function(d) sum(other != 0) > 3
smoke_test(producers, my_test)
```
Oops, we forgot to refer to the data set. Let's try it again.
```{#smoketest2 .R}
my_test <- function(d) sum(d$other != 0) > 3
smoke_test(producers, my_test)
```
Our function is not robust against occurrence of `NA`. Here's a third attempt.
```{#smoketest3 .R}
my_test <- function(d) sum(d$other != 0,na.rm=TRUE) > 3
smoke_test(producers, my_test)
```



### Exercises

1. Compute the mean of all variables using `sbi*size ~ sbi1 + sbi2` as collapsing
   scheme. Make sure there are at least 10 records in each group.
2. Compute the mean of the ratio between `industrial` and `total`, but demand
   that there are not more than 20% zeros in `other`. Use `csh` as collapsing scheme.















