
[![CRAN](http://www.r-pkg.org/badges/version/accumulate)](http://cran.r-project.org/package=accumulate/)
[![Downloads](https://cranlogs.r-pkg.org/badges/accumulate)](http://cran.r-project.org/package=accumulate/)
[![status](https://tinyverse.netlify.app/badge/accumulate)](https://CRAN.R-project.org/package=accumulate)

# accumulate

Split-apply-combine aggregation with dynamic grouping. 

The packages implements grouped aggregation, but rather than having static
groups like in `stats::aggregate()` or `dplyr::group_by()` it is possible to
change the grouping according to a user-defined scheme. For example, one may
demand that groups contain at least _n_ records, and collapse certain groups
together if this is not the case.


## Installing

The latest CRAN release can be installed as usual
```r
install.packages("accumulate")
```

The git version can be installed by cloning the repo and using `make`.

```bash
git clone https://github.com/markvanderloo/accumulate
cd accumulate
make install
```

No guarantees that it will actually build, install, or give correct results.
(This is after all the place where development takes place).


## Example

See [the introductory vignette](pkg/vignettes/introduction.md).


## Licence

This software is released under [EUPL](https://commission.europa.eu/content/european-union-public-licence_en).


