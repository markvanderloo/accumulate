
expect_equal(n_complete(c(1,NA,2), 1:3), 2)
expect_equal(fraction_complete(c(1,NA,2), 1:3), 2/3)
expect_equal(n_incomplete(c(1,NA,2), 1:3), 1)
expect_equal(fraction_incomplete(c(1,NA,2), 1:3), 1/3)

expect_error(accumulate:::stopf("foo %g",bar))
expect_equal(accumulate:::last(1:3),3)

