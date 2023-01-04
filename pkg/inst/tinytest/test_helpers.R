
expect_error(accumulate:::stopf("foo %g",bar))
expect_equal(accumulate:::last(1:3),3)


d <- data.frame(x=c(1:5,rep(NA,5)), y=1:10)
expect_equal(accumulate:::complete_cases(d)
           , c(rep(TRUE,5), rep(FALSE,5)))



expect_true( min_records(8)(d))
expect_false(min_records(12)(d))
expect_error(min_records(-1))
expect_error(min_records("foo"))

expect_true( min_complete(3)(d))
expect_false(min_complete(8)(d))
expect_true( min_complete(8,vars="y")(d))
expect_false(min_complete(8,vars="x")(d))
expect_error(min_complete(n="foo"))


expect_true( frac_complete(0.4)(d))
expect_false(frac_complete(0.8)(d))
expect_true( frac_complete(0.8, vars="y")(d))
expect_false(frac_complete(0.8, vars="x")(d))
expect_error(min_complete(r="foo"))




csh <- csh_from_digits(c("11","12","123"))
expect_equal(csh_from_digits(c("11","12"))
            , data.frame(A0=c("11","12"), A1=c("1","1")))
expect_equal(csh_from_digits(c("11","12"), levels=0)
            , data.frame(A0=c("11","12")))

expect_error(csh_from_digits(c(11,12), levels=-1))
expect_error(csh_from_digits(c(11,12), levels= 2))

#from_validator

if (!requireNamespace('validate', quietly=TRUE)) exit_file("validate not installed")

expect_true(from_validator( validate::validator(sum(is.na(x)) < 8) )(d))
expect_false(from_validator( validate::validator(sum(is.na(x)) < 4) )(d))


dat <- data.frame(x = 1:5, y=(-2):2)
expect_stdout(smoke_test(dat, function(d) y > 0), pattern="ERR")  
expect_stdout(smoke_test(dat, function(d) d$y > 0),pattern="length") 
expect_stdout(smoke_test(dat, function(d) sum(d$y > 0) > 2), pattern="NA detected")
expect_stdout(smoke_test(dat, function(d) sum(d$y > 0, na.rm=TRUE) > 2),pattern="")








