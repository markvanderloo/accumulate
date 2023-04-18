
expect_true(accumulate:::ok_formula(a ~ b))
expect_true(accumulate:::ok_formula(a*b ~ b))
expect_true(accumulate:::ok_formula(a*b*c ~ a*b + a))
expect_true(accumulate:::ok_formula(a*b*c*d ~ a*b*c + a*c + a + c))
expect_false(accumulate:::ok_formula(a+b ~ b))



