# objects of type 'object_list' are typed lists that only differ from ordenary
# lists in the way that they are printed. Also, selecting a subset with `[`
# returns an 'object_list'. 


# trivial check on constructor
expect_inherits(object_list(1:3),"object_list")

# check subsetting method
expect_inherits(object_list(1:3)[1], "object_list")


# only the type of each element is printed
expect_true(grepl("^<integer>$", format(object_list(1L))))
expect_stdout(print(object_list(1L)), "<integer>")





