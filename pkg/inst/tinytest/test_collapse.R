
#source("../../R/rcollapse.R")

d <- data.frame(A0 = as.character(c(11,11,12)), Y = c(4,8,6))
A <- data.frame(A0 = as.character(c(11,12)), A1 = as.character(c(1,1)))

out <- collapse(data = d
  , collapse  = A
  , test      = function(d) if (nrow(d)>=2) TRUE else FALSE
  , mn = mean(Y, na.rm=TRUE)
  , md = median(Y,na.rm=TRUE) )

expect_equal(out[,1],c("11","12"))
expect_equal(out[,2],c("11","1"))
expect_equal(out[,3],c(6,6))
expect_equal(out[,4],c(6,6))


