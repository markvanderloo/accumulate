
#source("../../R/rcollapse.R")

d <- data.frame(A0 = as.character(c(11,11,12)), Y = c(4,8,6))
A <- data.frame(A0 = as.character(c(11,12)), A1 = as.character(c(1,1)))

out <- collapse(data = d
  , collapse  = A
  , test      = function(d) if (nrow(d)>=2) TRUE else FALSE
  , mn = mean(Y, na.rm=TRUE)
  , md = median(Y,na.rm=TRUE) )

expect_equal(out[,1],c("11","12"))
expect_equal(out[,2],c(0,1))
expect_equal(out[,3],c(6,6))
expect_equal(out[,4],c(6,6))

input <- data.frame(
    A  = c(1,1,1,2,2,2,3,3,3)
  , B  = c(11,11,11,12,12,13,21,22,12)
  , B1 = c(1,1,1,1,1,1,2,2,1)
  , Y  = 2^(0:8)
)

output <- data.frame(
    A = c(1,2,2,3,3,3)
  , B = c(11,12,13,21,22,12)
  , level = c(0,1,1,2,2,2)
  , tY    = c(7,56,56,448,448,448)
)

out <- collapse(data=input
        , collapse=A*B ~ A*B1 + A
        , test=function(d) nrow(d)>=3
        , tY = sum(Y) )

expect_equivalent(out, output)

