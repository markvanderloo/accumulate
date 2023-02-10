#source("../../R/acc.R")
#library(tinytest)
# 
#
input    <- data.frame(Y1 = 2^(0:8), Y2 = 2^(0:8))
input$Y2[c(1,4,7)] <- NA
input$A0 <- c(123,123,123,135,136,137,212,213,225)


collapse <- data.frame(
      A0   = c(123, 135, 136, 137, 212, 213, 225)
    , A1   = c(12 , 13 , 13 , 13 , 21 , 21 , 22 )
    , A2   = c(1  , 1  , 1  , 1  , 2  , 2  , 2  )
)

pullback <- accumulate:::get_pb(collapse, input)

expect_equal(pullback(123, 0), input[1:3,])
expect_equal(pullback(135, 1), input[4:6,])
expect_equal(pullback(212, 2), input[7:9,])


input <- data.frame(
    A  = c(1,1,1,2,2,2,3,3,3)
  , B  = c(11,11,11,12,12,13,21,22,12)
  , B1 = c(1,1,1,1,1,1,2,2,1)
  , Y1 = 2^(0:8)
  , Y2 = 2^(0:8)
)

input$Y2[c(1,4,7)] <- NA

pullback <- accumulate:::get_pb(A*B ~ A*B1 + A, input)

expect_equal(pullback(data.frame(A=1,B=11), level=0), input[1:3,])
expect_equal(pullback(data.frame(A=3,B=21), level=1), input[7:8,])
expect_equal(pullback(data.frame(A=2,B=13), level=2), input[4:6,])








