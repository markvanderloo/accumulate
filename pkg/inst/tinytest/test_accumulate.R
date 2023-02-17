
#source("../../R/rcollapse.R")

## Accumulate with collapsing sequence as data frame
input    <- data.frame(Y1 = 2^(0:8), Y2 = 2^(0:8))
input$Y2[c(1,4,7)] <- NA

input$A0 <- c(123,123,123,135,136,137,212,213,225)


collapse <- data.frame(
      A0   = c(123, 135, 136, 137, 212, 213, 225)
    , A1   = c(12 , 13 , 13 , 13 , 21 , 21 , 22 )
    , A2   = c(1  , 1  , 1  , 1  , 2  , 2  , 2  )
)

out <- accumulate(input
         , collapse
         , test = function(d) nrow(d)>=3
         , fun  = sum, na.rm=TRUE)

expect_equal(out[,1], unique(collapse[,1]))
expect_equal(out[,2], c(0, 1 ,  1,  1,   2,   2, 2))
expect_equal(out[,3], c(7, 56, 56, 56, 448, 448, 448))
expect_equal(out[,4], c(6, 48, 48, 48, 384, 384, 384))

# With NA in result (case where no subset passes test())
out <- accumulate(input, collapse
          , test=function(d) nrow(d) >= 10
          , fun = sum, na.rm=TRUE)

expect_equal(out[,3], rep(NA,7))
expect_equal(out[,4], rep(NA,7))

 

## Accumulate with collapsing scheme as formula
input <- data.frame(
    A  = c(1,1,1,2,2,2,3,3,3)
  , B  = c(11,11,11,12,12,13,21,22,12)
  , B1 = c(1,1,1,1,1,1,2,2,1)
  , Y1 = 2^(0:8)
  , Y2 = 2^(0:8)
)

input$Y2[c(1,4,7)] <- NA

out <- accumulate(input
  , collapse = A*B ~ A*B1 + A
  , test=function(a) nrow(a)>=3
  , fun = sum, na.rm=TRUE)


expect_equal( out[,1], c( 1,  2,  2,   3,   3,   3) )
expect_equal( out[,2], c(11, 12, 13,  21,  22,  12) )
expect_equal( out[,3], c( 0,  1,  1,   2,   2,   2) )
expect_equal( out[,4], c( 7, 56, 56, 448, 448, 448) )
expect_equal( out[,5], c( 6, 48, 48, 384, 384, 384) )


d <- data.frame(A0 = as.character(c(11,11,12)), Y = c(4,8,6))
A <- data.frame(A0 = as.character(c(11,12)), A1 = as.character(c(1,1)))

out <- cumulate(data = d
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

out <- cumulate(data=input
        , collapse=A*B ~ A*B1 + A
        , test=function(d) nrow(d)>=3
        , tY = sum(Y) )

expect_equivalent(out, output)

# Case where the aggregate is an object (not a scalar)

out <- cumulate(data=input
        , collapse = A*B ~ A*B1 + A
        , test = function(d) nrow(d) >= 3
        , model = lm(Y ~ 1)
        , mean   = mean(Y)
       )

expect_equivalent(sapply(out$model, coef), out$mean)





