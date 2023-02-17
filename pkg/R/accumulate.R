#' Split-Apply-Combine with Collapsing Groups
#'
#' Compute grouped aggregates. If a group does not satisfy certain user-defined
#' conditions (such as too many missings, or not enough records) then the group
#' is expanded according to a user-defined 'collapsing' scheme.  This happens
#' recursively until either the group satisfies all conditions and the
#' aggregate is computed, or we run out of collapsing possibilities and the
#' \code{NA} is returned for that group. 
#' \itemize{
#'  \item{\code{accumulate} aggregates over all non-grouping variables defined in 
#'  \code{collapse}} 
#' \item{\code{cumulate} uses a syntax akin to \code{dplyr::summarise}}
#' }
#'
#'
#' @param data \code{[data.frame]} The data to aggregate by (collapsing) groups.
#' @param collapse \code{[formula|data.frame]} representing a group collapsing sequence.
#'        See below for details on how to specify each option.
#' @param test \code{[function]} A function that takes a subset of \code{data} and returns
#'             \code{TRUE} if it is suitable for computing the desired aggregates and 
#'             \code{FALSE} if a collapsing step is necessary.
#' @param fun \code{[function]} A scalar function that will be applied to all columns
#'            of \code{data}.
#' @param ... For \code{accumulate}, extra arguments to be passed to \code{fun}. For
#'            \code{cumulate}, a comma-separated list of \code{name=expression}, 
#'            where \code{expression} defines the aggregating operation.
#'
#' @section Using a formula to define the collapsing sequence:
#'
#' If all combinations of collapsing options are stored as columns in
#' \code{data}, the \code{formula} interface can be used. An example is the
#' easiest way to see how it works.  Suppose that \code{collapse = A*B ~ A1*B +
#' B} This means:
#' \itemize{
#'  \item{Compute output for groups defined by variables A and B}
#'  \item{If for a certain combination \code{(a,b)} in \code{AxB} the data does not
#'        pass the \code{test}, use \code{(a1,b)} in \code{A1xB} as alternative combination to compute
#'        a value for \code{(a,b)} (\code{A1xB} must yield larger groups than \code{AxB}).}
#'  \item{If that does not work, use only \code{B} as a grouping variable to compute
#'        a value for \code{(a,b)}}.
#'  \item{If that does not work, return \code{NA} for that particular combination \code{(a,b)}.}
#' }
#' Generally, the \code{formula} must be of the form \code{X0 ~ X1 + X2 + ... +
#' Xn} where each \code{Xi} is a (product of) grouping variable(s) in the data set.
#'
#' @section Using a data frame to define the collapsing scheme:
#'
#' In this case \code{collapse} is a data frame with columns \code{[A0, A1,
#' ..., An]}.  The variable \code{A0} represents the most fine-grained
#' grouping and must also be present in \code{data}. Aggregation works
#' as follows.
#' \itemize{
#'   \item{Compute output for groups defined by variable \code{A0}}
#'   \item{If for a certain \code{a0} in \code{A0} the corresponding selected
#'         data does not pass the \code{test}, use the larger dataset corresponding to
#'         \code{a1} in \code{A1} to compute output for \code{a1}}.
#'   \item{Repeat the second step until either the \code{test} is passed or 
#'         no more collapsing is possible. In the latter case, return \code{NA}
#'         for that particular value of \code{a0}}.
#' }
#' 
#' 
#'
#' @return
#' A data frame where each row represents a (multivariate) group.  The first
#' columns contain the grouping variables. The next column is called
#' \code{level} and indicates to what level collapsing was necessary to compute
#' a value, where 0 means that no collapsing was necessary. The following
#' colummns contain the aggregates defined in the \code{...} argument.  If no
#' amount of collapsing yields a data set that is satisfactory according to
#' \code{test}, then for that row, the \code{level} and subsequent columns are
#' \code{NA}.
#' 
#' 
#' @examples
#'
#' ## Example of data frame defining collapsing scheme, using accumulate
#'
#' input    <- data.frame(Y1 = 2^(0:8), Y2 = 2^(0:8))
#' input$Y2[c(1,4,7)] <- NA
#' # make sure that the input data also has the most fine-graind (target)
#' # grouping variable
#' input$A0 <- c(123,123,123,135,136,137,212,213,225)
#'
#' # define collapsing sequence
#' collapse <- data.frame(
#'      A0   = c(123, 135, 136, 137, 212, 213, 225)
#'    , A1   = c(12 , 13 , 13 , 13 , 21 , 21 , 22 )
#'    , A2   = c(1  , 1  , 1  , 1  , 2  , 2  , 2  )
#' )
#'
#' accumulate(input
#'  , collapse
#'  , test = function(d) nrow(d)>=3
#'  , fun  = sum, na.rm=TRUE)
#'
#'
#' ## Example of formula defining collapsing scheme, using cumulate
#' input <- data.frame(
#'    A  = c(1,1,1,2,2,2,3,3,3)
#'  , B  = c(11,11,11,12,12,13,21,22,12)
#'  , B1 = c(1,1,1,1,1,1,2,2,1)
#'  , Y  = 2^(0:8)
#' )
#' cumulate(input, collapse=A*B ~ A*B1 + A
#'         , test = function(d) nrow(d) >= 3
#'         , tY = sum(Y))
#'
#'
#' ## Example with formula defining collapsing scheme, using accumulate
#' # The collapsing scheme must be represented by variables in the 
#' # data. All columns not part of the collapsing scheme will be aggregated
#' # over.
#'
#' input <- data.frame(
#'     A  = c(1,1,1,2,2,2,3,3,3)
#'   , B  = c(11,11,11,12,12,13,21,22,12)
#'   , B1 = c(1,1,1,1,1,1,2,2,1)
#'   , Y1 = 2^(0:8)
#'   , Y2 = 2^(0:8)
#' )
#'
#' input$Y2[c(1,4,7)] <- NA
#'
#' accumulate(input
#'  , collapse = A*B ~ A*B1 + A
#'  , test=function(a) nrow(a)>=3
#'  , fun = sum, na.rm=TRUE)
#'
#'
#'
#' ## Example with data.frame defining collapsing scheme, using cumulate
#' dat <- data.frame(A0 = c("11","12","11","22"), Y = c(2,4,6,8))
#' # collapsing scheme
#' csh <- data.frame(
#'    A0 = c("11","12","22")
#'  , A1 = c("1" ,"1", "2") 
#' )
#' cumulate(data = dat
#'    , collapse = csh
#'    , test     = function(d) if (nrow(d)<2) FALSE else TRUE
#'    , mn = mean(Y, na.rm=TRUE)
#'    , md = median(Y, na.rm=TRUE)
#' )
#'
#' @export
accumulate <- function(data, collapse, test, fun, ...){
  compute   <- get_ag(collapse, fun, names(data), ...)
  work(data, collapse, test, compute)
}


#' @rdname accumulate
#' @export
cumulate <- function(data, collapse, test, ...){
  exprs   <- as.list(substitute(list(...))[-1])
  compute <- get_ag(collapse, exprs, names(data))
  work(data, collapse, test, compute)
}


work <- function(data, collapse, test, compute){

  pullback  <- get_pb(collapse, data)
  jmax      <- max_collapse(collapse)
  grpvars   <- groups(collapse)

  out       <- output_backbone(collapse, data)
  R         <- output_template(nrow(out), collapse, compute)
  for ( ia in seq_len(nrow(out)) ){
    j <- 0
    out_level <- out[ia, grpvars, drop=FALSE]
    d <- pullback(out_level, j)
    while ( j < jmax && !test(d) ){
      j <- j + 1
      d <- pullback(out_level,j)
    }
    if ( j < jmax || test(d) ){
      R[[ia]] <- compute(d)
      out$level[ia] <- j
    }
  }
  combine(out, R, compute)
}


# check if the argument is of a basic R type and of length 1.
is_scalar <- function(x){
  length(x) == 1 && (
  is.numeric(x) || 
  is.logical(x) || 
  is.character(x) || 
  is.factor(x) || 
  is.ordered(x) || 
  is.raw(x) || 
  is.complex(x))
}


# In 'work()', results are created in the form of a list, with one entry
# per output group. Each entry may be a vector of aggregates, if the aggregates
# are all 'atomic' and of the same type. Otherwise, each entry is a list.
# 
# The purpose of this function is to take such a row-wise list of the form
# [
#  [X = x1, Y = y1]
#  [X = x2, Y = y2]
# ]
# to a data.frame of theform
# X   Y
# x1  y1
# x2  y2
#
# where the columns may be lists, when the objects in them are more
# complicated then simple scalars (see is_atomic).
#
# Input:
# backbone: a data.frame where each row contains output group labels
# results : a list of results per group: nrow(backbone) == length(results)
# ag      : the agregation function used to create the values in 'results'
#
# Output:
# A data frame, of the form [backbone, results].
#
combine <- function(backbone,results, ag){
  # The simple case: aggregates are atomic; this probably covers
  # most cases.
  if (!any(sapply(results, is.list))){
    return( cbind(backbone, do.call("rbind", results)) )
  }

  # The complexer case: (some) aggregates are lists. 
  L <- lapply(attr(ag,"outnames"), function(var){
    # check if a column can be simplified, if so: do that
    if ( all(sapply(results, function(x) is_scalar(x[[var]]))) ) {
      sapply(results, `[[`,var)
    # otherwise, combine in a list
    } else {
      lapply(results, `[[`,var)
    }
  })

  names(L) <- names(results[[1]])
  # make listcol objects of columns that are lists, so they are 
  # printed better.
  L <- lapply(L, function(x) if (!is.list(x)) x else object_list(x))
  for ( x in names(L) ) backbone[[x]] <- L[[x]]
  backbone
}



##### Entrails ####

# get_pb: create pullback function.
# x  : formula or data.frame
# dat: data to create inverse mapping for.
# out: function accepting a single data frame row with one or more group labels 
#      and a collapse level. It returns a subset of 'dat'.
get_pb <- function(x, dat){
  # Poor man's dispatch since we don't want to export this function
  t <- last(class(x))
  switch(t
    , "data.frame" =  get_pb.data.frame(x,dat)
    , "formula"    =  get_pb.formula(x, dat)
    , stop("'x' must be a data frame or a formula")
  )

}


get_pb.data.frame <- function(x, dat){
  group_labels <- dat[,colnames(x)[1]]
  rownames(x) <- x[,1]
  
  # group: a 1x1 data frame with a single group label 
  # level: collapse level (column index-1 in x)
  function(group, level){ 
    i <- group_labels %in% x[x[,level+1] == x[as.character(unclass(group)), level+1],1]
    dat[i, , drop=FALSE]
  }

}

get_pb.formula <- function(x, dat){
  
  collapse <- c(x[[2]], get_collapse(x[[3]]))

  labels <- dat[all.vars(x)]
  # add column with row numbers; make sure column name does not exist already.
  iname <- "__idx__"
  n <- 0
  while( iname %in% names(labels) ){ 
      n <- n + 1
      iname <- sprintf("%s%i",iname,n)
  }
  labels[,iname] <- seq_len(nrow(labels))
 
  function(group, level){
    d <- merge(group, labels, by=names(group))
    collapse_value <- d[1, all.vars(collapse[[level+1]]),drop=FALSE]
    i <- merge(labels, collapse_value, by=names(collapse_value))[,iname]
    dat[sort(i),,drop=FALSE]
  }

}

# Accepts a formula 'e' of the form
# P1 ~ P2 + P3 + ... + Pn, where each Pj is a single variable
# name or a product of variable names. 
# The return value is a list [P2, P3,...,Pn] of subformulae.
get_collapse <- function(e, L = list()){
  if (length(e) == 1 || e[[1]]=="*") return(append(L, e))
  c(get_collapse(e[[2]],L), get_collapse(e[[3]],L))
}


# Ellipsis currying.
# Accepts a function fun, and returns a 1-argument function
# that has all arguments in ... fixed.
curry <- function(fun,...){
  L <- list(NULL, ...)
  function(x) {
    L[[1]] <- x
    do.call(fun,L)
  }
}

# Create aggregator function
# cps: collapsing scheme (data frame or formula)
# x: aggregating function or object of class 'expression'
# dnames: names of the input data.frame
# ...: extra arguments to be passed to 'x', if it is a function.
#
# Output:
# If x is a function:
#   A function f(dat,..._ that applies x to every non-grouping column 
#   in 'dat'  and returs a named vector with results.
# If x is of type 'expression':
#   A function f(dat) that evaluates each espression in the context of
#   dat and returns a named vector with results.
#
# The output function has an attribute 'outnames' containing 
# the names of the output variables.
get_ag <- function(cps, x, dnames, ...){
  ag <- if ( is.function(x) ){
    f <- curry(x, ...)
    # grouping variables 'gv' are not to be aggregated over
    gv <- if(inherits(cps,"formula")) all.vars(cps) else colnames(cps)
    function(dat) sapply(dat[ ,!colnames(dat) %in% gv, drop=FALSE], f)
  } else {
    function(dat) sapply(x, function(e) with(dat,eval(e)))
  }
  outnames <- if (!is.function(x)) names(x)
              else if (inherits(cps,"formula")) dnames[!dnames %in% all.vars(cps)]
              else dnames[!dnames %in% names(cps)]
  attr(ag, "outnames") <- outnames 
  ag
}


# get relevant group combinations
output_backbone <- function(cps, dat){
  out <- if (inherits(cps,"formula")){
    unique(dat[all.vars(cps[[2]])])
  } else { # cps is a data.frame
    unique(dat[,names(cps)[1],drop=FALSE])
  }
  out$level <- NA_integer_
  out
}

# get maximum number of collapsing steps (base 0).
max_collapse <- function(cps){
  if (inherits(cps,"formula")) length(cps[[3]]) else ncol(cps) - 1
}

# Get the variable names for the desired grouping from the collapsing scheme.
groups <- function(cps) {
  if (inherits(cps,"formula")) all.vars(cps[[2]]) else names(cps)[1]
}

# set up a list where we can store the output aggregate vectors. Seed with 
# NA's for groups that after jmax collapses do not pass the test (and hence
# will not yield an aggregate)
# 
# Input:
# n       : number of output records
# cps     : collapsing scheme (formula or data.frame)
# outnames: column names of output variables
output_template <- function(n, cps, ag){
  vars <- attr(ag,"outnames")
  template <- rep(NA, length(attr(ag,"outnames")))
  names(template) <- vars
  lapply(seq_len(n), function(i) template)
}





