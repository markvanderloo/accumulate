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
#' @seealso \code{\link{demand}}
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
  if (inherits(collapse, "data.frame")){
    accumulate1(as.data.frame(data), as.data.frame(collapse), test, fun, ...)
  } else if (inherits(collapse, "formula")){
    accumulate2(as.data.frame(data), collapse, test, fun, ...)
  } else {
    stop("Collapse must be a 'data.frame' or a 'formula'")
  }

}


accumulate1 <- function(data, collapse, test, fun, ...){

  # Target agregation level
  g  <- colnames(collapse)[1]
  ig <- colnames(data) == g
  g_type <- last(class(data[,g]))


  if (!any(ig) ){
    stopf("First column '%s' in 'collapse' is not present in 'data'",g)
  }


  collapse <- as.data.frame(lapply(collapse, as.character))
  data[,g] <- as.character(data[,g])
  rownames(collapse) <- collapse[,1] # for lookup

 
  out <- data.frame(unique(data[,g]), level=NA_integer_) 
  names(out)[1] <- g
  
  vals <- as.data.frame( matrix(NA, nrow = nrow(out), ncol = sum(!ig)) )
  colnames(vals) <- colnames(data)[!ig]
  out <- cbind(out, vals)

  for ( ia in seq_len(nrow(out)) ){
    j = 1
    out_level <- out[ia,1]
    d <- data[data[,g] == out_level,!ig,drop=FALSE]
    while( j < ncol(collapse) && !test(d) ){
      j <- j + 1
      backmap <- collapse[collapse[,j] == collapse[out_level,j], 1]
      d <-  data[data[,g] %in% backmap, !ig, drop=FALSE]
    }


    if ( j < length(collapse) || test(d)){
      out[ia, 2]           <- j-1
      out[ia, 3:ncol(out)] <- lapply(d[!ig], fun, ...)
    }
  }
  out[,1] <- do.call(paste0("as.",g_type), list(out[,1]))
  out

}


accumulate2 <- function(data, formula, test, fun, ...){

  collapse <- get_collapse(formula[[3]])

  lhs_vars <- all.vars(formula[[2]])
  
  data_vars <- !colnames(data) %in% all.vars(formula)

  # set up output set
  out <- cbind(unique(data[lhs_vars]), level=NA_integer_)
  vals <- as.data.frame(matrix(NA, nrow=nrow(out), ncol=sum(data_vars)))
  colnames(vals) <- colnames(data)[data_vars]
  out <- cbind(out, vals)

 
  for (ia in seq_len(nrow(out))){
    d <- merge(data, out[ia,lhs_vars])
    j = 0
    rhs_vars <- ""
    while(j < length(collapse) && !test(d)){
      j <- j + 1
      rhs_vars <- all.vars(collapse[[j]])
      d <- merge(data, d[1, rhs_vars,drop=FALSE])
    }

    if ( j < length(collapse) || test(d)){
      out[ia,length(lhs_vars) + 1] <- j
      out[ia,(length(lhs_vars)+2):ncol(out)] <- lapply(d[data_vars], fun, ... )
    }
  }
  rownames(out) <- NULL
  out

}




#' @rdname accumulate
#' @export
cumulate <- function(data, collapse, test, ...){

  if (inherits(collapse,"data.frame")){
    cumulate1(as.data.frame(data), as.data.frame(collapse), test, ...)
  } else if(inherits(collapse,"formula")){
    cumulate2(as.data.frame(data), collapse, test, ...)
  } else {
    stop("Collapse must be a 'data.frame' or a 'formula'")
  }
}




cumulate1 <- function(data, collapse, test, ...){
  exprs <- as.list(substitute(list(...))[-1])

  out <- prepare_output_df(collapse, names(exprs))

  rownames(collapse) <- collapse[,1]

  g <- names(collapse)[1]
  data[,g] <- as.character(data[,g])

  for ( a in collapse[,1]){
    d <- data[data[,g] == a,]
    j <- 1
    while ( j < ncol(collapse) && !test(d) ){
      j <- j + 1
      grps <- collapse[collapse[,j] == collapse[a, j],1]
      d <- data[data[,g] %in% grps, ]
    }
    if ( j < ncol(collapse) || test(d)){
      out[a,2] <- j-1
      out[a,3:ncol(out)] <- lapply(exprs, function(e) with(d, eval(e)))
    }
  }
  rownames(out) <- NULL
  out
}

cumulate2 <- function(data, formula, test,...){
  exprs <- as.list(substitute(list(...))[-1])

 
  collapse <- get_collapse(formula[[3]])

  lhs_vars <- all.vars(formula[[2]])
  
  # set up output set
  out <- cbind(unique(data[lhs_vars]), level=NA_integer_)
  vals <- as.data.frame(matrix(NA, nrow=nrow(out), ncol=length(exprs)))
  colnames(vals) <- names(exprs)
  out <- cbind(out, vals)

 
  for (ia in seq_len(nrow(out))){
    d <- merge(data, out[ia,lhs_vars])
    j = 0
    rhs_vars <- ""
    while(j < length(collapse) && !test(d)){
      j <- j + 1
      rhs_vars <- all.vars(collapse[[j]])
      d <- merge(data, d[1, rhs_vars,drop=FALSE])
    }

    if ( j < length(collapse) || test(d)){
      out[ia,length(lhs_vars) + 1] <- j
      out[ia,(length(lhs_vars)+2):ncol(out)] <- lapply(exprs, function(e) with(d, eval(e)))
    }
  }
  rownames(out) <- NULL
  out

}

prepare_output_df <- function(collapse, outvars){

  collapse <- as.data.frame(lapply(collapse, as.character))
  out <- data.frame(collapse[,1], NA_integer_, row.names=collapse[,1])
  colnames(out) <- c(names(collapse)[1], "level")
  vals <- as.data.frame(matrix(NA, nrow=nrow(out), ncol=length(outvars)))
  colnames(vals) <- outvars
  cbind(out, vals)

}


get_collapse <- function(e, L = list()){
  if (length(e) == 1 || e[[1]]=="*") return(append(L, e))
  c(get_collapse(e[[2]],L), get_collapse(e[[3]],L))
}







