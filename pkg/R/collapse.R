#' Split-Apply-Combine with Collapsing Groups: Multiple Aggregators
#'
#' Attempt to compute one or more grouped aggregates. If a group does not
#' satisfy certain user-defined conditions (such as too many missings, or not
#' enough records) then the group is expanded according to a user-defined
#' 'collapsing' scheme.  This happens recursively until either the group
#' satisfies all conditions and the aggregate is computed, or we run out of
#' collapsing possibilities and the \code{NA} is returned for that group.
#'
#'
#' @param data \code{[data.frame]} The data to aggregate by (collapsing) groups.
#' @param collapse \code{[formula|data.frame]} representing a group collapsing sequence.
#'        See below for details on how to specify each option.
#' @param test \code{[function]} A function that takes a subset of \code{data} and returns
#'             \code{TRUE} if it is suitable for computing the desired aggregates and 
#'             \code{FALSE} if a collapsing step is necessary.
#' @param ... A comma-separated list of \code{name=expression}, where \code{expression}
#'            defines the aggregating operation.
#'
#' @section Using a formula to define the collapsing sequence:
#'
#' If all combinations of collapsing options are stored as columns in
#' \code{data}, the \code{formula} interface can be used. An example is the
#' easiest way to see how it works is to illustrate the idea with an example.
#' Suppose that \code{collapse = A*B ~ A1*B + B} This means:
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
#' Xn} where each \code{Xi} is a product of grouping variables in the data set.
#'
#' @section Using a data frame to define the collapsing sequence:
#'
#' In this case \code{collapse} is a data frame with columns
#' \code{[A0, A1, ..., An]}. The variable \code{A0} must also be
#' present in \code{data}, and should represent a collapsed version
#' of \code{A0}. This means:
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
#'
#' @examples
#' # some example data
#' dat <- data.frame(A0 = c("11","12","11","22"), Y = c(2,4,6,8))
#' # collapsing scheme
#' csh <- data.frame(
#'    A0 = c("11","12","22")
#'  , A1 = c("1" ,"1", "2") 
#' )
#' collapse(data = dat
#'    , collapse = csh
#'    , test     = function(d) if (nrow(d)<2) FALSE else TRUE
#'    , mn = mean(Y, na.rm=TRUE)
#'    , md = median(Y, na.rm=TRUE)
#' )
#'
#' @export
collapse <- function(data, collapse, test, ...){

  if (inherits(collapse,"data.frame")){
    collapse1(as.data.frame(data), as.data.frame(collapse), test, ...)
  } else if(inherits(collapse,"formula")){
    collapse2(as.data.frame(data), collapse, test, ...)
  } else {
    stop("Collapse must be a 'data.frame' or a 'formula'")
  }
}



collapse1 <- function(data, collapse, test, ...){
  exprs <- as.list(substitute(list(...))[-1])


  collapse <- as.data.frame(lapply(collapse, as.character))
  rownames(collapse) <- collapse[,1]

  # set up output set
  out <- data.frame(collapse[,1], NA, row.names=collapse[,1])
  colnames(out) <- c(names(collapse)[1], "level")
  vals <- as.data.frame(matrix(NA, nrow=nrow(collapse), ncol=length(exprs)))
  colnames(vals) <- names(exprs)
  out <- cbind(out, vals)


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

collapse2 <- function(data, formula, test,...){
  exprs <- as.list(substitute(list(...))[-1])


  # get collapsing scheme
  collapse <- get_collapse(formula[[3]])

  # set up output set
  lhs_vars <- all.vars(formula[[2]])
  out <- cbind(unique(data[lhs_vars]), level=NA_integer_)
  vals <- as.data.frame(matrix(NA, nrow=nrow(collapse), ncol=length(expres)))
  colnames(vals) <- names(exprs)
  out <- cbind(out, vals)
  
  for (ia in seq_len(nrow(out))){
    d <- merge(data, out[ia,lhs_vars])
    j = 0
    while(j < length(collapse) && !test(d)){
      j <- j + 1
      rhs_vars <- all.vars(collapse[[j]])
      d <- merge(data, d[1, rhs_vars])
    }
    if ( j < length(collapse) || test(d)){
      out[ia,length(lhs_vars) + 1] <- j
      out[ia,(length(lhs_vars)+1):ncol(out)] <- lapply(exprs, function(e) with(d, eval(e)))
    }
  }
  out

}



get_collapse <- function(e, L = list()){
  if (length(e) == 1 || e[[1]]=="*") return(append(L, e))
  c(get_collapse(e[[2]],L), get_collapse(e[[3]],L))
}







