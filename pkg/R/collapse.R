#' Split-Apply-Combine with Collapsing Groups
#'
#' Attempt to compute a grouped aggregate. If a group does not satisfy certain
#' user-defined conditions (such as too many missings, or not enough records)
#' then the group is expanded according to a user-defined 'collapsing' scheme.
#' This happens recursively until either the group satisfies all conditions and
#' the aggregate is computed, or we run out of collapsing possibilities and the 
#' \code{NA} is returned for that group.
#'
#'
#' @param data \code{[data.frame]} The data to aggregate by (collapsing) groups.
#' @param collapse \code{[data.frame]} representing a group collapsing sequence (see details).
#'        All columns must be of the same type.
#' @param test \code{[function]} A function that takes a subset of \code{data} and returns
#'             \code{TRUE} if it is suitable for computing the desired aggregates and 
#'             \code{FALSE} if a collapsing step is necessary.
#' @param ... A comma-separated list of \code{name=expression}, where \code{expression}
#'            defines the aggregating operation.
#'
#' @return
#' A data frame with \code{nrow(collapse)} rows. The first column equals the first column
#' of \code{collapse} and indicates the group for which an aggregate was computed. 
#' The second column indicates the number of collapses that took place. The subsequent
#' columns contain the results of the expressins listed in the \code{...} argument.
#' If no amount of collapsing yields a data set that passes the test, then for that
#' row, the second and all subsequent columns are \code{NA}.
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
  exprs <- as.list(substitute(list(...))[-1])


  collapse <- as.data.frame(lapply(collapse, as.character))
  rownames(collapse) <- collapse[,1]

  # set up output set
  out <- data.frame(collapse[,1], NA, row.names=collapse[,1])
  colnames(out) <- c(names(collapse)[1], "collapse_level")
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
    if (!test(d)) next
    out[a,2] <- j-1
    out[a,3:ncol(out)] <- lapply(exprs, function(e) with(d, eval(e)))
  }
  rownames(out) <- NULL
  out
}



