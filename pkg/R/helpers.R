
stopf <- function(fmt,...) stop(sprintf(fmt,...), call.=FALSE)

last <- function(x) x[length(x)]

complete_cases <- function(d) !Reduce(`|`, lapply(d, is.na))



#' Demand minimal number of functons
#' 
#' @param n Minimal number of records in a group.
#'
#' @examples
#'
#' min_records(5)(women)
#' min_records(200)(women)
#'
#' @family helpers
#'
#' @export
min_records <- function(n){
  stopifnot(is.numeric(n), n>=0)
  min_rec <- n
  function(d) if (nrow(d) < n) FALSE else TRUE
}

#' Demand minimal number of complete records
#'
#' @param n Minimal number of records that must be complete
#' @param vars \code{[TRUE|column index]} Column index into the data to
#'        be tested (e.g. a character vectod with variable names or
#'        a numeric vector with column positions). The indexed columns
#'        will be testsed for completeness (absence of \code{NA}). Be default
#'        \code{vars=TRUE} meaning that all columns are taken into account.
#'
#' @family helpers
#'
#' @examples
#'
#' @export
min_complete <- function(n, vars=TRUE){
  stopifnot(is.numeric(n), n>=0
          , isTRUE(vars) || 
            is.character(vars) || 
            is.numeric(vars) || 
            is.logical(vars))
  min_compl <- n
  vars <- vars
  function(d) sum(complete_cases(d[vars])) >= min_compl
}

#' Demand minimal fraction of complete records
#'
#' @param r Minimal fraction of records that must be complete.
#' @param vars \code{[TRUE|column index]} Column index into the data to
#'        be tested (e.g. a character vectod with variable names or
#'        a numeric vector with column positions). The indexed columns
#'        will be testsed for completeness (absence of \code{NA}). Be default
#'        \code{vars=TRUE} meaning that all columns are taken into account.
#'
#' @family helpers
#'
#' @examples
#' 
#' @export
frac_complete <- function(r, vars=TRUE){
  stopifnot(is.numeric(r), 0<=r, r<=1
          , isTRUE(vars) || 
            is.character(vars) || 
            is.numeric(vars) || 
            is.logical(vars))
  min_frac <- r
  vars <- vars
  function(d) mean(complete_cases(d[vars])) >= r
}

#' Use a validate::validator object to test a group
#'
#' Returns \code{TRUE} when the data passes all checks defined in the
#' \code{validator} object, otherwise \code{FALSE}.
#'
#' @param v \code{[validator]} a validator object from the
#'        \code{validate} package.
#' @param ... options passed to \code{validate::confront}
#'
#' @note
#' Requires the \code{validate} package to be installed.
#'
#' @references
#' Mark P. J. van der Loo, Edwin de Jonge (2021). Data Validation
#' Infrastructure for R. Journal of Statistical Software, 97(10), 1-31.
#' doi:10.18637/jss.v097.i10
#'
#' @examples
#'
#' 
#'
#'
#' @export
from_validator <- function(v,...){
  if (!requireNamespace('validate', quietly=TRUE)){
    stop("Could not load the 'validate' package.")
  }
  args <- list(x=v,...)
  function(d) all( do.call(validate::confront, append(list(dat=d),args) ) )

}



#' Derive collapsing scheme from hierarchical classification
#'
#' Derive a collapsing scheme where group labels collapse to their
#' parents in the hierarchy.
#'
#' @param x \code{[character|integer]} labels in a hierarchical classification (lowest level)
#' @param levels \code{[integer >=0]} how many collapsing levels to include. Zero means
#'        only include the original labels.
#'
#' @examples
#' # balanced hierarchical classification
#' csh_from_digits(c("111","112","121","122","123"))
#' csh_from_digits(c("111","112","121","122","123"),levels=1)
#' 
#' # unbalanced hierarchical classification
#' csh_from_digits(c("111","112","121","122","1221","1222"))
#' csh_from_digits(c("111","112","121","122","1221","1222"),levels=2)
#'
#' @export
csh_from_digits <- function(x, levels=max(nchar(x))-1){
  stopifnot(levels>=0
           , levels < max(nchar(x)))
  x <- as.character(x)
  nlevels <- max(nchar(x))

  A <- matrix(NA_character_,nrow=length(x), ncol=nlevels)
  for ( i in seq_len(nlevels)){
    A[,i] <- substr(x,1,nlevels+1-i)
  }
  colnames(A) <- sprintf("A%d",seq_len(nlevels)-1)
  as.data.frame(A)[1:(levels+1)]
}



