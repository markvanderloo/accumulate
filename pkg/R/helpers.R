stopf <- function(fmt,...) stop(sprintf(fmt,...), call.=FALSE)

last <- function(x) x[length(x)]


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
  min_rec <- n
  function(d) if (nrow(d) < n) FALSE else TRUE
}

#' Demand maximal number of records
#'
#' @param n Minimal number of records in a group.
#'
#' @examples
#'
#' max_records(5)(women)
#' max_records(200)(women)
#'
#'
#' @family helpers
#'
#' @export
max_records <- function(n){
  max_rec <- n
  function(d) if (nrow(d) >n) FALSE else TRUE
}

#' Demand any number of expressions to be TRUE
#'
#' @param ... comma-separated list of expressions that must all evaluate to \code{TRUE}
#'            in order for a data set to be acceptable.
#'
#' @return A data-checking function.
#' @family helpers
#' @export
demand <- function(...){
  expr <- as.list(substitute(list(...))[-1])  
  function(d){
    L <- lapply(expr, function(e) with(d, eval(e)))
    all(sapply(L,all))
  }
}

#' Number of incomplete combinations
#'
#' A number of vectors is pointwise tested for missing values.  For each
#' position, if any vector has a missing \code{FALSE} is computed otherwise
#' \code{TRUE}. The total number of \code{FALSE}s is returned.
#'
#' @param ... comma-separated list of vectors of equal length
#'
#' @note This function is intended to facilitate the use of \code{\link{demand}}
#' @return A non-negative integer.
#'
#'
#' @examples
#' n_incomplete(c(1,NA,3), 1:3)
#'
#' 
#' @family helpers
#'
#' @export
n_incomplete <- function(...){
  L <- list(...)
  sum( Reduce(`|`, lapply(L,  is.na)) )
}

#' Number of complete combinations
#'
#' A number of vectors is pointwise tested for missing values.  For each
#' position, if any vector has a missing \code{FALSE} is computed otherwise
#' \code{TRUE}. The total number of \code{TRUE}s is returned.
#'
#' @param ... comma-separated list of vectors of equal length
#'
#' @note This function is intended to facilitate the use of \code{\link{demand}}
#' @return A non-negative integer.
#'
#'
#' @examples
#' n_complete(c(1,NA,3), 1:3)
#'
#' 
#' @family helpers
#'
#' @export
n_complete <- function(...){

  L <- list(...)
  sum( Reduce(`&`, lapply(L,  function(x) !is.na(x) )) )

}

#' Fraction of incomplete combinations
#'
#' A number of vectors is pointwise tested for missing values. For each
#' position, if any vector has a missing \code{FALSE} is computed otherwise
#' \code{TRUE}. The fraction of \code{FALSE}s is returned.
#'
#' @param ... comma-separated list of vectors of equal length
#'
#' @note This function is intended to facilitate the use of \code{\link{demand}}
#' @return A non-negative integer.
#'
#'
#' @examples
#' fraction_incomplete(c(1,NA,3), 1:3)
#'
#' 
#' @family helpers
#'
#' @export
fraction_incomplete <- function(...){
  L <- list(...)
  mean( Reduce(`|`, lapply(L,  is.na)) )
}

#' Fraction of complete combinations
#'
#' A number of vectors is pointwise tested for missing values.  For each
#' position, if any vector has a missing \code{FALSE} is computed otherwise
#' \code{TRUE}. The fraction of \code{TRUE}s is returned.
#'
#' @param ... comma-separated list of vectors of equal length
#'
#' @note This function is intended to facilitate the use of \code{\link{demand}}
#'
#' @return A non-negative integer.
#'
#' @examples
#' fraction_complete(c(1,NA,3), 1:3)
#'
#' 
#' @family helpers
#' @export
fraction_complete <- function(...){
  L <- list(...)
  mean( Reduce(`&`, lapply(L,  function(x) !is.na(x) )) )
}


#' Derive collapsing scheme from hierarchical classification
#'
#' Derive a collapsing scheme where group labels collapse to their
#' parents in the hierarchy.
#'
#' @param x \code{[character|integer]} labels in a hierarchical classification (lowest level)
#' @param levels \code{[integer]>=0} how many collapsing levels to include. Zero means
#'        only include the original labels.
#'
#' 
#' @export
csh_from_digits <- function(x, levels=max(nchar(x))-1){
  stopifnot(levels>=0)
  x <- as.character(x)
  nlevels <- max(nchar(x))

  A <- matrix(NA_character_,nrow=length(x), ncol=nlevels)
  for ( i in seq_len(nlevels)){
    A[,i] <- substr(x,1,nlevels+1-i)
  }
  colnames(A) <- sprintf("A%d",seq_len(nlevels)-1)
  as.data.frame(A)[1:(levels+1)]
}



