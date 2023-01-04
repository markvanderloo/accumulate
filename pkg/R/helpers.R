
catf  <- function(fmt,...) cat(sprintf(fmt,...))
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

#' Test your testing function for common edge cases
#'
#' Writing a testing function that works on any subset of records of a
#' dataframe can be quite subtle. This function tries the testing function on a
#' number of common (edge) cases that are easily overlooked.  It is \emph{not}
#' a unit test: a smoke test will not tell you whether your output is correct.
#' It only checks the output data type (mustbe \code{TRUE} or \code{FALSE} and
#' reports if errors, warnings, or messages occur.
#'
#' @param dat an example dataset. For example the full dataset
#'        to be fed into \code{\link{accumulate}} or \code{\link{cumulate}}.
#' @param test A testing function to be passed as argument to \code{\link{accumulate}}
#'        or \code{\link{cumulate}}.
#' @param verbose \code{[logical]} If \code{TRUE}, all results (including
#'        passed tests) are printed.  If \code{FALSE} only failed tests are printed.
#' @param halt \code{[logical]} toggle stopping when an error is thrown
#'
#' @return \code{NULL}, invisibly. This function has as side-effect that test
#' results are printed to screen.
#'
#'
#' @examples
#' dat <- data.frame(x = 1:5, y=(-2):2)
#' smoke_test(dat, function(d) y > 0)   #error: Y not found
#' smoke_test(dat, function(d) d$y > 0) # issue: output too long, not robust against NA
#' smoke_test(dat, function(d) sum(d$y > 0) > 2) # issue: not robust against NA
#' smoke_test(dat, function(d) sum(d$y > 0, na.rm=TRUE) > 2) # OK
#'
#' @export
smoke_test <- function(dat, test, verbose=FALSE, halt=TRUE){
  try_this(dat, test, verbose, info="full dataset") || !halt || return(invisible())
  try_this(dat[1,,drop=FALSE], test, verbose, info="first record") || !halt || return(invisible())
  try_this(dat[0,,drop=FALSE], test, verbose, info="zero records") || !halt || return(invisible())

  vars <- colnames(dat)
  for (var in vars){
    d <- dat[1,,drop=FALSE]
    d[1,var] <- NA
    try_this(d, test, verbose
     , info=sprintf("first record and %s is NA",var)) || !halt || return(invisible())
  } 

  d <- dat[1,,drop=FALSE]
  d[1,] <- NA
  try_this(d, test, verbose
    , info="first record and all values NA") || !halt || return(invisible()) 

  for (var in vars){
    d <- dat
    d[,var] <- NA
    try_this(d, test, verbose
            , info=sprintf("full dataset and %s is NA for all records",var)) || 
      !halt || return(invisible())
  }


  catf("\n")
  invisible(NULL)
 
}

try_this <- function(d, f, verbose, info){
  msg <- character()
  wrn <- character()
  err <- character()
  out <- NULL
  out <- tryCatch(withCallingHandlers(f(d)
          , message = function(m){ msg <<- append(msg, m$message); invokeRestart("muffleMessage")}
          , warning = function(w){ wrn <<- append(wrn, w$message);  invokeRestart("muffleWarning")}
         )
    , error   = function(e) err <<- append(err, e$message)
  )
  print(smoke(list(result=out, msg=msg, wrn=wrn, err=err, info=info)),verbose=verbose)
  invisible(length(err) == 0)
}

smoke <- function(x){
  structure(x,class="smoke")
}

print.smoke <- function(x, verbose){
  if (isTRUE(x$result)|| isFALSE(x$result)){
    if(verbose) catf("\nTest with %s: OK", x$info)
  } else {
    rep <- character(0)
    if (length(x$err) == 0){ 
      if (!is.logical(x$result)){
        rep <- c(rep,sprintf("Output is of class %s (must be 'logical')", class(x$result)))
      }
      if ( length(x$result) != 1){
        rep <- c(rep,sprintf("Output has length %d (must be 1)", length(x$result)))
      }
      if ( any(is.na(x$result)) ){
        rep <- c(rep, sprintf("NA detected in output (must be TRUE or FALSE)"))
      }
    }
    if (length(x$msg)>0){
      rep <- c(rep, paste(sprintf("MSG: %s",trimws(x$msg)), collapse="\n") )
    }
    if (length(x$wrn)>0){
      rep <- c(rep, paste(sprintf("WRN: %s",trimws(x$wrn)), collapse="\n") )
    }
    if (length(x$err)>0){
      rep <- c(rep, paste(sprintf("ERR: %s",trimws(x$err)), collapse="\n") )
    }
    report <- paste(rep, collapse="\n  ")
    catf("\nTest with %s raised issues.\n\r   %s",x$info, report)
  }
}




