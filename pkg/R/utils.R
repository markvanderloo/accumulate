
catf  <- function(fmt,...) cat(sprintf(fmt,...))
stopf <- function(fmt,...) stop(sprintf(fmt,...), call.=FALSE)

last <- function(x) x[length(x)]

complete_cases <- function(d) !Reduce(`|`, lapply(d, is.na))

#' create a classed list
#'
#' @param x a list
#'
#' @keywords internal
#'
#' @examples
#' listcol(list(lm(speed ~ dist, data=cars)))
#'
#' @export
listcol <- function(x) structure(x, class=c("listcol","list"))

#' @rdname listcol
#' @export
format.listcol <- function(x,...){
  sapply(x, function(u) sprintf("<%s>",paste(class(u),collapse=",")))
}

#' @rdname listcol
#' @export
print.listcol <- function(x,...) print(format.listcol(x,...))

#' @rdname listcol
#' @export
`[.listcol` <- function(x,i,j,...,drop=TRUE){
  listcol(unclass(x)[i])
}


