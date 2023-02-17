
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
#' object_list(list(lm(speed ~ dist, data=cars)))
#'
#' @export
object_list <- function(x) structure(x, class=c("object_list","list"))

#' @rdname object_list
#' @export
format.object_list <- function(x,...){
  sapply(x, function(u) sprintf("<%s>",paste(class(u),collapse=",")))
}

#' @rdname object_list
#' @export
print.object_list <- function(x,...) print(format.object_list(x,...))

#' @rdname object_list
#' @export
`[.object_list` <- function(x,i,j,...,drop=TRUE){
  object_list(unclass(x)[i])
}


