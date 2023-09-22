
catf  <- function(fmt,...) cat(sprintf(fmt,...))
stopf <- function(fmt,...) stop(sprintf(fmt,...), call.=FALSE)

last <- function(x) x[length(x)]

complete_cases <- function(d) !Reduce(`|`, lapply(d, is.na))

#' Create a classed list
#'
#' Classed lists are used to pretty-print a list that is stored
#' in a data frame.
#'
#' @param x a list
#'
#' @keywords internal
#'
#' @examples
#' object_list(list(lm(speed ~ dist, data=cars)))
#'
#' @export
#' @keywords internal
object_list <- function(x) structure(x, class=c("object_list","list"))

#' @rdname object_list
#' @export
#' @keywords internal
format.object_list <- function(x,...){
  sapply(x, function(u) sprintf("<%s>",paste(class(u),collapse=",")))
}

#' @rdname object_list
#' @export
#' @keywords internal
print.object_list <- function(x,...) print(format.object_list(x,...))

#' @rdname object_list
#' @export
`[.object_list` <- function(x,i,j,...,drop=TRUE){
  object_list(unclass(x)[i])
}


# check whether formula follows the allowed syntax
ok_formula <- function(x){
  x[[1]] == "~" && is_product(x[[2]]) && is_sum(x[[3]])
}

is_sum <- function(x){
  length(x)==1 || is_product(x) || (x[[1]] == "+" &&
    (length(x[[2]]) == 1 || is_sum(x[[2]]) || is_product(x[[2]])) &&
    (length(x[[3]]) == 1 || is_sum(x[[3]]) || is_product(x[[3]])))
}

is_product <- function(x){
  length(x) == 1 || (x[[1]] == "*" &&
  (length(x[[2]] == 1) || is_product(x[[2]])) &&
  (length(x[[3]] == 1) || is_product(x[[3]])) )
}











