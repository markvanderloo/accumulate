
catf  <- function(fmt,...) cat(sprintf(fmt,...))
stopf <- function(fmt,...) stop(sprintf(fmt,...), call.=FALSE)

last <- function(x) x[length(x)]

complete_cases <- function(d) !Reduce(`|`, lapply(d, is.na))

# create a classed list
listcol <- function(x){
  structure(x, class="listcol")
}

format.listcol <- function(x,...){
  sapply(x, function(u) sprintf("<%s>",paste(class(u),collapse=",")))
}

