
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


# get_pb: create pullback function.
# x  : formula or data.frame
# dat: data to create inverse mapping for.
# out: function accepting (a) group label(s) and a collapse level
#      that returns an integer row index for 'dat'
get_pb <- function(x, dat){
  # Poor man's dispatch since we don't want to export this function
  t <- last(class(x))
  switch(t
    , "data.frame" =  get_pb.data.frame(x,dat)
    , "formula"    =  get_pb.formula(x, dat)
    , stop("'x' must be a data frame or a formula")
  )

}


get_pb.data.frame <- function(x, dat){
  group_labels <- dat[,colnames(x)[1]]
  rownames(x) <- x[,1]


  function(group, level){ 
    which(group_labels %in% x[x[,level+1] == x[as.character(group), level+1],1])
  }

}

get_pb.formula <- function(x, dat){
  
  collapse <- c(x[[2]], get_collapse(x[[3]]))


  labels <- dat[all.vars(x)]
  # add column with row numbers; make sure column name does not exist already.
  iname <- "__idx__"
  n <- 0
  while( iname %in% names(labels) ){ 
      n <- n + 1
      iname <- sprintf("%s%i",iname,n)
  }
  labels[,iname] <- seq_len(nrow(labels))
 
  function(group, level){
    d <- merge(group, labels, by=names(group))
    collapse_value <- d[1, all.vars(collapse[[level+1]]),drop=FALSE]
    merge(labels, collapse_value, by=names(collapse_value))[,iname]
  }

}



get_collapse <- function(e, L = list()){
  if (length(e) == 1 || e[[1]]=="*") return(append(L, e))
  c(get_collapse(e[[2]],L), get_collapse(e[[3]],L))
}





