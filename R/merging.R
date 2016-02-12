#' Get levels of categorical variables
#'
#' Obtain the levels associated with categorical variables in an rvtable.
#'
#' All columns in an rvtable other than the Val and Prob columns are assumed to represent categorical variables.
#' If a column is a factor, all possible levels of the factor are returned even if they do not appear in the table.
#' For other types all unique values observed in the table are returned.
#' If \code{variable} is \code{NULL}, a list is returned for all categorical variables.
#' A shorter list is returned if \code{variable} is used to specify a subset of columns.
#'
#' @param x an rvtable.
#' @param variable specific column name or vector of column names in \code{x}.
#'
#' @return \code{NULL} if there are no columns other than Val and Prob, otherwise a list.
#' @export
#'
#' @examples
#' library(data.table)
#' x <- data.table(
#'   id1=rep(LETTERS[1:5], each=4),
#'   id2=factor(c("low", "high")),
#'   id3=rep(1:2, each=2),
#'   Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)) %>% rvtable
#' get_levels(x)
get_levels <- function(x, variable=NULL){
  .rv_class_check(x)
  if(is.null(variable)) variable <- names(x) else if(!(variable %in% names(x))) stop("`variable` not found.")
  variable <- setdiff(variable, c("Val", "Prob"))
  if(!length(variable)) stop("invalid variable")
  lev <- lapply(variable, function(id, d) if(is.factor(d[[id]])) levels(d[[id]]) else unique(d[[id]]), d=x)
  names(lev) <- variable
  lev
}

# merge distributions using a cycle of bootstrap resampling followed by density re-estimation, based on grouping variables
#' Title
#'
#' @param x
#' @param density.args
#' @param sample.args
#'
#' @return
#' @export
#'
#' @examples
merge_rvtable <- function(x, density.args=list(), sample.args=list()){
  .rv_class_check(x)
  grp <- groups(x)
  sample.args$density.args=density.args
  n <- sample.args$n
  if(is.null(n)) n <- 10000
  has.weights <- "weights" %in% names(x)
  discrete <- attr(x, "rvtype")=="discrete"
  if(attr(x, "tabletype")=="distribution"){
    x <- do.call(sample_rvtable, c(list(x=x), sample.args))
  }
  if(discrete){
    if(has.weights){
      x <- summarise(x, Val=sample(x=Val, size=n, replace=TRUE, prob=weights))
    } else {
      x <- summarise(x, Val=sample(x=Val, size=n, replace=TRUE))
    }
    x <- group_by_(x, .dots=grp) %>% summarise(Val=as.numeric(names(table(Val))), Prob=as.numeric(table(Val))/sum(table(Val)))
  } else {
    if(has.weights){
      x <- summarise(x,
        Val=do.call(density, c(list(x=sample(x=Val, size=n, replace=TRUE, prob=weights)), density.args))$x,
        Prob=do.call(density, c(list(x=sample(x=Val, size=n, replace=TRUE, prob=weights)), density.args))$y)
    } else {
      x <- summarise(x, Val=do.call(density, c(list(x=Val), density.args))$x, Prob=do.call(density, c(list(x=Val), density.args))$y)
    }
  }
  group_by_(x, .dots=grp) %>% rvtable(x, discrete=discrete)
}

# marginalize distribution of RV over explicit categorical variables, ignores grouping variables
#' Title
#'
#' @param x
#' @param margin
#' @param weights
#' @param density.args
#' @param sample.args
#'
#' @return
#' @export
#'
#' @examples
marginalize <- function(x, margin, weights=NULL, density.args=list(), sample.args=list()){
  .rv_class_check(x)
  discrete <- attr(x, "rvtype")=="discrete"
  tbl <- attr(x, "tabletype")
  id <- names(x)
  if(!length(margin)) stop("Must specify variable(s) to marginalize over.")
  if(!is.null(weights) & length(margin) > 1) stop("May only marginalize over one variable at a time if using level weights.")
  if(any(!(margin %in% id))) stop("Marginalizing variable not found.")
  if(any(margin %in% c("Val", "Prob"))) stop("Invalid marginalizaing variable.")
  grp2 <- lapply(setdiff(id, c("Val", "Prob", margin)), as.symbol)
  if(!length(grp2)) grp2 <- NULL
  x <- group_by_(x, .dots=grp2)
  if(!is.null(weights)){
    lev <- get_levels(x, margin)
    if(length(weights) != length(lev[[margin]])) stop("Number of weights does not match the number of levels in `margin`.")
    x <- x %>% split(.[[margin]]) %>% purrr::map2(weights, ~mutate(.x, weights=.y)) %>% rbindlist %>% group_by_(.dots=grp2)
  }
  class(x) <- unique(c("rvtable", class(x)))
  attr(x, "rvtype") <- ifelse(discrete, "discrete", "continuous")
  attr(x, "tabletype") <- tbl
  x <- merge_rvtable(x, density.args=density.args, sample.args=sample.args) %>% group_by_(.dots=grp2)
  rvtable(x, discrete=discrete)
}


# Repeat cycle of bootstrap resampling followed by density re-estimation n-1 times, assumes Prob column present
#' Title
#'
#' @param x
#' @param n
#' @param start
#' @param density.args
#' @param sample.args
#'
#' @return
#' @export
#'
#' @examples
bootDenCycle <- function(x, n, start=NULL, density.args=list(), sample.args=list()){
  .rv_class_check(x)
  rv <- attr(x, "rvtype")
  discrete <- rv=="discrete"
  tbl <- attr(x, "tabletype")
  grp <- groups(x)
  if(tbl=="sample") stop("rvtable must be in distribution form, not sample form.")
  if(!("Cycle" %in% names(x))) x$Cycle <- 1
  if(is.null(start)) start <- max(x$Cycle)
  if(n<=1){
    cols <- c(as.character(grp), "Cycle", "Val")
    x <- select_(x, .dots=lapply(c(cols, "Prob"), as.symbol)) %>%
      group_by(Cycle, add=TRUE) %>% distinct_(.dots=lapply(cols, as.symbol))
    return(x)
  }
  x2 <- filter(x, Cycle==start)
  class(x2) <- unique(c("rvtable", class(x2)))
  attr(x2, "rvtype") <- rv
  attr(x2, "tabletype") <- tbl
  x2 <- merge_rvtable(x2, density.args=density.args, sample.args=sample.args) %>% mutate(Cycle=start+1)
  bind_rows(x, x2) %>% data.table %>% group_by_(.dots=grp) %>% rvtable(discrete=discrete) %>%
    bootDenCycle(n-1, start+1, density.args=density.args, sample.args=sample.args)
}
