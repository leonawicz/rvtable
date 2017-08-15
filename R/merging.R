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
#' library(dplyr)
#' x <- data.table(
#'   id1=rep(LETTERS[1:5], each=4),
#'   id2=factor(c("low", "high")),
#'   id3=rep(1:2, each=2),
#'   Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)) %>% rvtable
#' get_levels(x)
get_levels <- function(x, variable=NULL){
  x <- .lost_rv_class_check(x)
  .rv_class_check(x)
  if(is.null(variable)) variable <- names(x) else if(!(variable %in% names(x))) stop("`variable` not found.")
  variable <- dplyr::setdiff(variable, c("Val", "Prob"))
  if(!length(variable)) stop("invalid variable")
  lev <- lapply(variable, function(id, d) if(is.factor(d[[id]])) levels(d[[id]]) else unique(d[[id]]), d=x)
  names(lev) <- variable
  lev
}

#' Merge RV Table Conditional Distributions
#'
#' Merge conditional distributions of a random variable in an rvtable over levels of ungrouped categorical variables.
#'
#' Distributions are merged using a cycle of bootstrap resampling followed by density re-estimation.
#' Merging relies simply on what \code{x} is grouped by.
#' It assumes equal weights for all levels of all grouping variables over which a random variable is being merged.
#' This is why it is called merging rather than marginalizing.
#' This function is also used by \code{marginalize} and can properly obtain a marginal distribution when passed an rvtable including a \code{weights} column, which \code{marginalize} provides.
#' If calling this function directly, it is important to know that it is generally intended for simple merging.
#' Use \code{marginalize} to ensure proper marginal distributions are obtained since it can be used with or without non-constant weights.
#'
#' @param x an rvtable.
#' @param density.args optional arguments passed to \code{density}. If supplied, overrides the \code{density.args} attribute of \code{x}.
#' @param sample.args optional arguments used when sampling. If supplied, overrides the \code{sample.args} attribute of \code{x}.
#'
#' @return an rvtable.
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(dplyr)
#' x <- data.table(
#'   id1=rep(LETTERS[1:5], each=4),
#'   id2=factor(c("low", "high")),
#'   id3=rep(1:2, each=2),
#'   Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)) %>% rvtable
#' merge_rvtable(x)
#' x %>% group_by(id1) %>% merge_rvtable
#' x %>% group_by(id1, id2) %>% merge_rvtable
#' }
merge_rvtable <- function(x, density.args, sample.args){
  x <- .lost_rv_class_check(x)
  .rv_class_check(x)
  grp <- dplyr::groups(x)
  if(missing(density.args)) density.args <- attr(x, "density.args")
  if(missing(sample.args)) sample.args <- attr(x, "sample.args")
  if(attr(x, "tabletype")=="distribution"){
    if(is.null(sample.args$n)) sample.args$n <- 10000
    attr(x, "sample.args") <- sample.args
    sample.args$density.args <- density.args
    x <- do.call(sample_rvtable, c(list(x=x), sample.args))
  }
  x <- .rvtable_makedist(x)
  x <- dplyr::group_by_(x, .dots=grp)
  .lost_rv_class_check(x)
}

.rvtable_rename <- function(x, vp){
  x <- .lost_rv_class_check(x)
  .rv_class_check(x)
  Val <- attr(x, "valcol")
  Prob <- attr(x, "probcol")
  if(vp=="to"){
    if(Val != "Val") x <- dplyr::rename_(x, Val=lazyeval::interp(~v, v=Val))
    if(Prob != "Prob") x <- dplyr::rename_(x, Prob=lazyeval::interp(~p, p=Prob))
  } else if(vp=="from"){
    if(Val != "Val") x <- dplyr::rename_(x, .dots=stats::setNames("Val", Val))
    if(Prob != "Prob") x <- dplyr::rename_(x, .dots=stats::setNames("Prob", Prob))
  }
  .lost_rv_class_check(x)
}

.rvtable_makedist <- function(x){
  .rv_class_check(x)
  Val <- attr(x, "valcol")
  Prob <- attr(x, "probcol")
  discrete <- attr(x, "rvtype")=="discrete"
  has.weights <- "weights" %in% names(x)
  density.args <- attr(x, "density.args")
  sample.args <- attr(x, "sample.args")
  x <- .rvtable_rename(x, "to")
  if(discrete){
    if(has.weights){
      x <- dplyr::do(x, data.table::data.table(
        Val=sample(x=.$Val, size=n, replace=TRUE, prob=.$weights)))
    } else {
      x <- dplyr::do(x, data.table::data.table(
        Val=sample(x=.$Val, size=n, replace=TRUE)))
    }
    x <- dplyr::group_by_(x, .dots=grp) %>% dplyr::do(
      data.table::data.table(Val=as.numeric(names(table(.$Val))),
                             Prob=as.numeric(table(.$Val)) / sum(table(.$Val))))
  } else {
    if(has.weights){
      x <- dplyr::do(x, data.table::data.table(
        Val=do.call(density,
                    c(list(x=sample(x=.$Val, size=n, replace=TRUE, prob=.$weights)), density.args))$x,
        Prob=do.call(density,
                     c(list(x=sample(x=.$Val, size=n, replace=TRUE, prob=.$weights)), density.args))$y))
    } else {
      x <- dplyr::do(x, data.table::data.table(
        Val=do.call(density, c(list(x=.$Val), density.args))$x,
        Prob=do.call(density, c(list(x=.$Val), density.args))$y))
    }
  }
  x <- .add_rvtable_class(x, Val, Prob, discrete, dist, density.args, sample.args)
  .rvtable_rename(x, "from")
}

#' Marginal Distribution rvtable
#'
#' Obtain a marginal distribution of a random variable in an rvtable.
#'
#' Grouping variables are ignored when marginalizing the distribution of a random variable over explicit categorical variables.
#' \code{margin} must be explicit.
#' \code{weights} only applies in the clear case of marginalizing over a single categorical variable.
#' Marginalizing over multiple variables in a single call to \code{marginalize}
#' is only available assuming equal weights for all values of those variables.
#' When using weights, \code{marginalize} must be called on one variable at a time.
#' Call \code{get_levels} on an rvtable first to ensure weights are passed in the correct order.
#'
#' @param x an rvtable.
#' @param margin variable(s) in rvtable to marginalize over.
#' @param weights relative weights for unique values or levels of a single \code{margin} variable.
#' @param density.args optional arguments passed to \code{density}. If supplied, overrides the \code{density.args} attribute of \code{x}.
#' @param sample.args optional arguments used when sampling. If supplied, overrides the \code{sample.args} attribute of \code{x}.
#'
#' @return an rvtable.
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(dplyr)
#' x <- data.table(
#'   id1=rep(LETTERS[1:5], each=4),
#'   id2=factor(c("low", "high")),
#'   id3=rep(1:2, each=2),
#'   Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)) %>% rvtable
#' marginalize(x, c("id1", "id2"))
#' get_levels(x, "id1")
#' marginalize(x, "id1", weights=c(1, 1.5, 2, 4, 1))
#' }
marginalize <- function(x, margin, weights=NULL, density.args, sample.args){
  x <- .lost_rv_class_check(x)
  .rv_class_check(x)
  Val <- attr(x, "valcol")
  Prob <- attr(x, "probcol")
  discrete <- attr(x, "rvtype")=="discrete"
  tbl <- attr(x, "tabletype")
  if(missing(density.args)) density.args <- attr(x, "density.args")
  if(missing(sample.args)) sample.args <- attr(x, "sample.args")
  x <- .rvtable_rename(x, "to")
  id <- names(x)
  if(missing(margin)) stop("Must specify variable(s) to marginalize over.")
  if(!is.null(weights) & length(margin) > 1)
    stop("May only marginalize over one variable at a time if using level weights.")
  if(any(!(margin %in% id))) stop("Marginalizing variable not found.")
  if(any(margin %in% c("Val", "Prob"))) stop("Invalid marginalizaing variable.")
  grp2 <- lapply(dplyr::setdiff(id, c("Val", "Prob", margin)), as.symbol)
  if(!length(grp2)) grp2 <- NULL
  x <- dplyr::group_by_(x, .dots=grp2)
  if(!is.null(weights)){
    lev <- get_levels(x, margin)
    if(length(weights) != length(lev[[margin]]))
      stop("Number of weights does not match the number of levels in `margin`.")
    x <- x %>% split(.[[margin]]) %>% purrr::map2(weights, ~dplyr::mutate(.x, weights=.y)) %>%
      data.table::rbindlist() %>% dplyr::group_by_(.dots=grp2)
  }
  .rvtable_rename(x, "from")
  x <- .add_rvtable_class(x, Val, Prob, discrete, tbl=="distribution", density.args, sample.args)
  x <- merge_rvtable(x) %>% dplyr::group_by_(.dots=grp2) %>% .lost_rv_class_check()
}

#' Repeated Resampling Utility
#'
#' Repeat cycles of bootstrap resampling followed by density re-estimation for testing purposes.
#'
#' This function repeats cycles of bootstrap resampling from a distribution followed by distribution re-estimation \code{n - 1} times.
#' \code{x} must be a distribution-form rvtable and not a sample-form rvtable.
#'
#' @param x an rvtable in distribution form only.
#' @param n total number of iteratively estimated distributions to return, including the original from \code{x}.
#' @param start usually \code{NULL} when first called and on subsequent recursive calls an integer representing which cycle to begin from in the updated rvtable.
#' @param density.args optional arguments passed to \code{density}. If supplied, overrides the \code{density.args} attribute of \code{x}.
#' @param sample.args optional arguments used when sampling. If supplied, overrides the \code{sample.args} attribute of \code{x}.
#' @param keep character, iterations to retain in output table. Options are \code{"all"} (default) or \code{"last"}.
#'
#' @return an rvtable.
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(dplyr)
#' x <- data.table(
#'   id1=rep(LETTERS[1:5], each=4),
#'   id2=factor(c("low", "high")),
#'   id3=rep(1:2, each=2),
#'   Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)) %>% rvtable
#' cycle_rvtable(x, 2)
#' x %>% group_by(id1, id2) %>% cycle_rvtable(3, keep="last")
#' }
cycle_rvtable <- function(x, n, start=NULL, density.args, sample.args, keep="all"){
  x <- .lost_rv_class_check(x)
  .rv_class_check(x)
  rv <- attr(x, "rvtype")
  discrete <- rv=="discrete"
  tbl <- attr(x, "tabletype")
  if(missing(density.args)) density.args <- attr(x, "density.args")
  if(missing(sample.args)) sample.args <- attr(x, "sample.args")
  grp <- dplyr::groups(x)
  if(!keep %in% c("all", "last")) stop("keep must be 'all' or 'last'.")
  if(tbl=="sample") stop("rvtable must be in distribution form, not sample form.")
  if(!("Cycle" %in% names(x))) x$Cycle <- 1
  if(is.null(start)) start <- max(x$Cycle)
  if(n<=1){
    cols <- c(as.character(grp), "Cycle", "Val", "Prob")
    x <- dplyr::select_(x, .dots=lapply(cols, as.symbol)) %>%
      dplyr::group_by_(.dots=grp) %>% dplyr::distinct_(.dots=lapply(cols, as.symbol)) %>% rvtable()
    return(x)
  }
  if(keep=="all"){
    x2 <- dplyr::filter_(x, .dots=list(paste0("Cycle==", start)))
    class(x2) <- unique(c("rvtable", class(x2)))
    attr(x2, "rvtype") <- rv
    attr(x2, "tabletype") <- tbl
    x2 <- merge_rvtable(x2, density.args=density.args, sample.args=sample.args) %>%
      dplyr::mutate(Cycle=start+1)
    x <- dplyr::bind_rows(x, x2)
  } else {
    x <- merge_rvtable(x, density.args=density.args, sample.args=sample.args) %>%
      dplyr::mutate(Cycle=start+1)
  }
  x %>% data.table::data.table() %>%
    dplyr::group_by_(.dots=grp) %>% rvtable(discrete=discrete) %>%
    cycle_rvtable(n-1, start+1, density.args=density.args, sample.args=sample.args, keep=keep)
}
