#' Get levels of ID variables
#'
#' Obtain the levels associated with categorical (ID) variables in an rvtable.
#'
#' All columns in an rvtable other than the values and probabilities columns are assumed to represent categorical variables.
#' If a column is a factor, all possible levels of the factor are returned even if they do not appear in the table.
#' For other types all unique values observed in the table are returned.
#' If \code{id} is \code{NULL}, a list is returned for all categorical variables.
#' A shorter list is returned if \code{id} is used to specify a subset of columns.
#'
#' @param x an rvtable.
#' @param id specific column name or vector of column names in \code{x}.
#'
#' @return \code{NULL} if there are no columns other than Val and Prob, otherwise a list.
#' @export
#'
#' @examples
#' x <- rvtable(data.frame(
#'   id1=rep(LETTERS[1:5], each=4),
#'   id2=factor(c("low", "high")),
#'   id3=rep(1:2, each=2),
#'   Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)))
#' get_levels(x)
get_levels <- function(x, id=NULL){
  x <- .lost_rv_class_check(x)
  .rv_class_check(x)
  novar <- is.null(id)
  if(novar) id <- names(x) else if(!(id %in% names(x))) stop("`id` not found.")
  id <- dplyr::setdiff(id, c("Val", "Prob"))
  if(!length(id)){
    if(novar) return(NULL)
    stop("invalid ID variable.")
  }
  lev <- purrr::map(id, ~(if(is.factor(x[[.x]])) levels(x[[.x]]) else unique(x[[.x]])))
  names(lev) <- id
  lev
}

#' Merge RV Table Conditional Distributions
#'
#' Merge conditional distributions of a random variable in an rvtable over levels of ungrouped categorical variables.
#'
#' Distributions are merged using a cycle of bootstrap resampling followed by density re-estimation.
#' Merging relies simply on what \code{x} is grouped by.
#' All levels of all ungrouped variables are merged. Weights are also ignored by \code{merge_rvtable}.
#' This is why the behavior is referred to as merging rather than marginalizing.
#' If an ungrouped variable has levels with unequal weights, weights are ignored, the merging is still forced,
#' but a warning is thrown.
#'
#' This function is also called internally by \code{marginalize}, in which case it does not ignore weights
#' and will calculate a proper marginal distribution even when there are unequal weights.
#' If calling this function directly, it is important to know that it is intended for simple merging.
#' Recommended practice is to use \code{marginalize} to ensure proper marginal distributions
#' are always obtained by default regardless of equal or unequal weights.
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
#' library(dplyr)
#' x <- data.frame(
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
  atts <- list(valcol(x), probcol(x), rvtype(x)=="discrete")
  grp <- as.character(dplyr::groups(x))
  not_grp <- names(x)[!names(x) %in% c(grp, atts[[1]], atts[[2]])]
  if(!length(not_grp)){
    warning("No ungrouped ID variables to merge. No merging performed.")
    return(x)
  } else {
    w <- get_weights(x, not_grp)
    any_wts <- any(as.numeric(unlist(purrr::map(w, ~.x$weights))) != 1)
    if(any_wts)
      warning("Ungrouped ID variable levels have unequal weights. Consider `marginalize` instead of `merge_rvtable`.")
  }
  weights <- get_weights(x, grp)
  if(missing(density.args)) density.args <- get_density_args(x)
  if(missing(sample.args)) sample.args <- get_sample_args(x)
  if(tabletype(x)=="distribution"){
    if(is.null(sample.args$n)) sample.args$n <- 10000
    attr(x, "sample.args") <- sample.args
    sample.args$density.args <- density.args
    x <- do.call(sample_rvtable, c(list(x=x), sample.args))
  }
  .rvtable_makedist(x) %>% dplyr::group_by_(.dots=grp) %>%
    .add_rvtable_class(atts[[1]], atts[[2]], atts[[3]], TRUE, weights, density.args, sample.args) %>%
    .lost_rv_class_check()
}

.rvtable_makedist <- function(x){
  .rv_class_check(x)
  Val <- valcol(x)
  Prob <- probcol(x)
  if(is.null(Prob)) Prob <- "Prob"
  discrete <- rvtype(x)=="discrete"
  density.args <- get_density_args(x)
  sample.args <- get_sample_args(x)
  n <- sample.args$n
  if(is.null(n)) n <- 10000
  grp <- as.character(dplyr::groups(x))
  weights <- get_weights(x, grp)
  x <- .rvtable_rename(x, "to")
  has.weights <- "weights" %in% names(x)
  if(discrete){
    if(has.weights){
      x <- dplyr::do(x, data.frame(
        Val=sample(x=.$Val, size=n, replace=TRUE, prob=.$weights), stringsAsFactors=FALSE))
    } else {
      x <- dplyr::do(x, data.frame(
        Val=sample(x=.$Val, size=n, replace=TRUE), stringsAsFactors=FALSE))
    }
    x <- dplyr::group_by_(x, .dots=grp) %>% dplyr::do(
      data.table::data.table(Val=as.numeric(names(table(.$Val))),
                             Prob=as.numeric(table(.$Val)) / sum(table(.$Val))))
  } else {
    if(has.weights){
      x <- dplyr::do(x, data.frame(
        Val=do.call(density,
                    c(list(x=sample(x=.$Val, size=n, replace=TRUE, prob=.$weights)), density.args))$x,
        Prob=do.call(density,
                     c(list(x=sample(x=.$Val, size=n, replace=TRUE, prob=.$weights)), density.args))$y,
        stringsAsFactors=FALSE))
    } else {
      x <- dplyr::do(x, data.frame(
        Val=do.call(density, c(list(x=.$Val), density.args))$x,
        Prob=do.call(density, c(list(x=.$Val), density.args))$y,
        stringsAsFactors=FALSE))
    }
  }
  x <- .add_rvtable_class(x, Val, Prob, discrete, TRUE, weights, density.args, sample.args)
  .rvtable_rename(x, "from")
}

.rvtable_rename <- function(x, vp){
  x <- .lost_rv_class_check(x)
  .rv_class_check(x)
  Val <- valcol(x)
  Prob <- probcol(x)
  distr <- tabletype(x) == "distribution"
  if(vp=="to"){
    if(Val != "Val") x <- dplyr::rename_(x, Val=lazyeval::interp(~v, v=Val))
    if(distr && Prob != "Prob") x <- dplyr::rename_(x, Prob=lazyeval::interp(~p, p=Prob))
  } else if(vp=="from"){
    if(Val != "Val") x <- dplyr::rename_(x, .dots=stats::setNames("Val", Val))
    if(distr && Prob != "Prob") x <- dplyr::rename_(x, .dots=stats::setNames("Prob", Prob))
  }
  .lost_rv_class_check(x)
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
#' library(dplyr)
#' x <- data.frame(
#'   id1=rep(LETTERS[1:5], each=4),
#'   id2=factor(c("low", "high")),
#'   id3=rep(1:2, each=2),
#'   Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)) %>% rvtable
#' marginalize(x, c("id1", "id2"))
#'
#' get_levels(x, "id1")
#' get_weights(x, "id1")
#' marginalize(x, "id1")
#' wts <- data.frame(levels=LETTERS[1:5], weights=c(1, 1.5, 2, 4, 1))
#' x <- set_weights(x, "id1", wts)
#' get_weights(x, "id1")
#' marginalize(x, "id1")
#' }
marginalize <- function(x, margin, density.args, sample.args){
  x <- .lost_rv_class_check(x)
  .rv_class_check(x)
  Val <- valcol(x)
  Prob <- probcol(x)
  discrete <- rvtype(x) == "discrete"
  distr <- tabletype(x) == "distribution"
  if(missing(density.args)) density.args <- get_density_args(x)
  if(missing(sample.args)) sample.args <- get_sample_args(x)
  if(missing(margin)) stop("Must specify variable(s) to marginalize over.")
  if(any(!(margin %in% names(x)))) stop("Marginalizing variable not found.")
  if(any(margin %in% c(Val, Prob))) stop("Invalid marginalizaing variable.")
  w <- get_weights(x, margin)
  w_all <- get_weights(x)
  any_wts <- any(as.numeric(unlist(purrr::map(w, ~.x$weights))) != 1)
  if(any_wts){
    id <- names(x)
    for(i in seq_along(names(w))){
      m <- names(w)[i]
      id <- id[id != m]
      w_all <- w_all[names(w_all) != m]
      grp2 <- dplyr::setdiff(id, c(Val, Prob))
      if(!length(grp2)) grp2 <- NULL
      x <- dplyr::group_by_(x, .dots=grp2)
      if(!distr) Prob <- NULL
      x <- x %>% split(.[[m]])
      x <- x[match(names(x), w[[m]]$levels)]
      wi <- w[[m]]$weights
      x <- purrr::map2(x, wi, ~dplyr::mutate(.x, weights=.y)) %>%
        bind_rows() %>% dplyr::group_by_(.dots=grp2)
      x <- .add_rvtable_class(x, Val, Prob, discrete, distr, list(), density.args, sample.args) %>%
        merge_rvtable() %>% dplyr::group_by_(.dots=grp2) %>%
        .add_rvtable_class(Val, Prob, discrete, distr, w_all, density.args, sample.args)
    }
  } else {
    w_sub <- w_all[!names(w_all) %in% margin]
    grp2 <- dplyr::setdiff(names(x), c(Val, Prob, margin))
    x <- dplyr::group_by_(x, .dots=grp2) %>%
      .add_rvtable_class(Val, Prob, discrete, distr, w_all, density.args, sample.args) %>%
      merge_rvtable() %>% dplyr::group_by_(.dots=grp2)
    x <- .add_rvtable_class(x, Val, Prob, discrete, distr, w_sub, density.args, sample.args)
  }
  x
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
#' library(dplyr)
#' x <- data.frame(
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
  Val <- valcol(x)
  Prob <- probcol(x)
  rv <- rvtype(x)
  discrete <- rv=="discrete"
  tbl <- tabletype(x)
  if(missing(density.args)) density.args <- get_density_args(x)
  if(missing(sample.args)) sample.args <- get_sample_args(x)
  grp <- dplyr::groups(x)
  weights <- get_weights(x)
  if(!keep %in% c("all", "last")) stop("keep must be 'all' or 'last'.")
  if(tbl=="sample") stop("rvtable must be in distribution form, not sample form.")
  if(!("Cycle" %in% names(x))) x$Cycle <- 1
  if(is.null(start)) start <- max(x$Cycle)
  if(n <= 1){
    cols <- c(as.character(grp), "Cycle", Val, Prob)
    x <- dplyr::ungroup(x) %>% dplyr::select_(.dots=cols) %>%
      dplyr::group_by_(.dots=grp) %>% dplyr::distinct_(.dots=cols) %>%
      .add_rvtable_class(Val, Prob, discrete, TRUE, weights, density.args, sample.args)
    return(x)
  }

  force_weights <- function(x, id, values){
    attr(x, "weights")[[id]] <- data.frame(levels=values, weights=1)
    x
  }

  cycles <- unique(x$Cycle)
  if(keep=="all"){
    x2 <- dplyr::filter_(x, .dots=list(paste0("Cycle==", start)))
    class(x2) <- unique(c("rvtable", class(x2)))
    attr(x2, "rvtype") <- rv
    attr(x2, "tabletype") <- tbl
    x2 <- force_weights(x2, "Cycle", cycles)
    weights$Cycle <- attr(x2, "weights")$Cycle
    weights2 <- weights
    weights2$Cycle <- weights2$Cycle[weights2$Cycle$levels == start, ]
    x2 <- .add_rvtable_class(x2, Val, Prob, discrete, TRUE, weights2, density.args, sample.args) %>%
      merge_rvtable(density.args=density.args, sample.args=sample.args) %>%
      dplyr::mutate(Cycle=start + 1)
    dots <- names(x2)
    x <- dplyr::bind_rows(dplyr::select_(ungroup(x), .dots=dots), x2)
    cycles <- c(cycles, start + 1)
  } else {
    x <- force_weights(x, "Cycle", cycles)
    weights$Cycle <- attr(x, "weights")$Cycle
    x <- .add_rvtable_class(x, Val, Prob, discrete, TRUE, weights, density.args, sample.args) %>%
      merge_rvtable(density.args=density.args, sample.args=sample.args) %>%
      dplyr::mutate(Cycle=start + 1)
    cycles <- start + 1
  }
  x <- dplyr::group_by_(x, .dots=grp)
  x <- force_weights(x, "Cycle", cycles)
  weights$Cycle <- attr(x, "weights")$Cycle
  weights <- weights[c(as.character(grp), "Cycle")]
  x <- .add_rvtable_class(x, Val, Prob, discrete, TRUE, weights, density.args, sample.args)
  cycle_rvtable(x, n - 1, start + 1, density.args=density.args, sample.args=sample.args, keep=keep)
}
