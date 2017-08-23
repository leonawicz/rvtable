#' Merge Conditional Distributions
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
  if(is.null(atts[[2]])) atts[[2]] <- "Prob"
  distr <- tabletype(x) == "distribution"
  grp <- as.character(dplyr::groups(x))
  not_grp <- names(x)[!names(x) %in% c(grp, atts[[1]], atts[[2]], "weights_")]
  if(!length(not_grp)){
    .no_merge_warn(x)
    if("weights_" %in% names(x)) x$weights_ <- NULL
    if(distr) return(x)
  } else {
    w <- get_weights(x, not_grp)
    any_wts <- any(as.numeric(unlist(purrr::map(w, ~.x$weights))) != 1)
    if(any_wts)
      warning("Ungrouped ID variable levels have unequal weights. Consider `marginalize` instead of `merge_rvtable`.")
  }
  weights <- get_weights(x)
  if(missing(density.args)) density.args <- get_density_args(x)
  if(missing(sample.args)) sample.args <- get_sample_args(x)
  if(distr){
    if(is.null(sample.args$n)) sample.args$n <- 10000
    x <- set_sample_args(x, sample.args)
    sample.args$density.args <- density.args
    x <- do.call(sample_rvtable, c(list(x=x), sample.args))
    sample.args$density.args <- NULL
  }
  x <- .rvtable_makedist(x) %>% dplyr::group_by_(.dots=grp) #%>%
  weights <- weights[names(weights) %in% names(x)]
  x <- .add_rvtable_class(x, atts[[1]], atts[[2]], atts[[3]], TRUE, weights, density.args, sample.args) %>%
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
  has.weights <- "weights_" %in% names(x) && any(x$weights_ != 1)
  if(discrete){
    if(has.weights){
      x <- dplyr::do(x, tibble::data_frame(
        Val=sample(x=.$Val, size=n, replace=TRUE, prob=.$weights_)))
    } else {
      x <- dplyr::do(x, tibble::data_frame(
        Val=sample(x=.$Val, size=n, replace=TRUE)))
    }
    x <- dplyr::group_by_(x, .dots=grp) %>% dplyr::do(
      tibble::data_frame(
        Val=as.numeric(names(table(.$Val))),
        Prob=as.numeric(table(.$Val)) / sum(table(.$Val)))
    )
  } else {
    if(has.weights){
      x <- dplyr::do(x, tibble::data_frame(
        Val=do.call(density,
                    c(list(x=sample(x=.$Val, size=n, replace=TRUE, prob=.$weights_)), density.args))$x,
        Prob=do.call(density,
                     c(list(x=sample(x=.$Val, size=n, replace=TRUE, prob=.$weights_)), density.args))$y,
        ))
    } else {
      x <- dplyr::do(x, tibble::data_frame(
        Val=do.call(density, c(list(x=.$Val), density.args))$x,
        Prob=do.call(density, c(list(x=.$Val), density.args))$y,
        ))
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

.no_merge_warn <- function(x){
  has.weights <- "weights_" %in% names(x)
  merge_type1 <- if(has.weights) "marginalize" else "merge"
  merge_type2 <- if(has.weights) "marginalizing" else "merging"
  merge_type_vars <- if(has.weights) " " else " ungrouped "
  no_merge1 <- paste0("No", merge_type_vars, "ID variables to ", merge_type1, ".")
  no_merge2 <- if(tabletype(x) == "distribution") paste0("No ", merge_type2, " performed.") else
    paste0("Density re-estimation performed, but no variables ", merge_type1, "d.")
  no_merge <- paste(no_merge1, no_merge2)
  warning(no_merge)
}

#' Marginal Distributions
#'
#' Obtain a marginal distribution of a random variable in an rvtable.
#'
#' Grouping variables are ignored when marginalizing the distribution of a random variable over explicit categorical variables.
#' Unlike \code{merge_rvtable}, \code{marginalize} will not merge ungrouped ID variables.
#' It will only marginalize over the ID variables specified in \code{margin}.
#' It will also account for unequal weighting of an ID variable's levels, taken from the \code{weights} attribute of an rvtable,
#' whereas \code{merge_rvtable} is a simpler function that ignores weights.
#'
#' @param x an rvtable.
#' @param margin variable(s) in rvtable to marginalize over.
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
  Prob2 <- if(is.null(Prob)) "Prob" else Prob
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
      x <- x %>% split(.[[m]])
      x <- x[match(names(x), w[[m]]$levels)]
      wi <- w[[m]]$weights
      x <- purrr::map2(x, wi, ~dplyr::mutate(.x, weights_=.y)) %>%
        dplyr::bind_rows() %>% dplyr::group_by_(.dots=grp2)
      x <- .add_rvtable_class(x, Val, Prob, discrete, distr, list(), density.args, sample.args) %>%
        merge_rvtable() %>% dplyr::group_by_(.dots=grp2) %>%
        .add_rvtable_class(Val, Prob2, discrete, TRUE, w_all, density.args, sample.args)
    }
  } else {
    w_sub <- w_all[!names(w_all) %in% margin]
    grp2 <- dplyr::setdiff(names(x), c(Val, Prob, margin))
    x <- dplyr::mutate(x, weights_=1) %>% dplyr::group_by_(.dots=grp2) %>%
      .add_rvtable_class(Val, Prob, discrete, distr, w_all, density.args, sample.args) %>%
      merge_rvtable() %>% dplyr::group_by_(.dots=grp2)
    x <- .add_rvtable_class(x, Val, Prob2, discrete, TRUE, w_sub, density.args, sample.args)
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
    attr(x, "weights")[[id]] <- tibble::data_frame(levels=values, weights=1)
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
    x <- dplyr::bind_rows(dplyr::select_(dplyr::ungroup(x), .dots=dots), x2)
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
