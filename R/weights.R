#' ID variable level weights
#'
#' Get or set ID variable level weights.
#'
#' Weights are important for marginalizing distributions, when collapsing an rvtable over levels of an ID
#' variable must account for known unequal weighting of levels. Weights are strictly used by \code{marginalize}.
#'
#' \code{get_weights} get a list of 2-column data frames of levels and associated weights for ID variables in an rvtable.
#' \code{set_weights} sets weights for levels of ID variables in an rvtable.
#' Weights for levels of ID variables in an rvtable are assumed to be equal unless explicitly set otherwise.
#' They are usually set via \code{set_weights} but can also be set as part of a call to the \code{rvtable} constructor.
#'
#' If \code{weights=NULL}, \code{id} is ignored and all ID variables' levels are set to equal weighting.
#' If \code{id} refers to a single ID variable in \code{x}, then \code{weights} can be a valid weights data frame.
#' In general, \code{weights} is a list of data frames corresponding to the variables specified \code{id}.
#' If \code{weights} is a named list, the named map to \code{id}. If unnamed, they must be provided in the same order as \code{id}.
#' Equal weights are set for an individual ID variable's levels
#' when a list element in \code{weights} correpsonding to an \code{id} value is \code{NULL}.
#'
#' @param x rvtable.
#' @param id character, ID column name in \code{x}.
#' @param weights list, data frame, or \code{NULL}. See details.
#'
#' @name weights
#'
#' @return a list for \code{get_weights}.
#'
#' @examples
#' x <- rvtable(data.frame(
#'   id1=rep(LETTERS[1:5], each=4),
#'   id2=factor(c("low", "high")),
#'   id3=rep(1:2, each=2),
#'   Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)))
#' get_weights(x)
#' get_weights(x, "id1")
#' x <- set_weights(x, "id1", data.frame(levels=LETTERS[1:5], weights=1:5))
#' get_weights(x, c("id1", "id3"))
#' wts <- data.frame(levels=1:2, weights=c(0.3, 0.7))
#' x <- set_weights(x, c("id1", "id3"), list(id1=NULL, id3=wts))
#' get_weights(x, c("id1", "id3"))
#' wts <- data.frame(levels=levels(x$id2), weights=c(2, 1))
#' x <- set_weights(x, c("id1", "id2"), list(NULL, wts))
#' get_weights(x)
NULL

#' @export
#' @rdname weights
get_weights <- function(x, id){
  .rv_class_check(x)
  w <- attr(x, "weights")
  if(missing(id)) return(w)
  w[id]
}

#' @export
#' @rdname weights
set_weights <- function(x, id, weights){
  .rv_class_check(x)
  ids <- idcols(x)
  if(is.null(ids)){
    warning("`x` has no ID variables. Weights not set.")
    return(x)
  }
  if(.no_weights(weights)){
    attr(x, "weights") <- .set_all_weights(x, NULL, valcol(x), probcol(x))
    return(x)
  }
  eq_len <- "`id` and `weights` must have equal length."
  if(!all(id %in% ids)) stop("`id` must contain only valid ID variables in `x`.")
  if(!is.null(weights) && !is.list(weights))
    stop("`weights` must be NULL, a data frame, or list of data frames.")
  if(length(id) == 1){
    if(!is.data.frame(weights) && length(weights) > 1) stop(eq_len)
    if(is.data.frame(weights)) weights <- list(weights)
  } else {
    if(is.data.frame(weights)) stop("`weights` must be a list of data frames when `id` is a vector.")
    if(length(id) != length(weights)) stop(eq_len)
  }
  if(is.null(names(weights))) names(weights) <- id
  weights <- weights[id]
  for(i in id) .check_valid_weights_df(x, i, weights[[i]], allow_null=TRUE)
  w <- get_weights(x)
  idx <- purrr::map_lgl(weights, ~.no_weights(.x))
  nam <- names(weights)
  if(any(!nam %in% id))
    stop("All `weights` names must be `x` ID column names.")
  if(any(!id %in% nam))
    stop("All `x` ID column names must be in  `weights` names.")
  if(length(idx) && any(idx))
    weights[idx] <- purrr::map(which(idx == TRUE), ~.weights_tbl(x, id[.x], weights[.x]))
  w[id] <- weights[id]
  attr(x, "weights") <- w
  x
}

.check_weights <- function(weights, idvars){
  no_wts <- .no_weights(weights)
  if(!is.null(weights) && !is.list(weights)) stop("`weights` must be NULL or a list.")
  n <- length(weights)
  if(!length(idvars) && !no_wts)
    stop("Cannot have weights when `x` has no ID columns.")
  if(!length(idvars)) return(list())
  if(n != 0 & n != length(idvars))
    stop("`weights` must be NULL or an empty list, `list()`, for unweighted auto-fill, or have length equal to the number of ID columns in `x`") # nolint
  if(no_wts){
    weights <- vector("list", length(idvars))
    names(weights) <- idvars
  } else {
    nam <- names(weights)
    if(is.null(nam))
      stop("`weights` must be a named list. Use ID column names of `x`.")
    if(any(!nam %in% idvars))
      stop("All `weights` names must be `x` ID column names.")
    if(any(!idvars %in% nam))
      stop("All `x` ID column names must be in  `weights` names.")
    for(i in seq_along(weights)){
      w <- weights[[i]]
      if(!is.null(w) && !is.data.frame(w))
        stop("list entries in `weights` must be NULL or a data frame.")
      if(is.data.frame(w)){
        if(ncol(w) != 2) stop("`weights` list entries must be data frames with two columns.")
        if(!all(names(w) %in% c("levels", "weights")))
          stop("Column names of weights data frames must be 'levels' and 'weights'.")
      }
    }
  }
  weights
}

.no_weights <- function(weights){
  is.null(weights) || identical(weights, list()) ||
    identical(weights, list(NULL)) || identical(weights, list(x=1)[0])
}

.weights_tbl <- function(x, id, weights=NULL){
  x<- x[[id]]
  lev <- if(is.factor(x)) levels(x) else unique(x)
  no_wts <- .no_weights(weights)
  if(!no_wts && (nrow(weights) != length(lev)))
    stop("`weights` must have number of rows equal to number of levels in `x[[id]]`.")
  if(no_wts) return(dplyr::tbl_df(data.frame(levels=lev, weights=1, stringsAsFactors=FALSE)))
  if(!"tbl_df" %in% weights) dplyr::tbl_df(weights)
}

.set_all_weights <- function(x, weights, Val, Prob){
  idvars <- names(x)[which(!names(x) %in% c(Val, Prob))]
  weights <- .check_weights(weights, idvars)
  weights <- purrr::map2(idvars, weights, ~.weights_tbl(x, id=.x, weights=.y))
  names(weights) <- idvars
  weights
}

.check_valid_weights_df <- function(x, id, weights, allow_null=TRUE){
  if(is.null(weights) & allow_null) return(TRUE)
  if(length(id) > 1) stop("Pass one ID variable to `id` at a time.")
  if(!is.data.frame(weights)) stop("`weights` list elements must be a data frame if not NULL.")
  if(ncol(weights) != 2 || !all(names(weights) %in% c("levels", "weights")))
    stop("`weights` data frame must have two columns: `levels` and `weights`.")
  if(!is.numeric(weights$weights)) stop("Weights must be numeric.")
  if(any(weights$weights < 0)) stop("Weights must be non-negative.")
  w <- get_weights(x, id)[[1]]
  if(nrow(weights) != nrow(w)) stop("`weights` data frame has wrong number of rows.")
  if(length(unique(weights$levels)) < nrow(weights))
    stop("`weights` does not have unique levels.")
  if(!all(weights$levels %in% w$levels)) stop("Invalid levels in `weights`.")
  TRUE
}
