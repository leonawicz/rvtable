#' Inverse Probability Mass Function
#'
#' Compute the probability mass function for a specified categorical variable conditional on values of the primary random variable.
#'
#' This function computes the pmf of a categorical variable,
#' providing probabilities corresponding to the levels of the variable,
#' conditional on values of the primary random variable in the rvtable.
#' When the primary random variable is continuous, `values` must be a length-2 vector giving a valid range.
#' When discrete, `values` can be a range or a single discrete value.
#' If conditioning on a value or range of values restricts the conditional support for `id` to one where `id`
#' has probability zero everywhere, a warning will be thrown and the returned rvtable will have zero rows.
#'
#' @param x an rvtable.
#' @param values range of values of the continuous random variable in the rvtable.
#' @param id the categorical variable for which to compute the pmf given \code{values}.
#' @param sample.args optional arguments used when sampling.
#'
#' @return an rvtable.
#' @export
#'
#' @examples
#' library(dplyr)
#' x <- data.frame(
#'   id1=rep(LETTERS[1:5], each=4),
#'   id2=factor(c("low", "high")),
#'   id3=rep(1:2, each=2),
#'   Val=rep(1:10, each=20), Prob=rep(sqrt(1:10), each=20)) %>% rvtable
#' y1 <- inverse_pmf(x, c(5, 8), "id1", sample.args=list(n=5))
#' y1
#' x <- filter(x, id2=="low" & id3==1) %>% select(-id2, -id3) %>% rvtable
#' y2 <- inverse_pmf(x, c(5,8), "id1", sample.args=list(n=5))
#' y2
inverse_pmf <- function(x, values, id, sample.args){
  .rv_class_check(x)
  .inverse_pmf_stop(x, values, id)
  if(length(values) == 1) values <- rep(values, 2)
  Val <- valcol(x)
  Prob <- probcol(x)
  weights <- get_weights(x)
  weights <- weights[names(weights) != id]
  density.args <- get_density_args(x)
  if(missing(sample.args)) sample.args <- get_sample_args(x)
  if(tabletype(x)=="distribution"){
    x <- do.call(sample_rvtable, c(list(x=x), sample.args))
  } else if(rvtype(x)=="continuous"){
    x <- do.call(sample_rvtable, c(list(x=x, resample=TRUE), sample.args))
  }
  x <- dplyr::rename_(x, Val=lazyeval::interp(~v, v=Val))
  xid <- names(x)
  dots <- lapply(xid[!(xid %in% c("Val", Prob))], as.symbol)
  if(length(dots)==1){
    x <- dplyr::mutate_(x, .dots=list("dummy"=1))
    dots2 <- lapply("dummy", as.symbol)
  } else {
    dots2 <- dots[!(as.character(dots) %in% id)]
  }
  n.levels <- length(unique(x[[id]]))
  x <- x %>% dplyr::group_by_(.dots=dots2)
  uni <- unique(x[[id]])

  x <- x %>% dplyr::do(NEW=uni,
    numer=dplyr::group_by_(., .dots=dots) %>%
      dplyr::do(data.frame(
        numer=length(which(.$Val >= values[1] & .$Val <= values[2])) / (n.levels*nrow(.)))
        ) %>% dplyr::ungroup() %>% dplyr::select(numer),
    denom=dplyr::group_by_(., .dots=dots2) %>%
      dplyr::do(data.frame(
        denom=rep(length(which(.$Val >= values[1] & .$Val <= values[2])) / nrow(.), n.levels))
        ) %>% dplyr::ungroup() %>% dplyr::select(denom)) %>%
    dplyr::ungroup()

  if("dummy" %in% names(x)) x <- dplyr::select_(x, .dots=list("-dummy"))
  names(x)[names(x)=="NEW"] <- id
  if(is.null(Prob)) Prob <- "Prob"
  x <- tidyr::unnest(x) %>% dplyr::filter(denom != 0) %>%
    dplyr::group_by_(.dots=dots) %>% dplyr::summarise(Prob=numer/denom) %>%
    dplyr::rename_(.dots=stats::setNames("Prob", Prob)) %>% dplyr::ungroup()
  if(nrow(x) == 0){
    warning(paste0("'", id,
       "' has probability zero over the given value range of the primary random variable."))
    x[[Prob]] <- numeric()
  }
  .add_rvtable_class(x, id, Prob, TRUE, TRUE, weights, density.args, sample.args)
}

.inverse_pmf_stop <- function(x, values, id){
  discrete <- rvtype(x)=="discrete"
  values_err_disc <- "discrete `values` must be a single value or valid range."
  values_err_cont <- "continuous `values` must be a valid range."
  if(discrete){
    if(length(values) == 0 || length(values) > 2 ||
       (length(values) == 2 && values[1] > values[2])) stop(values_err_disc)
  } else {
    if(length(values) != 2 || values[1] >= values[2]) stop(values_err_cont)
  }
  if(missing(id)) stop("`id` missing.")
  if(length(id) != 1) stop("`id` must refer to a one ID variable.")
  Val <- valcol(x)
  if(id == Val)
    stop(paste(Val, "is the primary variable. `id` must refer to an ID variable."))
  if(!id %in% names(x)) stop(paste(id, "not found."))
}
