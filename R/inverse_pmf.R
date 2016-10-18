#' Inverse Probability Mass Function
#'
#' Compute the probability mass function for a specified categorical variable conditional on an interval of the continuous random variable's values.
#'
#' This function computes the pmf of a categorical variable, providing probabilities corresponding to the levels of the variable, given a specific range of values of the continuous random variable in the rvtable.
#' At this time the rvtable type must be continuous.
#'
#' @param x an rvtable.
#' @param val.range range of values of the continuous random variable in the rvtable.
#' @param var.new the categorical variable for which to compute the pmf given \code{val.range}.
#' @param sample.args optional arguments used when sampling.
#'
#' @return an rvtable.
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
#' y1 <- inverse_pmf(x, c(5, 8), "id1", sample.args=list(n=5))
#' y1
#' x <- filter(x, id2=="low" & id3==1) %>% select(-id2, -id3) %>% rvtable
#' y2 <- inverse_pmf(x, c(5,8), "id1", sample.args=list(n=5))
#' y2
inverse_pmf <- function(x, val.range, var.new, sample.args=list()){
  .rv_class_check(x)
  stopifnot(length(val.range)==2 && val.range[1] < val.range[2])
  discrete <- attr(x, "rvtype")=="discrete"
  if(discrete) stop("inverse pmf not currently implemented for discrete rvtables.")
  if(attr(x, "tabletype")=="distribution"){
    x <- do.call(sample_rvtable, c(list(x=x), sample.args))
  }
  id <- names(x)
  stopifnot(var.new %in% id)
  dots <- lapply(id[!(id %in% c("Val", "Prob"))], as.symbol)
  if(length(dots)==1) { x <- dplyr::mutate_(x, .dots=list("dummy"=1)); dots2 <- lapply("dummy", as.symbol) } else dots2 <- dots[!(as.character(dots) %in% var.new)]
  n.levels <- length(unique(x[[var.new]]))
  x <- x %>% dplyr::group_by_(.dots=dots2)
  uni <- unique(x[[var.new]])
  x <- x %>% dplyr::do(NEW=uni,
    numer=dplyr::group_by_(., .dots=dots) %>%
      dplyr::do(data.table::data.table(numer=length(which(.$Val >= val.range[1] & .$Val <= val.range[2]))/(n.levels*nrow(.)))) %>%
      dplyr::group_by() %>% dplyr::select(numer),
    denom=dplyr::group_by_(., .dots=dots2) %>%
      dplyr::do(data.table::data.table(denom=rep(length(which(.$Val >= val.range[1] & .$Val <= val.range[2]))/nrow(.), n.levels))) %>%
      dplyr::group_by() %>% dplyr::select(denom))
  if("dummy" %in% names(x)) x <- dplyr::select_(x, .dots=list("-dummy"))
  id <- names(x)
  id[which(id=="NEW")] <- var.new
  data.table::setnames(x, id)
  if(nrow(x)==1){
    x <- dplyr::bind_rows(x, x)
    x <- tidyr::unnest(x)
    x <- dplyr::slice(x, 1:(nrow(x)/2))
  } else x <- tidyr::unnest(x)
  x <- dplyr::group_by_(x, .dots=dots) %>% dplyr::summarise(Prob=numer/denom) %>% data.table %>% rvtable(Val=var.new, discrete=TRUE)
}
