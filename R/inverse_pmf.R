#' Inverse Probability Mass Function
#'
#' Compute the probability mass function for a specified categorical variable conditional on an interval of the continuous random variable's values.
#'
#' This function computes the pmf of a categorical variable, providing probabilities corresponding to the levels of the variable, given a specific range of values of the continuous random variable in the rvtable.
#' At this time the rvtable type must be continuous.
#'
#' @param data an rvtable.
#' @param val.range range of values of the continuous random variable in the rvtable.
#' @param var.new the categorical variable for which to compute the pmf given \code{val.range}.
#' @param density.args optional arguments passed to \code{density}.
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
#' y2 <- inverse_pmf(x %>% filter(id2=="low" & id3==1) %>% select(-id2, -id3) %>% rvtable, c(5,8), "id1", sample.args=list(n=5))
#' y2
inverse_pmf <- function(x, val.range, var.new, sample.args=list()){
  require(tidyr)
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
  if(length(dots)==1) { x <- mutate(x, dummy=1); dots2 <- lapply("dummy", as.symbol) } else dots2 <- dots[!(as.character(dots) %in% var.new)]
  n.levels <- length(unique(x[[var.new]]))
  x <- x %>% group_by_(.dots=dots2)
  uni <- unique(x[[var.new]])
  x <- x %>% do(NEW=uni,
    numer=group_by_(., .dots=dots) %>% summarise(numer=length(which(Val >= val.range[1] & Val <= val.range[2]))/(n.levels*n())) %>% group_by %>% select(numer),
    denom=group_by_(., .dots=dots2) %>% summarise(denom=rep(length(which(Val >= val.range[1] & Val <= val.range[2]))/n(), n.levels)) %>% group_by %>% select(denom))
  if("dummy" %in% names(x)) x <- select(x, -dummy)
  id <- names(x)
  id[which(id=="NEW")] <- var.new
  setnames(x, id)
  if(nrow(x)==1){
    x <- bind_rows(x, x) %>% unnest
    x <- slice(x, 1:(nrow(x)/2))
  } else x <- unnest(x)
  x <- group_by_(x, .dots=dots) %>% summarise(Prob=numer/denom) %>% data.table %>% rvtable(Val=var.new, discrete=TRUE)
}
