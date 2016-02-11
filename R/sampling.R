#' Sample A Random Variable
#'
#' This is an unexported helpful function for bootstrap resampling with replacement from a distribution of a random variable.
#'
#' This helpful function allows duplicate values in \code{x}, treating \code{x} as an original sample from which to resample.
#' This is the case when \code{x} contains duplicates and \code{y} is constant.
#' If all \code{x} values are unique, \code{y} values act as weights when resampling \code{x}.
#' In the case where \code{x} contains duplicates and \code{y} is not constant, the relative frequency of \code{x} values and their associated probabilities jointly influence their sampling probability.
#' This is currently satisfactory behavior for the function's limited use cases.
#'
#' @param x a vector of values. Duplicate values are permitted.
#' @param y a vector of probabilities.
#' @param n bootstrap sample size.
#' @param discrete \code{x} a discrete random variable.
#' @param interp linearly interpolate between observed values of \code{x} before sampling if \code{x} is continuous. Ignored if \code{discrete=TRUE}.
#' @param n.interp length of sequence of interpolated sampling points if. Ignored if \code{discrete=TRUE} or \code{interp=FALSE}.
#' @param decimals number of decimal places for rounding samples. Ignored if \code{discrete=TRUE}.
#'
#' @return a numeric vector of bootstrap samples.
#'
#' @examples
#' x <- rvtable(rnorm(1000))
#' sample_rvtable(x, n=10)
#' x <- rvtable(sample(1:100, 50), discrete=TRUE)
#' y <- sample_rvtable(x, n=10)
#' sample_rvtable(n=8, resample=T)
.sample_rvdist <- function(x, y, n=10000, discrete=FALSE, interp=TRUE, n.interp=100000, decimals=NULL){
  if(!is.null(decimals) && is.na(as.integer(decimals))) stop("`decimals` is invalid.")
  p <- list(x=x, y=y)
  if(!discrete & interp) p <- approx(p$x, p$y, n=n.interp)
  p <- sample(x=p$x, size=n, replace=TRUE, prob=p$y)
  if(is.null(decimals) | discrete) p else round(p, decimals)
}

#' Sample A Random Variable In An RV Table
#'
#' Bootstrap resampling with replacement from a distribution of a random variable stored in an rvtable.
#'
#' Resample an rvtable yielding a new rvtable.
#' This is used to convert from a distribution-based rvtable, which has Val and Prob columns representing a distribution to one with only a Val column representing samples from a distribution.
#' This is often used preceeding plotting data in an rvtable so that samples may be passed to plot code rather than a representation of a distribution using values and associated probabilities.
#' This function can also take a sample-based rvtable, in which case it will resample it if \code{resample=TRUE}.
#' If is sample-based rvtable is passed and the random variable is continuous, the generation of a new continuous density prior to resampling can be controlled via \code{density.args}.
#' All rvtable objects are either distribution-based or sample-based.
#'
#' @param x an \code{rvtable} object.
#' @param resample if \code{x} is a sample-based rvtable, return \code{x} unchanged if \code{resample=FALSE}. Otherwise resample from the samples in the Val column. Ignored if rvtable is distribution-based.
#' @param n bootstrap sample size.
#' @param interp linearly interpolate between observed values of Val before sampling if Val is continuous. Ignored for discrete random variables.
#' @param n.interp length of sequence of interpolated sampling points if. Ignored for discrete random variables or \code{interp=FALSE}.
#' @param decimals number of decimal places for rounding samples. Ignored for discrete random variables.
#' @param density.args optional arguments passed to \code{density}.
#'
#' @return an \code{rvtable} object where the Val column represents samples and the Prob column is dropped.
#' @export
#'
#' @examples
#' x <- rvtable(rnorm(1000))
#' sample_rvtable(x, n=10)
#' x <- rvtable(sample(1:100, 50), discrete=TRUE)
#' y <- sample_rvtable(x, n=10)
#' sample_rvtable(y, n=8, resample=TRUE)
sample_rvtable <- function(x, resample=FALSE, n=10000, interp=TRUE, n.interp=100000, decimals=NULL, density.args=list()){
  .rv_class_check(x)
  rv <- attr(x, "rvtype")
  discrete <- rv=="discrete"
  tbl <- attr(x, "tabletype")
  id <- names(x)
  grp <- dplyr::groups(x)
  grp2 <- lapply(setdiff(id, c("Val", "Prob")), as.symbol)
  if(tbl=="sample" & !resample){
    message("rvtable already contains samples and resample=FALSE. Returning original rvtable.")
    return(x)
  }
  x <- dplyr::group_by_(x, .dots=grp2)
  if(tbl=="sample"){
    if(discrete) x <- dplyr::mutate(x, Prob=1)
    if(!discrete){
      x <- dplyr::summarise_(x,
                     Val=~do.call(density, c(list(x=Val), density.args))$x,
                     Prob=~do.call(density, c(list(x=Val), density.args))$y) %>%
        dplyr::group_by_(.dots=grp2)
    }
  }
  x <- dplyr::summarise_(x, Val=~.sample_rvdist(Val, Prob, n, discrete, interp, n.interp, decimals)) %>% dplyr::group_by_(.dots=grp)
  class(x) <- unique(c("rvtable", class(x)))
  attr(x, "rvtype") <- rv
  attr(x, "tabletype") <- "sample"
  x
}
